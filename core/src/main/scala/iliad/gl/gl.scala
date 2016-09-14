package iliad
package gl

import iliad.algebra._
import iliad.algebra.syntax.vector._

import cats._
import cats.data.{State => CatsState, ReaderT, StateT, Xor, XorT}
import cats.free._
import cats.implicits._

import freek._

import monocle._
import monocle.macros._

import scodec.bits._

object GL {

  case class State(cache: Cache.State, current: Current.State)
  type GL[A] =
    (Load :|: Cache :|: Draw :|: Current :|: OpenGL :|: FXNil)#Cop[A]
  type DSL[A] = Free[GL, A]
  type PRG[F[_], A] = ReaderT[StateT[F, State, ?], GLES30.type, A]

  def empty(screenDimensions: Vec2i): State = State(Cache.State.empty, Current.State.empty(screenDimensions))

  val _cache: Lens[State, Cache.State] = GenLens[State](_.cache)
  val _current: Lens[State, Current.State] = GenLens[State](_.current)

  private def liftOpenGL[F[_]: Monad]: OpenGL.Effect[F, ?] ~> PRG[F, ?] =
    new (OpenGL.Effect[F, ?] ~> PRG[F, ?]) {
      def apply[A](eff: ReaderT[F, GLES30.type, A]): PRG[F, A] =
        eff.mapF(_.liftT[StateT[?[_], State, ?]])
    }

  def liftS[F[_]: Monad, A](s: CatsState[State, A]): PRG[F, A] =
    ReaderT(_ => s.transformF(sa => Applicative[F].pure(sa.value)))

  private def liftCache[F[_]: Monad]: Cache.Effect ~> PRG[F, ?] =
    new (Cache.Effect ~> PRG[F, ?]) {
      def apply[A](eff: CatsState[Cache.State, A]): PRG[F, A] =
        liftS(eff.applyLens[State](_cache))
    }

  private def liftCurrent[F[_]: Monad]: Current.Effect ~> PRG[F, ?] =
    new (Current.Effect ~> PRG[F, ?]) {
      def apply[A](eff: CatsState[Current.State, A]): PRG[F, A] =
        liftS(eff.applyLens[State](_current))
    }

  def runner[F[_]: Monad](
      f: OpenGL.Interpreter[OpenGL.Effect[F, ?]]): Interpreter[GL, PRG[F, ?]] =
    Load.parse(f).andThen(liftOpenGL) :&:
      CacheParser.andThen(liftCache[F]) :&:
        Draw.parse(f).andThen(liftOpenGL) :&:
          CurrentParser.andThen(liftCurrent[F]) :&:
            f.andThen(liftOpenGL)

  private def getOrElse[A](f: DSL[Option[A]])(g: => DSL[A]): DSL[A] =
    f.flatMap(_.map(Free.pure[GL, A]).getOrElse(g))

  private def load(s: VertexShader.Source): DSL[VertexShader.Compiled] =
    getOrElse(Cache.get(s).expand[GL])(for {
      v <- Load(s).expand[GL]
      _ <- Cache.put(v).expand[GL]
    } yield v)

  private def load(s: FragmentShader.Source): DSL[FragmentShader.Compiled] =
    getOrElse(Cache.get(s).expand[GL])(for {
      v <- Load(s).expand[GL]
      _ <- Cache.put(v).expand[GL]
    } yield v)

  private def load(s: Sampler.Constructor): DSL[Sampler.Loaded] = 
    getOrElse(Cache.get(s).expand[GL])(for {
      sl <- Load(s).expand[GL]
      _ <- Cache.put(sl).expand[GL]
    } yield sl)

  def load(p: Program.Unlinked): DSL[Program.Linked] =
    getOrElse(Cache.get(p).expand[GL])(for {
      v <- load(p.vertex)
      f <- load(p.fragment)
      _ <- p.samplers.values.toList.traverse(load)
      pl <- Load(v, f).expand[GL]
      _ <- Cache.put(pl).expand[GL]
      _ <- Current.set(pl).expand[GL]
    } yield pl)

  private def add(u: VertexBuffer.Update): DSL[Unit] =
    for {
      _ <- Cache.put(u).expand[GL]
      _ <- Current.set(u.buffer).expand[GL]
    } yield ()

  private def loadVertices(r: VertexData.Ref,
                           d: ByteVector,
                           pageSize: Int): DSL[Unit] =
    Cache.get(r.buffer).expand[GL] flatMap {
      case Some(prev) =>
        if (VertexBuffer.fits(prev, d.size.toInt))
          for {
            next <- Load.insert(r, d, pageSize, prev).expand[GL]
            _ <- add(next)
          } yield ()
        else
          for {
            next <- Load.copy(r, d, pageSize, prev).expand[GL]
            _ <- add(next)
          } yield ()
      case None =>
        for {
          b <- Load.create(r, d, pageSize, r.buffer).expand[GL]
          _ <- add(b)
        } yield ()
    }

  def load(r: VertexData.Ref,
           d: ByteVector,
           pageSize: Int): DSL[VertexDataAlreadyLoaded Xor Unit] =
    Cache.get(r).expand[GL] flatMap {
      case Some(_) => Free.pure(VertexDataAlreadyLoaded(r).left)
      case None => loadVertices(r, d, pageSize).map(_.right)
    }

  private def add(u: ElementBuffer.Update): DSL[Unit] =
    for {
      _ <- Cache.put(u).expand[GL]
      _ <- Current.set(u.buffer).expand[GL]
    } yield ()

  private def loadElements(r: ElementData.Ref,
                           d: ByteVector,
                           pageSize: Int): DSL[Unit] =
    Cache.get(r.buffer).expand[GL] flatMap {
      case Some(prev) =>
        if (ElementBuffer.fits(prev, d.size.toInt))
          for {
            next <- Load.insert(r, d, pageSize, prev).expand[GL]
            _ <- add(next)
          } yield ()
        else
          for {
            next <- Load.copy(r, d, pageSize, prev).expand[GL]
            _ <- add(next)
          } yield ()
      case None =>
        for {
          b <- Load.create(r, d, pageSize, r.buffer).expand[GL]
          _ <- add(b)
        } yield ()
    }

  def load(r: ElementData.Ref,
           d: ByteVector,
           pageSize: Int): DSL[ElementDataAlreadyLoaded Xor Unit] =
    Cache.get(r).expand[GL] flatMap {
      case Some(_) => Free.pure(ElementDataAlreadyLoaded(r).left)
      case None => loadElements(r, d, pageSize).map(_.right)
    }

  def load(t: Texture.Constructor,
           d: Texture.Data): DSL[Texture.Loaded] =
    getOrElse(Cache.get(t).expand[GL])(for {
      tl <- Load(t, d).expand[GL]
      _ <- Cache.put(tl).expand[GL]
    } yield tl)

  def load(r: Renderbuffer.Constructor): DSL[Renderbuffer.Loaded] =
    getOrElse(Cache.get(r).expand[GL])(for {
      rl <- Load(r).expand[GL]
      _ <- Cache.put(rl).expand[GL]
    } yield rl)

  private def attachment(a: Framebuffer.AttachmentConstructor)
    : DSL[GLError Xor Framebuffer.AttachmentLoaded] = a match {
    case t: Texture.Constructor =>
      Cache.ensure(Cache.get(t), TextureNotLoadedError(t)).expand[GL].widen
    case r: Renderbuffer.Constructor =>
      Cache
        .ensure(Cache.get(r), RenderbufferNotLoadedError(r))
        .expand[GL]
        .widen
  }

  private def attachments(f: Framebuffer.Constructor)
    : DSL[GLError Xor List[(FramebufferAttachment,
                            Framebuffer.AttachmentLoaded)]] =
    f.attachments.traverse {
      case (c, a) => XorT(attachment(a)).map(c -> _).value
    }.map(_.sequence)

  def load(f: Framebuffer.Constructor): DSL[GLError Xor Unit] =
    Cache.get(f).expand[GL] flatMap {
      case Some(fl) => Free.pure(().right)
      case None =>
        (for {
          as <- XorT(attachments(f))
          fl <- xort[Framebuffer.Loaded, GLError](Load(f, as).expand[GL])
          _ <- xort[Unit, GLError](Cache.put(fl).expand[GL])
        } yield ()).value
    }

  private def ensure(f: Current.DSL[Boolean])(g: => DSL[Unit]): DSL[Unit] =
    f.expand[GL] flatMap (b => if (b) Free.pure(()) else g)

  private def set(f: Framebuffer.Loaded): DSL[Unit] =
    ensure(Current.contains(f))(Draw.bind(f).expand[GL] >>
          Current.set(f).expand[GL])

  private def setCapability(c: Capability, value: Boolean): DSL[Unit] =
    if (value)
      OpenGL.enable(c).expand[GL] >> Current.enable(c).expand[GL]
    else
      OpenGL.disable(c).expand[GL] >> Current.disable(c).expand[GL]

  private def set(c: Capability, value: Boolean): DSL[Unit] =
    ensure(Current.get(c).map(_.contains(value)))(setCapability(c, value))

  private def set(cs: Map[Capability, Boolean]): DSL[Unit] =
    cs.toList.traverseUnit {
      case (c, value) => set(c, value)
    }

  private def set(m: ColorMask): DSL[Unit] =
    ensure(Current.contains(m))(Draw.bind(m).expand[GL] >>
          Current.set(m).expand[GL])

  private def setClearColour(c: Vec4f): DSL[Unit] =
    ensure(Current.containsClearColour(c))(Draw.bindClearColour(c).expand[GL] >>
         Current.setClearColour(c).expand[GL])

  private def set(blend: Option[Blend]): DSL[Unit] = blend match {
    case Some(Blend(m, f)) => for {
      _ <- ensure(Current.contains(m))(Draw.bind(m).expand[GL] >> Current.set(m).expand[GL])
      _ <- ensure(Current.contains(f))(Draw.bind(f).expand[GL] >> Current.set(f).expand[GL])
    } yield ()
    case None => Free.pure(())
  }

  private def set(viewport: Rect[Int]): DSL[Unit] = 
    ensure(Current.contains(viewport))(Draw.bindViewport(viewport).expand[GL] >>
      Current.set(viewport).expand[GL])

  private def flip(
      t: Texture.Constructor): DSL[TextureNotLoadedError Xor Unit] =
    (for {
      tl <- ensure(Cache.get(t), TextureNotLoadedError(t))
      _ <- tl match {
            case d: Texture.DoubleLoaded =>
              xort[Unit, TextureNotLoadedError](Cache.put(d.flip).expand[GL])
            case _ => XorT.pure[DSL, TextureNotLoadedError, Unit](())
          }
    } yield ()).value

  private def flipDouble(
      f: Framebuffer.DoubleLoaded): DSL[TextureNotLoadedError Xor Unit] =
    (for {
      _ <- xort(Cache.put(f.flip).expand[GL])
      _ <- XorT(f.constructor.textures.traverse(flip).map(_.sequenceUnit))
    } yield ()).value

  private def flip(
      f: Framebuffer.Loaded): DSL[TextureNotLoadedError Xor Unit] =
    f match {
      case s: Framebuffer.SingleLoaded => Free.pure(().right)
      case d: Framebuffer.DoubleLoaded => flipDouble(d)
    }

  private def set(p: Program.Linked): DSL[Unit] = ensure(Current.contains(p))(
      Draw.use(p).expand[GL] >> Current.set(p).expand[GL]
  )

  private def set(u: Program.TextureUniform): DSL[GLError Xor Unit] =
    (for {
      t <- XorT(
              Cache
                .ensure(Cache.get(u.texture), TextureNotLoadedError(u.texture))
                .expand[GL]).leftWiden[GLError]
      s <- XorT(
              Cache
                .ensure(Cache.get(u.sampler), SamplerNotLoadedError(u.sampler))
                .expand[GL]).leftWiden[GLError]
      _ <- xort[Unit, GLError](Draw.bind(u.unit, u.location, t, s).expand[GL])
    } yield ()).value

  private def set(
      p: Program.Linked,
      ts: Map[String, Texture.Constructor]): DSL[GLError Xor Unit] = {
    p.textureUniforms(ts)
      .traverse(_.traverse(set).map(_.sequenceUnit))
      .map(_.flatMap(identity))
  }

  private def set(u: Program.UniformValue): DSL[Unit] =
    Draw.bind(u.uniform, u.value).expand[GL]

  private def set(p: Program.Linked,
                  us: List[Uniform.Value]): DSL[UnsetUniformError Xor Unit] =
    p.uniforms(us).traverse(_.traverseUnit(set))

  private def set(vb: VertexBuffer.Loaded): DSL[Unit] =
    ensure(Current.contains(vb))(
        Draw.bind(vb).expand[GL] >> Current.set(vb).expand[GL]
    )

  private def set(eb: ElementBuffer.Loaded): DSL[Unit] =
    ensure(Current.contains(eb))(
        Draw.bind(eb).expand[GL] >> Current.set(eb).expand[GL]
    )

  private def ensure[A, E](c: Cache.DSL[Option[A]], e: E): XorT[DSL, E, A] =
    XorT(Cache.ensure(c, e).expand[GL])

  private def xort[A, E](dsl: DSL[A]): XorT[DSL, E, A] = XorT.right(dsl)

  def draw(draw: DrawOp): DSL[GLError Xor Unit] =
    (for {
      fl <- ensure(
               Cache.get(draw.framebuffer),
               FramebufferNotLoadedError(draw.framebuffer)).leftWiden[GLError]
      _ <- xort(set(fl))
      _ <- xort(set(draw.capabilities))
      _ <- xort(set(draw.colorMask))
      _ <- xort(set(draw.blend))
      _ <- xort(set(draw.viewport))
      p <- ensure(Cache.get(draw.program), ProgramNotLoadedError(draw.program))
            .leftWiden[GLError]
      _ <- xort(set(p))
      _ <- XorT(set(p, draw.textureUniforms))
      _ <- XorT(set(p, draw.uniforms)).leftWiden[GLError]
      vb <- ensure(Cache.get(draw.vertexBuffer),
                   VertexBufferNotLoadedError(draw.vertexBuffer))
             .leftWiden[GLError]
      _ <- xort(set(vb))
      vd <- ensure(
               Cache.get(draw.vertexData),
               VertexDataNotLoadedError(draw.vertexData)).leftWiden[GLError]
      eb <- ensure(Cache.get(draw.elementBuffer),
                   ElementBufferNotLoadedError(draw.elementBuffer))
             .leftWiden[GLError]
      _ <- xort(set(eb))
      ed <- ensure(
               Cache.get(draw.elementData),
               ElementDataNotLoadedError(draw.elementData)).leftWiden[GLError]
      as <- XorT.fromXor[DSL](p.loaded(Attribute.offsets(vd.offset(draw.vertexModel), draw.attributes)))
        .leftWiden[GLError]
      _ <- xort(Draw.enable(as, Attribute.stride(draw.attributes)).expand[GL])
      _ <- xort(Draw(ed.offset(draw.elementModel)).expand[GL])
      _ <- XorT(flip(fl)).leftWiden[GLError]
    } yield ()).value

  def clear(c: ClearOp): DSL[GLError Xor Unit] =
    (for {
      fl <- ensure(Cache.get(c.framebuffer),
                   FramebufferNotLoadedError(c.framebuffer)).leftWiden[GLError]
      _ <- xort[Unit, GLError](set(fl))
      _ <- xort(setClearColour(c.colour))
      _ <- xort(set(c.viewport))
      _ <- xort[Unit, GLError](Draw.clear(c.bitMask).expand[GL])
} yield ()).value
}
