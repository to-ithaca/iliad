package iliad
package gl

import iliad.kernel.platform.GLES30Library
import iliad.syntax.vectord._

import cats._
import cats.data.{State => CatsState, ReaderT, StateT, Xor, XorT}
import cats.free._
import cats.implicits._

import freek._

import monocle._
import monocle.macros._

import MonocleExtra._
import CatsExtra._

object GL {
  case class State(cache: Cache.State, current: Current.State)

  type DSL[A] = Free[GLProgram.Cop, A]

  type PRG[F[_], A] = ReaderT[StateT[F, State, ?], GLES30Library, A]

  def empty: State = State(Cache.State.empty, Current.State.empty)

  val _cache: Lens[State, Cache.State] = GenLens[State](_.cache)
  val _current: Lens[State, Current.State] = GenLens[State](_.current)

  private def liftOpenGL[F[_]: Monad]: OpenGL.Effect[F, ?] ~> PRG[F, ?] =
    new (OpenGL.Effect[F, ?] ~> PRG[F, ?]) {
      def apply[A](eff: ReaderT[F, GLES30Library, A]): PRG[F, A] =
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

  def runner[F[_]: MonadRec](f: OpenGL.Interpreter[OpenGL.Effect[F, ?]])
    : Interpreter[GLProgram.Cop, PRG[F, ?]] =
    Load.parse(f).andThen(liftOpenGL) :&:
      CacheParser.andThen(liftCache[F]) :&:
        Draw.parse(f).andThen(liftOpenGL) :&:
          CurrentParser.andThen(liftCurrent[F]) :&:
            f.andThen(liftOpenGL)

  private def getOrElse[A](f: DSL[Option[A]])(g: => DSL[A]): DSL[A] =
    f.flatMap(_.map(Free.pure[GLProgram.Cop, A]).getOrElse(g))

  private def load(s: VertexShader.Source): DSL[VertexShader.Compiled] =
    getOrElse(Cache.get(s).expand[AST])(for {
      v <- Load(s).expand[AST]
      _ <- Cache.put(v).expand[AST]
    } yield v)

  private def load(s: FragmentShader.Source): DSL[FragmentShader.Compiled] =
    getOrElse(Cache.get(s).expand[AST])(for {
      v <- Load(s).expand[AST]
      _ <- Cache.put(v).expand[AST]
    } yield v)

  def load(p: Program.Unlinked): DSL[Program.Linked] =
    getOrElse(Cache.get(p).expand[AST])(for {
      v <- load(p.vertex)
      f <- load(p.fragment)
      pl <- Load(v, f).expand[AST]
      _ <- Cache.put(pl).expand[AST]
      _ <- Current.set(pl).expand[AST]
    } yield pl)

  private def add(u: VertexBuffer.Update): DSL[Unit] =
    for {
      _ <- Cache.put(u).expand[AST]
      _ <- Current.set(u.buffer).expand[AST]
    } yield ()

  private def loadVertices(r: VertexData.Ref,
                           d: VertexData.Data,
                           pageSize: Int): DSL[Unit] =
    Cache.get(r.buffer).expand[AST] flatMap {
      case Some(prev) =>
        if (VertexBuffer.fits(prev, d.size))
          for {
            next <- Load.insert(r, d, pageSize, prev).expand[AST]
            _ <- add(next)
          } yield ()
        else
          for {
            next <- Load.copy(r, d, pageSize, prev).expand[AST]
            _ <- add(next)
          } yield ()
      case None =>
        for {
          b <- Load.create(r, d, pageSize, r.buffer).expand[AST]
          _ <- add(b)
        } yield ()
    }

  def load(r: VertexData.Ref,
           d: VertexData.Data,
           pageSize: Int): DSL[VertexDataAlreadyLoaded Xor Unit] =
    Cache.get(r).expand[AST] flatMap {
      case Some(_) => Free.pure(VertexDataAlreadyLoaded(r).left)
      case None => loadVertices(r, d, pageSize).map(_.right)
    }

  private def add(u: ElementBuffer.Update): DSL[Unit] =
    for {
      _ <- Cache.put(u).expand[AST]
      _ <- Current.set(u.buffer).expand[AST]
    } yield ()

  private def loadElements(r: ElementData.Ref,
                           d: ElementData.Data,
                           pageSize: Int): DSL[Unit] =
    Cache.get(r.buffer).expand[AST] flatMap {
      case Some(prev) =>
        if (ElementBuffer.fits(prev, d.size))
          for {
            next <- Load.insert(r, d, pageSize, prev).expand[AST]
            _ <- add(next)
          } yield ()
        else
          for {
            next <- Load.copy(r, d, pageSize, prev).expand[AST]
            _ <- add(next)
          } yield ()
      case None =>
        for {
          b <- Load.create(r, d, pageSize, r.buffer).expand[AST]
          _ <- add(b)
        } yield ()
    }

  def load(r: ElementData.Ref,
           d: ElementData.Data,
           pageSize: Int): DSL[ElementDataAlreadyLoaded Xor Unit] =
    Cache.get(r).expand[AST] flatMap {
      case Some(_) => Free.pure(ElementDataAlreadyLoaded(r).left)
      case None => loadElements(r, d, pageSize).map(_.right)
    }

  def load(t: Texture.Constructor, d: Texture.Data): DSL[Texture.Loaded] =
    getOrElse(Cache.get(t).expand[AST])(for {
      tl <- Load(t, d).expand[AST]
      _ <- Cache.put(tl).expand[AST]
    } yield tl)

  def load(r: Renderbuffer.Constructor): DSL[Renderbuffer.Loaded] =
    getOrElse(Cache.get(r).expand[AST])(for {
      rl <- Load(r).expand[AST]
      _ <- Cache.put(rl).expand[AST]
    } yield rl)

  private def attachment(a: Framebuffer.AttachmentConstructor)
    : DSL[GLError Xor Framebuffer.AttachmentLoaded] = a match {
    case t: Texture.Constructor =>
      Cache.ensure(Cache.get(t), TextureNotLoadedError(t)).expand[AST].widen
    case r: Renderbuffer.Constructor =>
      Cache
        .ensure(Cache.get(r), RenderbufferNotLoadedError(r))
        .expand[AST]
        .widen
  }

  private def attachments(f: Framebuffer.Constructor)
    : DSL[GLError Xor List[(FramebufferAttachment,
                            Framebuffer.AttachmentLoaded)]] =
    f.attachments.traverse {
      case (c, a) => XorT(attachment(a)).map(c -> _).value
    }.map(_.sequence)

  def load(f: Framebuffer.Constructor): DSL[GLError Xor Unit] =
    Cache.get(f).expand[AST] flatMap {
      case Some(fl) => Free.pure(().right)
      case None =>
        (for {
          as <- XorT(attachments(f))
          fl <- xort[Framebuffer.Loaded, GLError](Load(f, as).expand[AST])
          _ <- xort[Unit, GLError](Cache.put(fl).expand[AST])
        } yield ()).value
    }

  private def ensure(f: Current.DSL[Boolean])(g: => DSL[Unit]): DSL[Unit] =
    f.expand[AST] flatMap (b => if (b) Free.pure(()) else g)

  private def set(f: Framebuffer.Loaded): DSL[Unit] =
    ensure(Current.contains(f))(Draw.bind(f).expand[AST] >>
          Current.set(f).expand[AST])

  private def setCapability(c: Capability, value: Boolean): DSL[Unit] =
    if (value)
      OpenGL.enable(c).expand[AST] >> Current.enable(c).expand[AST]
    else
      OpenGL.disable(c).expand[AST] >> Current.disable(c).expand[AST]

  private def set(c: Capability, value: Boolean): DSL[Unit] =
    ensure(Current.get(c).map(_.contains(value)))(setCapability(c, value))

  private def set(cs: Map[Capability, Boolean]): DSL[Unit] =
    cs.toList.traverseUnit {
      case (c, value) => set(c, value)
    }

  private def set(m: ColorMask): DSL[Unit] =
    ensure(Current.contains(m))(Draw.bind(m).expand[AST] >>
          Current.set(m).expand[AST])

  private def setClearColour(c: Vec4f): DSL[Unit] =
    ensure(Current.containsClearColour(c))(Draw.bindClearColour(c).expand[AST] >>
         Current.setClearColour(c).expand[AST])

  private def flip(
      t: Texture.Constructor): DSL[TextureNotLoadedError Xor Unit] =
    (for {
      tl <- ensure(Cache.get(t), TextureNotLoadedError(t))
      _ <- tl match {
            case d: Texture.DoubleLoaded =>
              xort[Unit, TextureNotLoadedError](Cache.put(d.flip).expand[AST])
            case _ => XorT.pure[DSL, TextureNotLoadedError, Unit](())
          }
    } yield ()).value

  private def flipDouble(
      f: Framebuffer.DoubleLoaded): DSL[TextureNotLoadedError Xor Unit] =
    (for {
      _ <- xort(Cache.put(f.flip).expand[AST])
      _ <- XorT(f.constructor.textures.traverse(flip).map(_.sequenceUnit))
    } yield ()).value

  private def flip(
      f: Framebuffer.Loaded): DSL[TextureNotLoadedError Xor Unit] =
    f match {
      case s: Framebuffer.SingleLoaded => Free.pure(().right)
      case d: Framebuffer.DoubleLoaded => flipDouble(d)
    }

  private def set(p: Program.Linked): DSL[Unit] = ensure(Current.contains(p))(
      Draw.use(p).expand[AST] >> Current.set(p).expand[AST]
  )

  private def set(u: Program.TextureUniform): DSL[GLError Xor Unit] =
    (for {
      t <- XorT(
              Cache
                .ensure(Cache.get(u.texture), TextureNotLoadedError(u.texture))
                .expand[AST]).leftWiden[GLError]
      s <- XorT(
              Cache
                .ensure(Cache.get(u.sampler), SamplerNotLoadedError(u.sampler))
                .expand[AST]).leftWiden[GLError]
      _ <- xort[Unit, GLError](Draw.bind(u.unit, u.location, t, s).expand[AST])
    } yield ()).value

  private def set(
      p: Program.Linked,
      ts: Map[String, Texture.Constructor]): DSL[GLError Xor Unit] = {
    p.textureUniforms(ts)
      .traverse(_.traverse(set).map(_.sequenceUnit))
      .map(_.flatMap(identity))
  }

  private def set(u: Program.UniformValue): DSL[Unit] =
    Draw.bind(u.uniform, u.value).expand[AST]

  private def set(p: Program.Linked,
                  us: List[Uniform.Value]): DSL[UnsetUniformError Xor Unit] =
    p.uniforms(us).traverse(_.traverseUnit(set))

  private def set(vb: VertexBuffer.Loaded): DSL[Unit] =
    ensure(Current.contains(vb))(
        Draw.bind(vb).expand[AST] >> Current.set(vb).expand[AST]
    )

  private def set(eb: ElementBuffer.Loaded): DSL[Unit] =
    ensure(Current.contains(eb))(
        Draw.bind(eb).expand[AST] >> Current.set(eb).expand[AST]
    )

  private def ensure[A, E](c: Cache.DSL[Option[A]], e: E): XorT[DSL, E, A] =
    XorT(Cache.ensure(c, e).expand[AST])

  private def xort[A, E](dsl: DSL[A]): XorT[DSL, E, A] = XorT.right(dsl)

  def draw(draw: DrawOp): DSL[GLError Xor Unit] =
    (for {
      fl <- ensure(
               Cache.get(draw.framebuffer),
               FramebufferNotLoadedError(draw.framebuffer)).leftWiden[GLError]
      _ <- xort(set(fl))
      _ <- xort(set(draw.capabilities))
      _ <- xort(set(draw.colorMask))
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
      as <- XorT.fromXor[DSL](p.loaded(draw.attributes)).leftWiden[GLError]
      _ <- xort(Draw.enable(as, vd.offset(draw.vertexModel)).expand[AST])
      _ <- xort(Draw(ed.offset(draw.elementModel)).expand[AST])
      _ <- XorT(flip(fl)).leftWiden[GLError]
    } yield ()).value

  def clear(c: ClearOp): DSL[GLError Xor Unit] =
    (for {
      fl <- ensure(Cache.get(c.framebuffer),
                   FramebufferNotLoadedError(c.framebuffer)).leftWiden[GLError]
      _ <- xort[Unit, GLError](set(fl))
      _ <- xort(setClearColour(c.colour))
      _ <- xort[Unit, GLError](Draw.clear(c.bitMask).expand[AST])
    } yield ()).value
}
