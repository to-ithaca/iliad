package iliad
package gl

import iliad.kernel.platform.GLES30Library

import cats._
import cats.data.{State => CatsState, ReaderT, StateT, Xor, XorT}
import cats.free._
import cats.implicits._

import freek._

import monocle._
import monocle.macros._

import FreekExtra._
import MonocleExtra._
import CatsExtra._

object GL {

  case class State(cache: Cache.State, current: Current.State)
  type GL[A] =
    (Load :|: Cache :|: Draw :|: Current :|: OpenGL :|: FXNil)#Cop[A]
  type DSL[A] = Free[GL, A]
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
    getOrElse(Cache.get(s).freekF[GL])(for {
      v <- Load(s).freekF[GL]
      _ <- Cache.put(v).freekF[GL]
    } yield v)

  private def load(s: FragmentShader.Source): DSL[FragmentShader.Compiled] =
    getOrElse(Cache.get(s).freekF[GL])(for {
      v <- Load(s).freekF[GL]
      _ <- Cache.put(v).freekF[GL]
    } yield v)

  def load(p: Program.Unlinked): DSL[Program.Linked] =
    getOrElse(Cache.get(p).freekF[GL])(for {
      v <- load(p.vertex)
      f <- load(p.fragment)
      pl <- Load(v, f).freekF[GL]
      _ <- Cache.put(pl).freekF[GL]
      _ <- Current.set(pl).freekF[GL]
    } yield pl)

  private def add(u: VertexBuffer.Update): DSL[Unit] =
    for {
      _ <- Cache.put(u).freekF[GL]
      _ <- Current.set(u.buffer).freekF[GL]
    } yield ()

  def load(r: VertexData.Ref, d: VertexData.Data, pageSize: Int): DSL[Unit] =
    Cache.get(r.buffer).freekF[GL] flatMap {
      case Some(prev) =>
        if (VertexBuffer.fits(prev, d.size))
          for {
            next <- Load.insert(r, d, pageSize, prev).freekF[GL]
            _ <- add(next)
          } yield ()
        else
          for {
            next <- Load.copy(r, d, pageSize, prev).freekF[GL]
            _ <- add(next)
          } yield ()
      case None =>
        for {
          b <- Load.create(r, d, pageSize, r.buffer).freekF[GL]
          _ <- add(b)
        } yield ()
    }

  private def add(u: ElementBuffer.Update): DSL[Unit] =
    for {
      _ <- Cache.put(u).freekF[GL]
      _ <- Current.set(u.buffer).freekF[GL]
    } yield ()

  def load(r: ElementData.Ref, d: ElementData.Data, pageSize: Int): DSL[Unit] =
    Cache.get(r.buffer).freekF[GL] flatMap {
      case Some(prev) =>
        if (ElementBuffer.fits(prev, d.size))
          for {
            next <- Load.insert(r, d, pageSize, prev).freekF[GL]
            _ <- add(next)
          } yield ()
        else
          for {
            next <- Load.copy(r, d, pageSize, prev).freekF[GL]
            _ <- add(next)
          } yield ()
      case None =>
        for {
          b <- Load.create(r, d, pageSize, r.buffer).freekF[GL]
          _ <- add(b)
        } yield ()
    }

  def load(t: Texture.Constructor,
           d: Option[Texture.Data]): DSL[Texture.Loaded] =
    getOrElse(Cache.get(t).freekF[GL])(for {
      tl <- Load(t, d).freekF[GL]
      _ <- Cache.put(tl).freekF[GL]
    } yield tl)

  def load(r: Renderbuffer.Constructor): DSL[Renderbuffer.Loaded] =
    getOrElse(Cache.get(r).freekF[GL])(for {
      rl <- Load(r).freekF[GL]
      _ <- Cache.put(rl).freekF[GL]
    } yield rl)

  private def attachment(a: Framebuffer.AttachmentConstructor)
    : DSL[String Xor Framebuffer.AttachmentLoaded] = a match {
    case t: Texture.Constructor =>
      Cache.ensure(Cache.get(t), s"Texture missing $t").freekF[GL].widen
    case r: Renderbuffer.Constructor =>
      Cache.ensure(Cache.get(r), s"Renderbuffer missing $r").freekF[GL].widen
  }

  private def attachments(f: Framebuffer.Constructor): DSL[
      String Xor List[(FramebufferAttachment, Framebuffer.AttachmentLoaded)]] =
    f.attachments.traverse {
      case (c, a) => XorT(attachment(a)).map(c -> _).value
    }.map(_.sequence)

  def load(f: Framebuffer.Constructor): DSL[String Xor Unit] =
    Cache.get(f).freekF[GL] flatMap {
      case Some(fl) => Free.pure(().right)
      case None =>
        (for {
          as <- XorT(attachments(f))
          fl <- xort(Load(f, as).freekF[GL])
          _ <- xort(Cache.put(fl).freekF[GL])
        } yield ()).value
    }

  private def ensure(f: Current.DSL[Boolean])(g: => DSL[Unit]): DSL[Unit] =
    f.freekF[GL] flatMap (b => if (b) Free.pure(()) else g)

  private def set(f: Framebuffer.Loaded): DSL[Unit] =
    ensure(Current.contains(f))(Draw.bind(f).freekF[GL] >>
          Current.set(f).freekF[GL])

  private def flip(t: Texture.Constructor): DSL[String Xor Unit] =
    (for {
      tl <- ensure(Cache.get(t), "Texture not loaded.")
      _ <- tl match {
            case d: Texture.DoubleLoaded =>
              xort(Cache.put(d.flip).freekF[GL])
            case _ => XorT.pure[DSL, String, Unit](())
          }
    } yield ()).value

  private def flipDouble(f: Framebuffer.DoubleLoaded): DSL[String Xor Unit] =
    (for {
      _ <- xort(Cache.put(f.flip).freekF[GL])
      _ <- XorT(f.constructor.textures.traverse(flip).map(_.sequenceUnit))
    } yield ()).value

  private def flip(f: Framebuffer.Loaded): DSL[String Xor Unit] = f match {
    case s: Framebuffer.SingleLoaded => Free.pure(().right)
    case d: Framebuffer.DoubleLoaded => flipDouble(d)
  }

  private def set(p: Program.Linked): DSL[Unit] = ensure(Current.contains(p))(
      Draw.use(p).freekF[GL] >> Current.set(p).freekF[GL]
  )

  private def set(u: Program.TextureUniform): DSL[String Xor Unit] =
    (for {
      t <- XorT(
              Cache
                .ensure(Cache.get(u.texture), "Unable to find texture.")
                .freekF[GL])
      s <- XorT(
              Cache
                .ensure(Cache.get(u.sampler), "Unable to find sampler.")
                .freekF[GL])
      _ <- xort(Draw.bind(u.unit, u.location, t, s).freekF[GL])
    } yield ()).value

  private def set(p: Program.Linked,
                  ts: Map[String, Texture.Constructor]): DSL[String Xor Unit] =
    p.textureUniforms(ts)
      .traverse(_.traverse(set).map(_.sequenceUnit))
      .map(_.flatMap(identity))

  private def set(vb: VertexBuffer.Loaded): DSL[Unit] =
    ensure(Current.contains(vb))(
        Draw.bind(vb).freekF[GL] >> Current.set(vb).freekF[GL]
    )

  private def set(eb: ElementBuffer.Loaded): DSL[Unit] =
    ensure(Current.contains(eb))(
        Draw.bind(eb).freekF[GL] >> Current.set(eb).freekF[GL]
    )

  private def ensure[A](c: Cache.DSL[Option[A]],
                        msg: String): XorT[DSL, String, A] =
    XorT(Cache.ensure(c, msg).freekF[GL])

  private def xort[A](dsl: DSL[A]): XorT[DSL, String, A] = XorT.right(dsl)

  def draw(draw: DrawOp): DSL[String Xor Unit] =
    (for {
      fl <- ensure(Cache.get(draw.framebuffer),
                   "Framebuffer not loaded. Unable to draw.")
      _ <- xort(set(fl))
      p <- ensure(Cache.get(draw.program),
                  s"Program not loaded. Unable to draw $draw")
      _ <- xort(set(p))
      _ <- XorT(set(p, draw.textureUniforms))
      vb <- ensure(Cache.get(draw.vertexBuffer),
                   s"Vertex buffer not loaded. Unable to draw $draw")
      _ <- xort(set(vb))
      vd <- ensure(Cache.get(draw.vertexData),
                   s"Vertex data not loaded. Unable to draw $draw")
      eb <- ensure(Cache.get(draw.elementBuffer),
                   s"Element buffer not loaded. Unable to draw $draw")
      _ <- xort(set(eb))
      ed <- ensure(Cache.get(draw.elementData),
                   s"Element data not loaded. Unable to draw $draw")
      as <- XorT.fromXor[DSL](p.loaded(draw.attributes))
      _ <- xort(Draw.enable(as, vd.offset(draw.vertexModel)).freekF[GL])
      _ <- xort(Draw(ed.offset(draw.elementModel)).freekF[GL])
      _ <- XorT(flip(fl))
    } yield ()).value

  def clear(c: ClearOp): DSL[String Xor Unit] =
    (for {
      fl <- ensure(Cache.get(c.framebuffer),
                   "Framebuffer not loaded. Unable to draw.")
      _ <- xort(set(fl))
      _ <- xort(Draw.clear(c.bitMask).freekF[GL])
    } yield ()).value
}
