package iliad
package gl

import iliad.kernel.platform.GLES30Library

import cats._
import cats.data._
import cats.free._
import cats.implicits._

import freek._

import monocle.function.all._
import monocle.std.all._

import FreekExtra._
import MonocleExtra._
import CatsExtra._

object CachedGL {

  type CachedGL[A] =
    (Load :|: Cached :|: Draw :|: Current :|: GL :|: FXNil)#Cop[A]
  type DSL[A] = Free[CachedGL, A]
  type PRG[F[_], A] =
    ReaderT[StateT[F, (Cached.State, Current.State), ?], GLES30Library, A]

  private def liftOpenGL[F[_]: Monad]: GL.Effect[F, ?] ~> PRG[F, ?] =
    new (GL.Effect[F, ?] ~> PRG[F, ?]) {
      def apply[A](eff: ReaderT[F, GLES30Library, A]): PRG[F, A] =
        eff.mapF(_.liftT[StateT[?[_], (Cached.State, Current.State), ?]])
    }

  def liftS[F[_]: Monad, A](
      s: State[(Cached.State, Current.State), A]): PRG[F, A] =
    ReaderT(_ => s.transformF(sa => Applicative[F].pure(sa.value)))

  private def liftCached[F[_]: Monad]: Cached.Effect ~> PRG[F, ?] =
    new (Cached.Effect ~> PRG[F, ?]) {
      def apply[A](eff: State[Cached.State, A]): PRG[F, A] =
        liftS(eff.applyLens[(Cached.State, Current.State)](first))
    }

  private def liftCurrent[F[_]: Monad]: Current.Effect ~> PRG[F, ?] =
    new (Current.Effect ~> PRG[F, ?]) {
      def apply[A](eff: State[Current.State, A]): PRG[F, A] =
        liftS(eff.applyLens[(Cached.State, Current.State)](second))
    }

  def runner[F[_]: Monad](
      f: GL.Interpreter[GL.Effect[F, ?]]): Interpreter[CachedGL, PRG[F, ?]] =
    Load.parse(f).andThen(liftOpenGL) :&:
      CachedParser.andThen(liftCached[F]) :&:
        Draw.parse(f).andThen(liftOpenGL) :&:
          CurrentParser.andThen(liftCurrent[F]) :&:
            f.andThen(liftOpenGL)

  private def getOrElse[A](f: DSL[Option[A]])(g: => DSL[A]): DSL[A] =
    f.flatMap(_.map(Free.pure[CachedGL, A]).getOrElse(g))

  private def load(s: VertexShader.Source): DSL[VertexShader.Compiled] =
    getOrElse(Cached.get(s).freekF[CachedGL])(for {
      v <- Load(s).freekF[CachedGL]
      _ <- Cached.put(v).freekF[CachedGL]
    } yield v)

  private def load(s: FragmentShader.Source): DSL[FragmentShader.Compiled] =
    getOrElse(Cached.get(s).freekF[CachedGL])(for {
      v <- Load(s).freekF[CachedGL]
      _ <- Cached.put(v).freekF[CachedGL]
    } yield v)

  def load(p: Program.Unlinked): DSL[Program.Linked] =
    getOrElse(Cached.get(p).freekF[CachedGL])(for {
      v <- load(p.vertex)
      f <- load(p.fragment)
      pl <- Load(v, f).freekF[CachedGL]
      _ <- Cached.put(pl).freekF[CachedGL]
      _ <- Current.set(pl).freekF[CachedGL]
    } yield pl)

  private def add(u: VertexBuffer.Update): DSL[Unit] =
    for {
      _ <- Cached.put(u).freekF[CachedGL]
      _ <- Current.set(u.buffer).freekF[CachedGL]
    } yield ()

  def load(r: VertexData.Ref, d: VertexData.Data, pageSize: Int): DSL[Unit] =
    Cached.get(r.buffer).freekF[CachedGL] flatMap {
      case Some(prev) =>
        if (VertexBuffer.fits(prev, d.size))
          for {
            next <- Load.insert(r, d, pageSize, prev).freekF[CachedGL]
            _ <- add(next)
          } yield ()
        else
          for {
            next <- Load.copy(r, d, pageSize, prev).freekF[CachedGL]
            _ <- add(next)
          } yield ()
      case None =>
        for {
          b <- Load.create(r, d, pageSize, r.buffer).freekF[CachedGL]
          _ <- add(b)
        } yield ()
    }

  private def add(u: ElementBuffer.Update): DSL[Unit] =
    for {
      _ <- Cached.put(u).freekF[CachedGL]
      _ <- Current.set(u.buffer).freekF[CachedGL]
    } yield ()

  def load(r: ElementData.Ref, d: ElementData.Data, pageSize: Int): DSL[Unit] =
    Cached.get(r.buffer).freekF[CachedGL] flatMap {
      case Some(prev) =>
        if (ElementBuffer.fits(prev, d.size))
          for {
            next <- Load.insert(r, d, pageSize, prev).freekF[CachedGL]
            _ <- add(next)
          } yield ()
        else
          for {
            next <- Load.copy(r, d, pageSize, prev).freekF[CachedGL]
            _ <- add(next)
          } yield ()
      case None =>
        for {
          b <- Load.create(r, d, pageSize, r.buffer).freekF[CachedGL]
          _ <- add(b)
        } yield ()
    }

  def load(t: Texture.Constructor,
           d: Option[Texture.Data]): DSL[Texture.Loaded] =
    getOrElse(Cached.get(t).freekF[CachedGL])(for {
      tl <- Load(t, d).freekF[CachedGL]
      _ <- Cached.put(tl).freekF[CachedGL]
    } yield tl)

  def load(r: Renderbuffer.Constructor): DSL[Renderbuffer.Loaded] =
    getOrElse(Cached.get(r).freekF[CachedGL])(for {
      rl <- Load(r).freekF[CachedGL]
      _ <- Cached.put(rl).freekF[CachedGL]
    } yield rl)

  private def attachment(a: Framebuffer.AttachmentConstructor)
    : DSL[String Xor Framebuffer.AttachmentLoaded] = a match {
    case t: Texture.Constructor =>
      Cached
        .ensure(Cached.get(t), s"Texture missing $t")
        .freekF[CachedGL]
        .widen
    case r: Renderbuffer.Constructor =>
      Cached
        .ensure(Cached.get(r), s"Renderbuffer missing $r")
        .freekF[CachedGL]
        .widen
  }

  private def attachments(f: Framebuffer.Constructor): DSL[
      String Xor List[(FramebufferAttachment, Framebuffer.AttachmentLoaded)]] =
    f.attachments.traverse {
      case (c, a) => XorT(attachment(a)).map(c -> _).value
    }.map(_.sequence)

  def load(f: Framebuffer.Constructor): DSL[String Xor Unit] =
    Cached.get(f).freekF[CachedGL] flatMap {
      case Some(fl) => Free.pure(().right)
      case None =>
        (for {
          as <- XorT(attachments(f))
          fl <- xort(Load(f, as).freekF[CachedGL])
          _ <- xort(Cached.put(fl).freekF[CachedGL])
        } yield ()).value
    }

  private def ensure(f: Current.DSL[Boolean])(g: => DSL[Unit]): DSL[Unit] =
    f.freekF[CachedGL] flatMap (b => if (b) Free.pure(()) else g)

  private def set(f: Framebuffer.Loaded): DSL[Unit] =
    ensure(Current.contains(f))(Draw.bind(f).freekF[CachedGL] >>
          Current.set(f).freekF[CachedGL])

  private def flip(t: Texture.Constructor): DSL[String Xor Unit] =
    (for {
      tl <- ensure(Cached.get(t), "Texture not loaded.")
      _ <- tl match {
            case d: Texture.DoubleLoaded =>
              xort(Cached.put(d.flip).freekF[CachedGL])
            case _ => XorT.pure[DSL, String, Unit](())
          }
    } yield ()).value

  private def flipDouble(f: Framebuffer.DoubleLoaded): DSL[String Xor Unit] =
    (for {
      _ <- xort(Cached.put(f.flip).freekF[CachedGL])
      _ <- XorT(f.constructor.textures.traverse(flip).map(_.sequenceUnit))
    } yield ()).value

  private def flip(f: Framebuffer.Loaded): DSL[String Xor Unit] = f match {
    case s: Framebuffer.SingleLoaded => Free.pure(().right)
    case d: Framebuffer.DoubleLoaded => flipDouble(d)
  }

  private def set(p: Program.Linked): DSL[Unit] = ensure(Current.contains(p))(
      Draw.use(p).freekF[CachedGL] >> Current.set(p).freekF[CachedGL]
  )

  private def set(u: Program.TextureUniform): DSL[String Xor Unit] =
    (for {
      t <- XorT(
              Cached
                .ensure(Cached.get(u.texture), "Unable to find texture.")
                .freekF[CachedGL])
      s <- XorT(
              Cached
                .ensure(Cached.get(u.sampler), "Unable to find sampler.")
                .freekF[CachedGL])
      _ <- xort(Draw.bind(u.unit, u.location, t, s).freekF[CachedGL])
    } yield ()).value

  private def set(p: Program.Linked,
                  ts: Map[String, Texture.Constructor]): DSL[String Xor Unit] =
    p.textureUniforms(ts)
      .traverse(_.traverse(set).map(_.sequenceUnit))
      .map(_.flatMap(identity))

  private def set(vb: VertexBuffer.Loaded): DSL[Unit] =
    ensure(Current.contains(vb))(
        Draw.bind(vb).freekF[CachedGL] >> Current.set(vb).freekF[CachedGL]
    )

  private def set(eb: ElementBuffer.Loaded): DSL[Unit] =
    ensure(Current.contains(eb))(
        Draw.bind(eb).freekF[CachedGL] >> Current.set(eb).freekF[CachedGL]
    )

  private def ensure[A](c: Cached.DSL[Option[A]],
                        msg: String): XorT[DSL, String, A] =
    XorT(Cached.ensure(c, msg).freekF[CachedGL])

  private def xort[A](dsl: DSL[A]): XorT[DSL, String, A] = XorT.right(dsl)

  def draw(draw: DrawOp): XorT[DSL, String, Unit] = 
    for {
      fl <- ensure(Cached.get(draw.framebuffer),
                   "Framebuffer not loaded. Unable to draw.")
      _ <- xort(set(fl))
      p <- ensure(Cached.get(draw.program),
                  s"Program not loaded. Unable to draw $draw")
      _ <- xort(set(p))
      _ <- XorT(set(p, draw.textureUniforms))
      vb <- ensure(Cached.get(draw.vertexBuffer),
                   s"Vertex buffer not loaded. Unable to draw $draw")
      _ <- xort(set(vb))
      vd <- ensure(Cached.get(draw.vertexData),
                   s"Vertex data not loaded. Unable to draw $draw")
      eb <- ensure(Cached.get(draw.elementBuffer),
                   s"Element buffer not loaded. Unable to draw $draw")
      _ <- xort(set(eb))
      ed <- ensure(Cached.get(draw.elementData),
                   s"Element data not loaded. Unable to draw $draw")
      as <- XorT.fromXor[DSL](p.loaded(draw.attributes))
      _ <- xort(Draw.enable(as, vd.offset(draw.vertexModel)).freekF[CachedGL])
      _ <- xort(Draw(ed.offset(draw.elementModel)).freekF[CachedGL])
      _ <- XorT(flip(fl))
    } yield ()

  def clear(c: ClearOp): XorT[DSL, String, Unit] =
    for {
      fl <- ensure(Cached.get(c.framebuffer),
                   "Framebuffer not loaded. Unable to draw.")
      _ <- xort(set(fl))
      _ <- xort(Draw.clear(c.bitMask).freekF[CachedGL])
    } yield ()
}
