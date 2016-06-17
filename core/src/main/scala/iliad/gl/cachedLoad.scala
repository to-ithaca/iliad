package iliad
package gl

import iliad.kernel.platform.GLES30Library

import cats._
import cats.arrow._
import cats.data._
import cats.free._, Free._
import cats.implicits._

import freek._

import FreekExtra._

object CachedLoad extends CachedLoadFunctions {

  type CLoad[A] = (Load :|: Cached :|: Draw :|: Current :|:  FXNil)#Cop[A]
  type DSL[A] = Free[CLoad, A]
  type PRG[F[_], A] =
    ReaderT[StateT[F, (Cached.CachedState, Current.CurrentState), ?], GLES30Library, A]
/*
  private def load[F[_]: Monad](
      gl: GL ~> ReaderT[F, GLES30Library, ?]): Load ~> PRG[F, ?] =
    Load
      .parse(gl)
      .andThen(new (ReaderT[F, GLES30Library, ?] ~> PRG[F, ?]) {
        def apply[A](r: ReaderT[F, GLES30Library, A]): PRG[F, A] =
          r.mapF(fa => StateT(s => fa.map(s -> _)))
      })

  private def cached[F[_]: Monad] =
    Cached.parse.andThen(
        new (State[Cached.CachedState, ?] ~> PRG[F, ?]) {
      def apply[A](s: State[Cached.CachedState, A]): PRG[F, A] =
        ReaderT(_ => s.transformF(sa => Applicative[F].pure(sa.value)))
    })

  def runner[F[_]: Monad](gl: GL ~> ReaderT[F, GLES30Library, ?]) =
    load[F](gl) :&: cached[F]
*/
}

sealed trait CachedLoadFunctions {
  import CachedLoad.CLoad
  import CachedLoad.DSL

  def load(s: VertexShader.Source): DSL[VertexShader.Compiled] =
    Cached.get(s).freekF[CLoad] flatMap {
      case Some(v) => Free.pure(v)
      case None =>
        for {
          v <- Load(s).freekF[CLoad]
          _ <- Cached.put(v).freekF[CLoad]
        } yield v
    }

  def load(s: FragmentShader.Source): DSL[FragmentShader.Compiled] =
    Cached.get(s).freekF[CLoad] flatMap {
      case Some(v) => Free.pure(v)
      case None =>
        for {
          v <- Load(s).freekF[CLoad]
          _ <- Cached.put(v).freekF[CLoad]
        } yield v
    }

  def load(p: Program.Unlinked): DSL[Program.Linked] =
    Cached.get(p).freekF[CLoad] flatMap {
      case Some(p) => Free.pure(p)
      case None =>
        for {
          v <- CachedLoad.load(p.vs)
          f <- CachedLoad.load(p.fs)
          pl <- Load(v, f).freekF[CLoad]
          _ <- Cached.put(pl).freekF[CLoad]
        } yield pl
    }

  def load(r: VertexData.Ref,
            d: VertexData.Data,
            pageSize: Int,
            vb: VertexBuffer.Constructor): DSL[Unit] =
    Cached.get(vb).freekF[CLoad] flatMap {
      case Some(prev) =>
        if (prev.fits(d.size))
          for {
            next <- Load.insert(r, d, pageSize, prev).freekF[CLoad]
            _ <- Cached.update(prev, next).freekF[CLoad]
          } yield ()
        else
          for {
            next <- Load.copy(r, d, pageSize, prev).freekF[CLoad]
            _ <- Cached.update(prev, next).freekF[CLoad]
          } yield ()
      case None =>
        for {
          b <- Load.newBuffer(r, d, pageSize, vb).freekF[CLoad]
          _ <- Cached.put(b).freekF[CLoad]
        } yield ()
    }

  def load(r: ElementData.Ref,
            d: ElementData.Data,
            pageSize: Int,
            eb: ElementBuffer.Constructor): DSL[Unit] =
    Cached.get(eb).freekF[CLoad] flatMap {
      case Some(prev) =>
        if (prev.fits(d.size))
          for {
            next <- Load.insert(r, d, pageSize, prev).freekF[CLoad]
            _ <- Cached.update(prev, next).freekF[CLoad]
          } yield ()
        else
          for {
            next <- Load.copy(r, d, pageSize, prev).freekF[CLoad]
            _ <- Cached.update(prev, next).freekF[CLoad]
          } yield ()
      case None =>
        for {
          b <- Load.newBuffer(r, d, pageSize, eb).freekF[CLoad]
          _ <- Cached.put(b).freekF[CLoad]
        } yield ()
    }

  def clear(bitMask: ChannelBitMask): DSL[Unit] = Draw.clear(bitMask).freekF[CLoad]

  private def doIfNot(pred: Current.DSL[Boolean])(f: DSL[Unit]): DSL[Unit] = pred.freekF[CLoad] flatMap {
    case false => f
    case true => Free.pure(())
  }

  private def setFramebuffer(framebuffer: Int): DSL[Unit] = doIfNot(Current.contains(framebuffer))(
    Draw.bindFramebuffer(framebuffer).freekF[CLoad] >> Current.set(framebuffer).freekF[CLoad]
  )

  private def set(p: Program.Linked): DSL[Unit] = doIfNot(Current.contains(p))(
    Draw.use(p).freekF[CLoad] >> Current.set(p).freekF[CLoad]
  )

  private def set(vb: VertexBuffer.Loaded): DSL[Unit] = doIfNot(Current.contains(vb))(
    Draw.bind(vb).freekF[CLoad] >> Current.set(vb).freekF[CLoad]
  )

  private def set(eb: ElementBuffer.Loaded): DSL[Unit] = doIfNot(Current.contains(eb))(
    Draw.bind(eb).freekF[CLoad] >> Current.set(eb).freekF[CLoad]
  )

  private def ensure[A](c: Cached.DSL[Option[A]], msg: String): XorT[DSL, String, A] = 
    XorT(Cached.ensure(c, msg).freekF[CLoad])

  private def xort[A](dsl: DSL[A]): XorT[DSL, String, A] = XorT.right(dsl)

  private def draw(draw: DrawOp): DSL[String Xor Unit] = (for {
    _ <- xort(setFramebuffer(draw.framebuffer))
    p <- ensure(Cached.get(draw.program), s"Program not loaded. Unable to draw $draw")
    _ <- xort(set(p)) 
    vb <- ensure(Cached.get(draw.model.vertex.ref.b), s"Vertex buffer not loaded. Unable to draw $draw")
    _ <- xort(set(vb))
    vd <- ensure(Cached.get(draw.model.vertex.ref), s"Vertex data not loaded. Unable to draw $draw")
    eb <- ensure(Cached.get(draw.model.element.ref.b), s"Element buffer not loaded. Unable to draw $draw")
    _ <- xort(set(eb))
    ed <- ensure(Cached.get(draw.model.element.ref), s"Element data not loaded. Unable to draw $draw")
    as <- XorT.fromXor[DSL](p.loaded(draw.model.vertex.ref.b.attributes))
    _ <- xort(Draw.enable(as, vd.offset(draw.model.vertex)).freekF[CLoad])
    _ <- xort(Draw(ed.offset(draw.model.element)).freekF[CLoad])
 } yield ()).value
}

/*
object TestRunner {
  def run[A](lib: GLES30Library)(start: String)(
      cmd: CachedLoad.DSL[A]): (String, A) = {
    val r = CachedLoad.runner[Id](GL.runInterpreter)
    cmd.interpret(r).run(lib).run(start)
  }
}
 */
