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

  type CLoad[A] = (Load :|: Cached :|: FXNil)#Cop[A]
  type DSL[A] = Free[CLoad, A]
  type PRG[F[_], A] =
    ReaderT[StateT[F, Cached.CachedState, ?], GLES30Library, A]

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
}

sealed trait CachedLoadFunctions {
  import CachedLoad.CLoad
  import CachedLoad.DSL

  def apply(s: VertexShader.Source): DSL[VertexShader.Compiled] =
    Cached.get(s).freekF[CLoad] flatMap {
      case Some(v) => Free.pure(v)
      case None =>
        for {
          v <- Load(s).freekF[CLoad]
          _ <- Cached.put(v).freekF[CLoad]
        } yield v
    }

  def apply(s: FragmentShader.Source): DSL[FragmentShader.Compiled] =
    Cached.get(s).freekF[CLoad] flatMap {
      case Some(v) => Free.pure(v)
      case None =>
        for {
          v <- Load(s).freekF[CLoad]
          _ <- Cached.put(v).freekF[CLoad]
        } yield v
    }

  def apply(p: Program.Unlinked): DSL[Program.Linked] =
    Cached.get(p).freekF[CLoad] flatMap {
      case Some(p) => Free.pure(p)
      case None =>
        for {
          v <- CachedLoad(p.vs)
          f <- CachedLoad(p.fs)
          pl <- Load(v, f).freekF[CLoad]
          _ <- Cached.put(pl).freekF[CLoad]
        } yield pl
    }

  def apply(r: VertexData.Ref,
            d: VertexData.Data,
            pageSize: Int,
            vb: VertexBuffer.Constructor): DSL[Unit] =
    Cached.getVertex(vb).freekF[CLoad] flatMap {
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

  def apply(r: ElementData.Ref,
            d: ElementData.Data,
            pageSize: Int,
            eb: ElementBuffer.Constructor): DSL[Unit] =
    Cached.getElement(eb).freekF[CLoad] flatMap {
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
