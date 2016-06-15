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
    Load.parse(gl).andThen(
      new (ReaderT[F, GLES30Library, ?] ~> PRG[F, ?]) {
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
