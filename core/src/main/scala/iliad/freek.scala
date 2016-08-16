package iliad

import cats._
import cats.free._
import freek._

object FreekExtra {

  implicit class ToFreeOps[F[_], A](val free: Free[F, A]) extends AnyVal {
    @inline
    def expand[C[_] <: CoproductK[_]](
        implicit sub: SubCop[ConsK[F, CNilK, ?], C]): Free[C, A] =
      free
        .mapSuspension(new (F ~> ConsK[F, CNilK, ?]) {
          def apply[A](fa: F[A]): ConsK[F, CNilK, A] = Inlk(fa)
        })
        .expand[C]
  }
}
