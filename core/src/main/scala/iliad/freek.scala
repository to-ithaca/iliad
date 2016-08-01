package iliad

import cats._
import cats.free._
import freek._

object FreekExtra {

  implicit class ToFreeOps[F[_], A](val free: Free[F, A]) extends AnyVal {
    @inline
    def freekF[C <: FX](implicit sub: SubFX[In1[F, ?], C]): Free[sub.Cop, A] =
      free
        .mapSuspension(new (F ~> In1[F, ?]) {
          def apply[A](fa: F[A]): In1[F, A] = In1(fa)
        })
        .expand[C]
  }
}
