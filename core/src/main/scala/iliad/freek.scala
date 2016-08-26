package iliad

import cats._
import cats.free._
import freek._

trait FreekInstances {
  implicit def toFreekFreeOps[F[_], A](free: Free[F, A]): FreekToFreeOps[F, A] = new FreekToFreeOps(free)
}

final class FreekToFreeOps[F[_], A](val free: Free[F, A]) extends AnyVal {

  def expand[C[_] <: CoproductK[_]](
    implicit sub: SubCop[ConsK[F, CNilK, ?], C]): Free[C, A] =
    free
      .mapSuspension(new (F ~> ConsK[F, CNilK, ?]) {
        def apply[A](fa: F[A]): ConsK[F, CNilK, A] = Inlk(fa)
      })
      .expand[C]
}
