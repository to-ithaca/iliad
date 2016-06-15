package iliad

import cats.free._

object CatsExtras {
  implicit def freeOps[F[_], A](f: F[A]): FreeOps[F, A] = new FreeOps(f)
}

final class FreeOps[F[_], A](f: F[A]) {
  def free: Free[F, A] = Free.liftF(f)
}
