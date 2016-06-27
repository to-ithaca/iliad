package iliad

import cats._
import cats.free._
import cats.implicits._

object CatsExtra {
  implicit def freeOps[F[_], A](f: Free[F, A]): FreeOps[F, A] = new FreeOps(f)
  implicit def toFreeOps[F[_], A](f: F[A]): ToFreeOps[F, A] = new ToFreeOps(f)
  implicit def sequenceOps[F[_]: Traverse, G[_]: Applicative, A](
      fga: F[G[A]]): SequenceOps[F, G, A] =
    new SequenceOps(fga)
  implicit def traverseOps[F[_]: Traverse, A](fa: F[A]): TraverseOps[F, A] =
    new TraverseOps(fa)
}

final class FreeOps[F[_], A](f: Free[F, A]) {
  def widen[B >: A]: Free[F, B] = f.map(identity)
}

final class ToFreeOps[F[_], A](f: F[A]) {
  def free: Free[F, A] = Free.liftF(f)
}

final class TraverseOps[F[_]: Traverse, A](fa: F[A]) {
  def traverseUnit[G[_]: Applicative, B](f: A => G[B]): G[Unit] =
    fa.traverse(f).map(_ => ())
}

final class SequenceOps[F[_]: Traverse, G[_]: Applicative, A](fga: F[G[A]]) {
  def sequenceUnit: G[Unit] = fga.sequence.map(_ => ())
}
