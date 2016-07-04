package iliad

import cats._
import cats.data._
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
  implicit def xortOps[F[_], A, B](xort: XorT[F, A, B]): XorTOps[F, A, B] =
    new XorTOps(xort)
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

object StateTExtra {
  def inspect[F[_]: Applicative, S, A](f: S => F[A]): StateT[F, S, A] =
    StateT(s => f(s).map(s -> _))

  def lift[F[_]: Applicative, S, A](fa: F[A]): StateT[F, S, A] =
    StateT(s => fa.map(a => s -> a))
}

object KleisliExtra {
  def lift[F[_], A, B](fb: F[B]): Kleisli[F, A, B] = Kleisli(_ => fb)
}

final class XorTOps[F[_], A, B](xort: XorT[F, A, B]) {
  def transformF[G[_]](f: F[A Xor B] => G[A Xor B]): XorT[G, A, B] =
    XorT(f(xort.value))
}
