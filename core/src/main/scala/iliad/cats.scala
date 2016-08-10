package iliad

import cats._
import cats.data._
import cats.free._
import cats.implicits._

//TODO: get rid of merged functions

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
  implicit def xorOps[A, B](xor: Xor[A, B]): XorOps[A, B] =
    new XorOps(xor)
  implicit def validatedNelOps[E, A](
      v: ValidatedNel[E, A]): ValidatedNelOps[E, A] =
    new ValidatedNelOps(v)
  implicit def nonEmptyListOps[A](o: NonEmptyList[A]): NonEmptyListOps[A] =
    new NonEmptyListOps(o)
  implicit def monadReaderOps[F[_], R](
      M: MonadReader[F, R]): MonadReaderOps[F, R] =
    new MonadReaderOps(M)

  implicit def kleisliMonadRec[F[_], A](
      implicit ev0: Monad[F],
      ev1: MonadRec[F]): MonadRec[Kleisli[F, A, ?]] =
    new KleisliMonadRec[F, A] {
      def F0: Monad[F] = ev0
      def F1: MonadRec[F] = ev1
    }
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

final class XorTOps[F[_], A, B](xort: XorT[F, A, B]) {
  def transformF[G[_]](f: F[A Xor B] => G[A Xor B]): XorT[G, A, B] =
    XorT(f(xort.value))

  def leftWiden[AA >: A](implicit F: Functor[F]): XorT[F, AA, B] =
    xort.leftMap(a => a)
}

final class XorOps[A, B](xor: Xor[A, B]) {
  def leftWiden[AA >: A]: Xor[AA, B] = xor.leftMap(a => a)

  //TODO: this should not be here
  def task: fs2.Task[B] =
    xor
      .bimap(a => fs2.Task.fail(new Error(a.toString)), fs2.Task.now)
      .merge[fs2.Task[B]]
}

final class ValidatedNelOps[E, A](v: ValidatedNel[E, A]) {
  def widen[EE >: E]: ValidatedNel[EE, A] = v.leftMap(_.map(identity))
}

final class NonEmptyListOps[A](o: NonEmptyList[A]) {
  def widen[AA >: A]: NonEmptyList[AA] = o.map(identity)
}

final class MonadReaderOps[F[_], R](M: MonadReader[F, R]) {
  def reader[A](f: R => A): F[A] = M.map(M.ask)(f)
}

private trait KleisliMonadRec[F[_], A]
    extends MonadRec[Kleisli[F, A, ?]]
    with Monad[Kleisli[F, A, ?]] {
  implicit def F0: Monad[F]
  implicit def F1: MonadRec[F]

  def pure[B](x: B): Kleisli[F, A, B] =
    Kleisli.pure[F, A, B](x)

  def flatMap[B, C](fa: Kleisli[F, A, B])(
      f: B => Kleisli[F, A, C]): Kleisli[F, A, C] =
    fa.flatMap(f)

  def tailRecM[B, C](b: B)(f: B => Kleisli[F, A, B Xor C]): Kleisli[F, A, C] =
    Kleisli[F, A, C](a => F1.tailRecM[B, C](b)(bb => f(bb).run(a)))
}
