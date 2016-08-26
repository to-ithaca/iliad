package iliad

import cats._
import cats.data._
import cats.free._
import cats.implicits._

object CatsExtra {
  implicit def freeOps[F[_], A](f: Free[F, A]): FreeOps[F, A] = new FreeOps(f)
  implicit def toFreeOps[F[_], A](f: F[A]): ToFreeOps[F, A] = new ToFreeOps(f)
  implicit def sequenceOps[F[_], G[_], A](
      fga: F[G[A]]): SequenceOps[F, G, A] =
    new SequenceOps(fga)
  implicit def traverseOps[F[_], A](fa: F[A]): TraverseOps[F, A] =
    new TraverseOps(fa)
  implicit def xortOps[F[_], A, B](xort: XorT[F, A, B]): XorTOps[F, A, B] =
    new XorTOps(xort)
  implicit def xorOps[A, B](xor: Xor[A, B]): XorOps[A, B] =
    new XorOps(xor)
  implicit def validatedNelOps[E, A](
      v: ValidatedNel[E, A]): ValidatedNelOps[E, A] =
    new ValidatedNelOps(v)
  implicit def oneAndOps[F[_], A](o: OneAnd[F, A]): OneAndOps[F, A] =
    new OneAndOps(o)
  implicit def monadReaderOps[F[_], R](
      M: MonadReader[F, R]): MonadReaderOps[F, R] =
    new MonadReaderOps(M)

  implicit def stateTMonadError[F[_], S, E](implicit M: MonadError[F, E]): MonadError[StateT[F, S, ?], E]
  = new StateTMonadError[F, S, E] {
    def FE = M
  }
}

final class FreeOps[F[_], A](val f: Free[F, A]) extends AnyVal {
  def widen[B >: A]: Free[F, B] = f.map(identity)
}

final class ToFreeOps[F[_], A](val f: F[A]) extends AnyVal {
  def free: Free[F, A] = Free.liftF(f)
}

final class TraverseOps[F[_], A](val fa: F[A]) extends AnyVal {
  def traverseUnit[G[_]: Applicative, B](f: A => G[B])(implicit T: Traverse[F]): G[Unit] =
    fa.traverse(f).map(_ => ())
}

final class SequenceOps[F[_], G[_], A](val fga: F[G[A]]) extends AnyVal {
  def sequenceUnit(implicit T: Traverse[F], AA: Applicative[G]): G[Unit] = fga.sequence.map(_ => ())
}

object StateTExtra {

  def get[F[_]: Applicative, S]: StateT[F, S, S] = StateT(s => Applicative[F].pure((s, s)))

  def modify[F[_]: Applicative, S](f: S => S): StateT[F, S, Unit] =
    StateT(s => Applicative[F].pure((f(s), ())))

  def inspectF[F[_]: Applicative, S, A](f: S => F[A]): StateT[F, S, A] =
    StateT(s => f(s).map(s -> _))

  def lift[F[_]: Applicative, S, A](fa: F[A]): StateT[F, S, A] =
    StateT(s => fa.map(a => s -> a))

  def modifyF[F[_]: Applicative, S](f: S => F[S]): StateT[F, S, Unit] =
    StateT(s => f(s).map(s => (s, ())))
}

object KleisliExtra {
  def lift[F[_], A, B](fb: F[B]): Kleisli[F, A, B] = Kleisli(_ => fb)
}

final class XorTOps[F[_], A, B](val xort: XorT[F, A, B]) extends AnyVal {
  def transformF[G[_]](f: F[A Xor B] => G[A Xor B]): XorT[G, A, B] =
    XorT(f(xort.value))

  def leftWiden[AA >: A](implicit F: Functor[F]): XorT[F, AA, B] =
    xort.leftMap(a => a)
}

final class XorOps[A, B](val xor: Xor[A, B]) extends AnyVal {
  def leftWiden[AA >: A]: Xor[AA, B] = xor.leftMap(a => a)
}

final class ValidatedNelOps[E, A](val v: ValidatedNel[E, A]) extends AnyVal {
  def widen[EE >: E]: ValidatedNel[EE, A] = v.leftMap(_.map(identity))
}

final class OneAndOps[F[_], A](val o: OneAnd[F, A]) extends AnyVal {
  def widen[AA >: A](implicit FF: Functor[F]): OneAnd[F, AA] = o.map(identity)
}

final class MonadReaderOps[F[_], R](val M: MonadReader[F, R]) extends AnyVal {
  def reader[A](f: R => A): F[A] = M.map(M.ask)(f)
}

sealed trait StateTMonadError[F[_], S, E] extends MonadError[StateT[F, S, ?], E] {

  implicit def FE: MonadError[F, E]
  
  def pure[A](x: A): StateT[F,S,A] = StateT.pure[F, S, A](x)
  def handleErrorWith[A](fa: StateT[F,S,A])(f: E => StateT[F,S,A]): StateT[F,S,A] = 
    StateT( s => FE.handleErrorWith(fa.run(s))(e => f(e).run(s)))
  
  def raiseError[A](e: E): StateT[F,S,A] = StateTExtra.lift(FE.raiseError(e))
  def flatMap[A, B](fa: StateT[F,S,A])(f: A => StateT[F,S,B]): StateT[F,S,B] = 
    fa.flatMap(f)
}
