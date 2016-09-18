package iliad

import cats._
import cats.data._
import cats.free._
import cats.functor._
import cats.implicits._


trait CatsInstances extends StateTInstances1 {

  implicit def freeOps[F[_], A](f: Free[F, A]): FreeOps[F, A] = new FreeOps(f)
  implicit def toFreeOps[F[_], A](f: F[A]): ToFreeOps[F, A] = new ToFreeOps(f)
  implicit def sequenceOps[F[_], G[_], A](
      fga: F[G[A]]): SequenceOps[F, G, A] =
    new SequenceOps(fga)
  implicit def sequenceMOps[F[_], G[_], A](
      fgfa: F[G[F[A]]]): SequenceMOps[F, G, A] =
    new SequenceMOps(fgfa)

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
  implicit def stateTObjectOps(stateT: StateT.type): StateTObjectOps = new StateTObjectOps(stateT)
  implicit def kleisliObjectOps(kleisli: Kleisli.type): KleisliObjectOps = new KleisliObjectOps(kleisli)
  implicit def monadReaderOps[F[_], R](
      M: MonadReader[F, R]): MonadReaderOps[F, R] =
    new MonadReaderOps(M)

  implicit def stateTMonadReader[F[_], S](implicit F: Monad[F]): MonadReader[StateT[F, S, ?], S] =
    new StateTMonadReader[F, S] {
      def M = F
    }

  implicit def monadStateInvariant[F[_]]: Invariant[MonadState[F, ?]] = 
    new MonadStateInvariant[F]

  implicit def monadReaderInvariant[F[_]]: Invariant[MonadReader[F, ?]] =
    new MonadReaderInvariant[F]

  implicit def kleisliStateMonadReaderState[R, S]: MonadReaderState[Kleisli[State[S, ?], R, ?], R, S] = new KleisliStateMonadReaderState[R, S]

  implicit def toCatsFunctorOps[F[_], A](fa: F[A]): FunctorOps[F, A] = new FunctorOps(fa)
}

sealed trait StateTInstances1 {

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

final class SequenceMOps[F[_], G[_], A](val fgfa: F[G[F[A]]]) extends AnyVal {
  def sequenceM(implicit T: Traverse[F], M: Monad[F], AA: Applicative[G]): G[F[A]] = fgfa.sequence.map(_.flatten)
}

final class FunctorOps[F[_], A](val fa: F[A]) extends AnyVal {
  def widen[B >: A](implicit FF: Functor[F]): F[B] = fa.asInstanceOf[F[B]]

  def void(implicit FF: Functor[F]): F[Unit] = fa.map(_ => ())

  def as[B](b: B)(implicit FF: Functor[F]): F[B] = fa.map(_ => b)
}

final class StateTObjectOps(val stateT: StateT.type) extends AnyVal {

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

final class KleisliObjectOps(val kleisli: Kleisli.type) extends AnyVal {
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
  
  def raiseError[A](e: E): StateT[F,S,A] = StateT.lift(FE.raiseError(e))
  def flatMap[A, B](fa: StateT[F,S,A])(f: A => StateT[F,S,B]): StateT[F,S,B] = 
    fa.flatMap(f)
}

sealed trait StateTMonadReader[F[_], S] extends MonadReader[StateT[F, S, ?], S] {

  implicit def M: Monad[F]

  def pure[A](x: A): StateT[F, S, A] = StateT.pure(x)
  def ask: StateT[F, S, S] = StateT.get
  def flatMap[A, B](fa: StateT[F,S,A])(f: A => StateT[F,S,B]): StateT[F,S,B] = fa.flatMap(f)
  def local[A](f: S => S)(fa: StateT[F,S,A]): StateT[F,S,A] = ask.flatMapF(fa.runA)
}


final class MonadStateInvariant[F[_]] extends Invariant[MonadState[F, ?]] {
  def imap[A, B](fa: MonadState[F, A])(f: A => B)(g: B => A): MonadState[F, B] =
    new MonadState[F, B] {
      def pure[A](x: A): F[A] = fa.pure(x)
      def flatMap[A, B](faa: F[A])(f: A => F[B]): F[B] = fa.flatMap(faa)(f) 
      def get: F[B] = fa.map(fa.get)(f)
      def set(s: B): F[Unit] = fa.set(g(s))
    }
}


final class MonadReaderInvariant[F[_]] extends Invariant[MonadReader[F, ?]] {
  def imap[A, B](fa: MonadReader[F, A])(f: A => B)(g: B => A): MonadReader[F, B] = 
    new MonadReader[F, B] {
      def pure[A](x: A): F[A] = fa.pure(x)
      def flatMap[A, B](faa: F[A])(f: A => F[B]): F[B] = fa.flatMap(faa)(f) 
      def ask: F[B] = map(fa.ask)(f)
      def local[A](ff: B => B)(faa: F[A]): F[A] = fa.local(a => g(ff(f(a))))(faa)
    }
}

sealed trait MonadReaderState[F[_], R, S] extends MonadReader[F, R] with MonadState[F, S]

final class KleisliStateMonadReaderState[R, S] extends MonadReaderState[Kleisli[State[S, ?], R, ?], R, S] {

  def pure[A](x: A): Kleisli[State[S, ?], R, A] = Kleisli.pure(x)
  def flatMap[A, B](fa: Kleisli[State[S, ?], R, A])(f: A => Kleisli[State[S, ?], R, B]): Kleisli[State[S, ?], R, B] = fa.flatMap(f)
  def ask: Kleisli[State[S, ?], R, R] = Kleisli.ask
  def local[A](f: R => R)(fa: Kleisli[State[S, ?], R, A]): Kleisli[State[S, ?], R, A] = Kleisli.local(f)(fa)
  def get: Kleisli[State[S, ?], R, S] = Kleisli.lift(State.get)
  def set(s: S): Kleisli[State[S, ?], R, Unit] = Kleisli.lift(State.set(s))
}
