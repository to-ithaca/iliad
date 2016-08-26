package iliad

import monocle._
import monocle.syntax.all._

import cats._
import cats.functor.Invariant
import cats.data._

import scala.reflect._

trait MonocleInstances {

  implicit def toMonocleStateTOps[F[_]: Monad, S, A](
      s: StateT[F, S, A]): MonocleStateTOps[F, S, A] =
    new MonocleStateTOps(s)

  implicit def lensInvariant[S]: Invariant[Lens[S, ?]] = new LensInvariant[S]
  implicit def lensCartesian[S]: Cartesian[Lens[S, ?]] = new LensCartesian[S]

  implicit def getterMonad[S]: Monad[Getter[S, ?]] = new GetterMonad[S]

  implicit def getterOps[S, A](getter: Getter[S, A]): GetterOps[S, A] = new GetterOps(getter)
  implicit def setterOps[S, A](setter: Setter[S, A]): SetterOps[S, A] = new SetterOps(setter)

  implicit def lensGetterOps[S, A](lens: Lens[S, A]): GetterOps[S, A] = new GetterOps(lens.asGetter)
  implicit def lensSetterOps[S, A](lens: Lens[S, A]): SetterOps[S, A] = new SetterOps(lens.asSetter)

  implicit def optionalObjectOps(optional: Optional.type): OptionalObjectOps = 
    new OptionalObjectOps(optional)
}

final class MonocleStateTOps[F[_], S, A](val s: StateT[F, S, A]) extends AnyVal {
  def applyLens[R](l: Lens[R, S])(implicit M: Monad[F]): StateT[F, R, A] =
    s.transformS(_ &|-> l get, {
      case (r, s) => r &|-> l set s
    })

  def applyBack[R, SS >: S](ss: S, l: Lens[R, SS])(implicit M: Monad[F]): StateT[F, R, A] =
    s.transformS(_ => ss, {
      case (r, s) => r &|-> l set s
    })
}


final class LensInvariant[S] extends Invariant[Lens[S, ?]] {
  def imap[A, B](fa: Lens[S, A])(f: A => B)(g: B => A): Lens[S, B] = {
    val fb = Lens[A, B](f)(b => a => g(b))
    fa ^|-> fb
  }
}

final class LensCartesian[S] extends Cartesian[Lens[S, ?]] {
  def product[A, B](fa: Lens[S, A], fb: Lens[S, B]): Lens[S, (A, B)] = 
    Lens[S, (A, B)](s => (s &|-> fa get, s &|-> fb get)) {
      case (a, b) => s => ((fa set a) compose (fb set b))(s)
    }
}

final class GetterMonad[S] extends Monad[Getter[S, ?]] {
  def pure[A](x: A): monocle.Getter[S,A] = Getter[S, A](_ => x)

  def flatMap[A, B](fa: Getter[S,A])(f: A => Getter[S,B]): monocle.Getter[S,B] = 
    Getter[S, B](s => f(fa get s) get s)

  override def map[A, B](fa: Getter[S, A])(f: A => B): Getter[S, B] =
   Getter[S, B]((fa get _) andThen f)

  override def ap[A, B](ff: Getter[S, A => B])(fa: Getter[S, A]): Getter[S, B] =
    Getter[S, B](s => (ff get s)(fa get s))
}

final class GetterOps[S, A](val getter: Getter[S, A]) extends AnyVal {
  def view[F[_]](implicit M: MonadReader[F, S]): F[A] =
    M.reader(getter get)

  def inspect[F[_]](implicit M: MonadState[F, S]): F[A] =
    M.inspect(getter get)
}

final class SetterOps[S, A](val setter: Setter[S, A]) extends AnyVal {
  def assign[F[_]](a: A)(implicit M: MonadState[F, S]): F[Unit] =
    M.modify(setter set a)
  def modifying[F[_]](f: A => A)(implicit M: MonadState[F, S]): F[Unit] =
    M.modify(setter modify f)
}

final class OptionalObjectOps(val optional: Optional.type) extends AnyVal {
  def narrow[A, B <: A](implicit ct: ClassTag[B]): Optional[A, B] = Optional[A, B]({
    case b: B => Some(b)
    case _ => None
  })(b => _ => b)
}
