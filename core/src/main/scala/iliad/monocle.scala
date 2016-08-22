package iliad

import monocle._
import monocle.syntax.all._

import cats._
import cats.functor.Invariant
import cats.data._

object MonocleExtra {

  implicit def toStateTOps[F[_]: Monad, S, A](
      s: StateT[F, S, A]): StateTOps[F, S, A] =
    new StateTOps(s)

  final class StateTOps[F[_], S, A](val s: StateT[F, S, A]) extends AnyVal {
    def applyLens[R](l: Lens[R, S])(implicit M: Monad[F]): StateT[F, R, A] =
      s.transformS(_ &|-> l get, {
        case (r, s) => r &|-> l set s
      })

    def applyBack[R, SS >: S](ss: S, l: Lens[R, SS])(implicit M: Monad[F]): StateT[F, R, A] =
      s.transformS(_ => ss, {
        case (r, s) => r &|-> l set s
      })
  }

  implicit def lensInvariant[S]: Invariant[Lens[S, ?]] = new LensInvariant[S]
  implicit def lensCartesian[S]: Cartesian[Lens[S, ?]] = new LensCartesian[S]

  implicit def getterApply[S]: Apply[Getter[S, ?]] = new GetterApply[S]
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

final class GetterApply[S] extends Apply[Getter[S, ?]] {
 def map[A, B](fa: Getter[S, A])(f: A => B): Getter[S, B] =
   Getter[S, B]((fa get _) andThen f)
  def ap[A, B](ff: Getter[S, A => B])(fa: Getter[S, A]): Getter[S, B] =
    Getter[S, B](s => (ff get s)(fa get s))
}
