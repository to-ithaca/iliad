package iliad

import monocle._
import monocle.syntax.all._

import cats.data._

object MonocleExtra {

  implicit def toStateOps[S, A](s: State[S, A]): StateOps[S, A] =
    new StateOps(s)

  final class StateOps[S, A](s: State[S, A]) {
    def applyLens[R](l: Lens[R, S]): State[R, A] =
      s.transformS(_ &|-> l get, {
        case (r, s) => r &|-> l set s
      })
  }
}
