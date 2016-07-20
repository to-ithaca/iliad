package iliad
package gfx

import cats._
import cats.data._
import cats.implicits._

import CatsExtra._

object Action {
  type Effect =
    StateT[Xor[NonEmptyList[GraphicsError], ?], Graph.Instance, Unit]

  private[gfx] def apply(a: Action): Effect = a match {
    case Show(ns) => Instantiate(ns).transformF(_.leftMap(_.widen))
  }
}

sealed trait Action
private case class Show(ns: List[Node.Instance]) extends Action

trait ActionFunctions {
  private def lift(a: Action): Graphics.Graphics =
    shapeless.Coproduct[Graphics.Graphics](a)

  def show(ns: List[Node.Instance]): Graphics.Graphics =
    lift(Show(ns))
}
