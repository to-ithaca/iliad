package iliad
package gfx

import cats._
import cats.data._
import cats.implicits._

object Action {
  type Effect = StateT[Xor[String, ?], Graph.Instance, Unit]

  private[gfx] def apply(a: Action): Effect = a match {
    case Show(ns) =>
      Instantiate(ns).transformF(_.leftMap(_.unwrap.mkString("\n")))
  }
}

sealed trait Action
private case class Show(ns: List[Node.Instance]) extends Action

trait ActionFunctions {
  private def lift(a: Action): Graphics =
    shapeless.Coproduct[Graphics](a)

  def show(ns: List[Node.Instance]): Graphics =
    lift(Show(ns))
}
