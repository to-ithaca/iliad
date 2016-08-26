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
    case ShowDraw(d, sources) => Instantiate(d, sources).transformF(_.leftMap(_.widen))
    case ShowClear(c) => Instantiate(c).transformF(_.leftMap(_.widen))
    case Hide(ns) => HideInstantiate(ns).transformF(_.leftMap(_.widen))
  }
}

sealed trait Action
private case class ShowDraw(d: Draw.Instance, sources: List[Draw.Instance]) extends Action
private case class ShowClear(d: Clear.Instance) extends Action
private case class Hide(ns: List[Node.Instance]) extends Action

trait ActionFunctions {
  private def lift(a: Action): GFX =
    shapeless.Coproduct[GFX](a)

  def show(d: Draw.Instance, sources: Draw.Instance*): GFX =
    lift(ShowDraw(d, sources.toList))

  def show(c: Clear.Instance): GFX =
    lift(ShowClear(c))

  def hide(ns: Node.Instance*): GFX =
    lift(Hide(ns.toList))
}
