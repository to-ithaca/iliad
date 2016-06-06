package iliad
package kernel

import iliad.kernel.vectord._

//TODO: Parameterize on at
sealed trait InputEvent

object InputEvent {
  case class Tap(at: Long, position: Vec2f) extends InputEvent
}

object EventHandler {
  type Callback[A] = A => Unit
  def zero[A](a: A): Unit = {}
}


import InputEvent._
import EventHandler._

trait EventHandler {
  def onTap(cb: Callback[Tap]): Unit
}
