package iliad
package kernel

import iliad.kernel.utils.vectord._

sealed trait Event
object Event {
  case class Touch(t: TouchEvent) extends Event 
}

sealed trait TouchEvent
object TouchEvent {
  case class Tap(position: Vec2f) extends TouchEvent
}


trait EventHandler {
  def registerTapCallback(cb: TouchEvent.Tap => Unit): Unit
}
