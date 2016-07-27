package iliad
package kernel

//TODO: Parameterize on at
sealed trait InputEvent

object InputEvent {
  case class Tap(at: Long, x: Float, y: Float) extends InputEvent
  case class Swipe(start: Tap, end: Tap) extends InputEvent {
    def distance: Float = {
      val dx = (end.x - start.x).toDouble
      val dy = (end.y - start.y).toDouble
      Math.sqrt(dx * dx + dy * dy).toFloat
    }
  }
}

object EventHandler {
  type Callback[A] = A => Unit
  def zero[A](a: A): Unit = {}
}

import InputEvent._
import EventHandler._

trait EventHandler {
  def onEvent(cb: Callback[InputEvent]): Unit
}
