package iliad
package kernel

import cats._
import cats.data._

//TODO: Parameterize on at
//TODO: drags should have NELs
sealed trait InputEvent

object InputEvent {
  case class Point(at: Long, x: Float, y: Float)

  case class Tap(point: Point) extends InputEvent {
    lazy val at: Long = point.at
  }
  case class DragStarted(start: Point, current: Point) extends InputEvent
  case class DragContinued(points: NonEmptyList[InputEvent.Point])
      extends InputEvent {
    def start: Point = points.toList.last
    def end: Point = points.head
  }
  case class DragBecameSwipe(points: NonEmptyList[InputEvent.Point])
      extends InputEvent {
    lazy val start: Point = points.toList.last
    lazy val end: Point = points.head
    lazy val distance: Float = {
      val dx = (end.x - start.x).toDouble
      val dy = (end.y - start.y).toDouble
      Math.sqrt(dx * dx + dy * dy).toFloat
    }
  }

  case class DragFinished(points: NonEmptyList[InputEvent.Point]) extends InputEvent

  def distance(s: Point, e: Point): Float = {
    val dx = (e.x - s.x).toDouble
    val dy = (e.y - s.y).toDouble
    Math.sqrt(dx * dx + dy * dy).toFloat
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
