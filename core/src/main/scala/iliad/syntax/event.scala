package iliad
package syntax

import iliad.kernel.InputEvent
import iliad.syntax.vectord._

import spire.implicits._

trait PointInstances {
  implicit def toPointOps(p: InputEvent.Point): PointOps = new PointOps(p)
}

final class PointOps(p: InputEvent.Point) {
  def position: Vec2f = v"${p.x} ${p.y}"

  /** window coordinates range from -1f to 1f */
  def windowCoord: Vec2f = (position :* 2f) - 1f
}

import iliad.syntax.point._

trait TapInstances {
  implicit def toTapOps(t: InputEvent.Tap): TapOps = new TapOps(t)
}
final class TapOps(t: InputEvent.Tap) {
  def position: Vec2f = t.point.position

  /** window coordinates range from -1f to 1f */
  def windowCoord: Vec2f = t.point.windowCoord
}

import iliad.syntax.tap._

trait SwipeInstances {
  implicit def swipeOps(s: InputEvent.DragBecameSwipe): SwipeOps =
    new SwipeOps(s)
}

final class SwipeOps(s: InputEvent.DragBecameSwipe) {
  def duration: Long = s.end.at - s.start.at
  def direction: Vec2f = (s.end.position - s.start.position).normalize
  def isDown(acceptance: Float): Boolean = direction ⋅ v"0f -1f" > acceptance
  def isLeft(acceptance: Float): Boolean = direction ⋅ v"-1f 0f" > acceptance
  def isRight(acceptance: Float): Boolean = direction ⋅ v"1f 0f" > acceptance
  def isUp(acceptance: Float): Boolean = direction ⋅ v"0f 1f" > acceptance
}

trait DragContinuedInstances {
  implicit def dragContinuedOps(
      d: InputEvent.DragContinued): DragContinuedOps =
    new DragContinuedOps(d)
}

final class DragContinuedOps(d: InputEvent.DragContinued) {
  def distance: Float = (d.tail.last.position - d.start.position).norm
}
