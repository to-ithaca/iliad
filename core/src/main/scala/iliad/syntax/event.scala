package iliad
package syntax

import iliad.kernel.InputEvent
import iliad.syntax.vectord._

import spire.implicits._

trait TapInstances {
  implicit def toTapOps(t: InputEvent.Tap): TapOps = new TapOps(t)
}
final class TapOps(t: InputEvent.Tap) {
  def position: Vec2f = v"${t.x} ${t.y}"

  /** window coordinates range from -1f to 1f */
  def windowCoord: Vec2f = (position :* 2f) - 1f
}

import iliad.syntax.tap._

trait SwipeInstances {
  implicit def swipeOps(s: InputEvent.Swipe): SwipeOps = new SwipeOps(s)
}

final class SwipeOps(s: InputEvent.Swipe) {
  def duration: Long = s.end.at - s.start.at
  def direction: Vec2f = (s.end.position - s.start.position).normalize
  def isDown(acceptance: Float): Boolean = direction ⋅ v"0f -1f" > acceptance
  def isLeft(acceptance: Float): Boolean = direction ⋅ v"-1f 0f" > acceptance
  def isRight(acceptance: Float): Boolean = direction ⋅ v"1f 0f" > acceptance
  def isUp(acceptance: Float): Boolean = direction ⋅ v"0f 1f" > acceptance
}
