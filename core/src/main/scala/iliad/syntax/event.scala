package iliad
package syntax

import iliad.kernel.InputEvent
import iliad.syntax.vectord._

trait TapInstances {
  implicit def toTapOps(t: InputEvent.Tap): TapOps = new TapOps(t)
}
final class TapOps(t: InputEvent.Tap) {
  def position: Vec2f = v"${t.x} ${t.y}"
}
