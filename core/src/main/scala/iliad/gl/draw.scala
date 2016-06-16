package iliad
package gl

import cats._
import cats.free._

object Draw {
  type DSL[A] = Free[Draw, A]
}

sealed trait Draw[A]

case class DrawModel(f: Framebuffer, p: Program.Unlinked, m: Model)
    extends Draw[Unit]

case class ClearFrame(bitMask: ChannelBitMask) extends Draw[Unit]
case object ClearCache extends Draw[Unit]
