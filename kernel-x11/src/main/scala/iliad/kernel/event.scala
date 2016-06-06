package iliad
package kernel

import iliad.kernel.vectord._

import com.sun.jna.platform.unix.X11._
import iliad.kernel.platform.unix.X11

import org.slf4j._

import EventHandler._
import InputEvent._

trait X11EventHandler extends EventHandler {

  private val log = LoggerFactory.getLogger(classOf[X11EventHandler])

  //TODO: This should be the meta information of the actual "platform itself"
  def viewDimensions: Vec2i

  private var tapCallback: Callback[Tap] = EventHandler.zero

  def onTap(cb: Tap => Unit) = tapCallback = cb

  def handleEvent(e: XEvent) = e.`type` match {
    case ButtonPress =>
      log.debug("received tap")
      e.readField("xbutton")
      val xFraction = (e.xbutton.x - e.xbutton.x_root).toFloat / viewDimensions(0).toFloat
      val yFraction = (e.xbutton.y - e.xbutton.y_root).toFloat / viewDimensions(1).toFloat
      tapCallback(Tap(e.xbutton.time.longValue, v"$xFraction $yFraction"))
    case other =>
      log.warn("Unhandled event of type {}", other)
  }
}
