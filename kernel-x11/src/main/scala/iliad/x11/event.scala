package iliad
package x11

import iliad.kernel._
import iliad.kernel.utils.vectord._

import iliad.kernel.platform.unix.X11

import org.slf4j._


trait X11EventHandler extends EventHandler {

  private val log = LoggerFactory.getLogger(classOf[X11EventHandler])

  def viewDimensions: Vec2i

  var tapCallback: TouchEvent.Tap => Unit = (_) => ()

  def registerTapCallback(cb: TouchEvent.Tap => Unit) = tapCallback = cb

  def handleEvent(e: X11.XEvent) = e.`type` match {
    case X11.ButtonPress =>
      log.debug("Recognised tap")
      e.readField("xbutton")
      val xFraction = (e.xbutton.x - e.xbutton.x_root).toFloat / viewDimensions(0).toFloat
      val yFraction = (e.xbutton.y - e.xbutton.y_root).toFloat / viewDimensions(1).toFloat
      tapCallback(TouchEvent.Tap(v"$xFraction $yFraction"))
    case other =>
      log.warn("Unhandled event of type {}", other)
  }
}
