package iliad
package kernel

import com.sun.jna.platform.unix.X11._
import iliad.kernel.platform.unix.X11

import org.slf4j._

import EventHandler._
import InputEvent._

trait X11EventHandler extends EventHandler {

  private val log = LoggerFactory.getLogger(classOf[X11EventHandler])

  //TODO: This should be the meta information of the actual "platform itself"
  def width: Int
  def height: Int

  private var tapCallback: Callback[Tap] = EventHandler.zero

  def onTap(cb: Tap => Unit): Unit = tapCallback = cb

  def handleEvent(e: XEvent): Unit = e.`type` match {
    case ButtonPress =>
      log.debug("received tap")
      e.readField("xbutton")
      val xFraction =
        (e.xbutton.x - e.xbutton.x_root).toFloat / width.toFloat
      val yFraction =
        (e.xbutton.y - e.xbutton.y_root).toFloat / height.toFloat
      tapCallback(Tap(e.xbutton.time.longValue, xFraction, yFraction))
    case other =>
      log.warn("Unhandled event of type {}", other)
  }
}
