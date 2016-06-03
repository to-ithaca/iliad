package iliad
package kernel

import iliad.kernel.utils.vectord._

import com.sun.jna.platform.win32.User32._
import com.sun.jna.platform.win32.WinDef._

import iliad.kernel.platform.win32.User32._
import iliad.kernel.platform.win32.User32.Macros._

import org.slf4j._

import EventHandler._
import InputEvent._

trait Win32EventHandler extends EventHandler {

  private val log = LoggerFactory.getLogger(classOf[Win32EventHandler])

  //TODO: This should be the meta information of the actual "platform itself"
  def viewDimensions: Vec2i

  private var tapCallback: Callback[Tap] = EventHandler.zero

  def onTap(cb: Tap => Unit) = tapCallback = cb

  def handleEvent(hwnd: HWND, uMsg: Int, wParam: WPARAM, lParam: LPARAM): Boolean = uMsg match {
    case 1 =>
      log.debug("received tap")
      val xFraction = Macros.GET_X_LPARAM(lParam).toFloat / viewDimensions(0).toFloat
      val yFraction = GET_Y_LPARAM(lParam).toFloat / viewDimensions(1).toFloat
      //TODO: windows must have a better way of getting the time
      tapCallback(Tap(System.currentTimeMillis(), v"$xFraction $yFraction"))
      true
    case _ => false
  }
}
