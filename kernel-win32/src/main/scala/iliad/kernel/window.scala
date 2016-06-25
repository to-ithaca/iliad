package iliad
package kernel

import cats._
import cats.data._
import cats.implicits._

import scala.reflect._

import iliad.kernel.platform.win32.User32._

import com.sun.jna.platform.win32.WinDef._
import com.sun.jna.platform.win32._
import com.sun.jna.platform.win32.WinUser._
import com.sun.jna._

import Win32._

private[kernel] final class MessageCallback(delegate: Win32EventHandler)
    extends Callback {

  private def dequeueMsg: LRESULT = new LRESULT(0)

  def callback(hwnd: HWND,
               uMsg: Int,
               wParam: WPARAM,
               lParam: LPARAM): LRESULT =
    uMsg match {
      case WM_DESTROY =>
        user32.PostQuitMessage(0)
        dequeueMsg
      case event =>
        if (delegate.handleEvent(hwnd, event, wParam, lParam)) dequeueMsg
        else user32.DefWindowProc(hwnd, uMsg, wParam, lParam)
    }
}

object Win32 {

  private[kernel] val kernel32 = Kernel32.INSTANCE
  private[kernel] val user32 = iliad.kernel.platform.win32.User32.INSTANCE

  //contains the session information
  case class Error(code: Int, message: String)
  case class Session(cls: WNDCLASSEX, hwnd: HWND, hdc: HDC)
}

trait Win32GLDependencies extends GLDependencies {
  type NativeWindow = HWND
  type NativeDisplay = HDC
  type NativePixmap = HBITMAP
  type EGLDisplay = iliad.kernel.platform.win32.EGLDisplay
  type EGLConfig = iliad.kernel.platform.win32.EGLConfig
  type EGLSurface = iliad.kernel.platform.win32.EGLSurface
  type EGLContext = iliad.kernel.platform.win32.EGLContext

  val configClassTag: ClassTag[EGLConfig] = classTag[EGLConfig]
}

abstract class Win32Bootstrap(name: String, val width: Int, val height: Int)
    extends Win32EventHandler
    with IliadApp {

  def main(args: Array[String]): Unit = {
    val win32 = new Win32(name, width, height, this)
    win32.session match {
      case Xor.Right(session) =>
        //TODO: Not fond of the run method bootstrap here!
        run()
        win32.main.run(session)
      case Xor.Left(err) => System.exit(1)
    }
  }
}

class Win32(name: String, width: Int, height: Int, delegate: Win32EventHandler) {

  def checkError[A](action: String)(a: A): Error Xor A = {
    val err = kernel32.GetLastError()
    if (err == 0) a.right
    else Error(err, s"recieved $err while attemping to $action").left
  }

  def checkNull[A](action: String)(a: A): Error Xor A =
    if (a == null) Error(0, "$action unexpectedly returned null").left
    else a.right

  def register: Error Xor WNDCLASSEX = {
    val wc = new WNDCLASSEX
    val cn = new WString(name)
    val hInstance = kernel32.GetModuleHandle("")
    wc.hInstance = hInstance
    wc.lpszClassName = cn
    wc.style = CS_HREDRAW | CS_VREDRAW
    wc.hbrBackground = null
    wc.lpfnWndProc = new MessageCallback(delegate)
    user32.RegisterClassEx(wc)
    checkError("registering window class")(wc)
  }

  def createWindow(wndClass: WNDCLASSEX): Error Xor HWND = {
    val hwnd = user32.CreateWindowEx(
        WS_EX_LEFT,
        wndClass.lpszClassName,
        name,
        WS_OVERLAPPEDWINDOW,
        100,
        100,
        width,
        height,
        null,
        null,
        wndClass.hInstance,
        null
    )
    checkError("creating window")(hwnd).flatMap(checkNull("creating window"))
  }

  def displayConnection(hwnd: HWND): Error Xor HDC =
    checkNull("getting display connection")(user32.GetDC(hwnd))

  /** creates the window session */
  def session: Error Xor Session =
    for {
      cls <- register
      hwnd <- createWindow(cls)
      hdc <- displayConnection(hwnd)
    } yield Session(cls, hwnd, hdc)

  def show: Reader[Session, Unit] =
    Reader(s => user32.ShowWindow(s.hwnd, SW_SHOWNORMAL))
  def update: Reader[Session, Unit] = Reader(s => user32.UpdateWindow(s.hwnd))
  def foreground: Reader[Session, Unit] =
    Reader(s => user32.SetForegroundWindow(s.hwnd))

  /** starts the message loop, must be called on the main thread*/
  def loop: Reader[Session, Int] = Reader { s =>
    val msg = new WinUser.MSG
    @annotation.tailrec
    def go(ret: Int): Int = {
      if (ret > 0) {
        user32.TranslateMessage(msg)
        user32.DispatchMessage(msg)
        go(user32.GetMessage(msg, s.hwnd, 0, 0))
      } else ret
    }
    go(user32.GetMessage(msg, s.hwnd, 0, 0))
  }
  def close: Reader[Session, Boolean] = Reader { s =>
    user32.UnregisterClass(s.cls.lpszClassName, s.cls.hInstance)
  }

  def main: Reader[Session, Int] =
    for {
      _ <- show
      _ <- update
      _ <- foreground
      code <- loop
      _ <- close
    } yield code
}
