package iliad
package kernel

import fs2._
import fs2.util.Task

import cats._
import cats.data._
import cats.implicits._

import com.sun.jna.platform.win32.WinDef._
import com.sun.jna.platform.win32._
import com.sun.jna.platform.win32.WinUser._
import com.sun.jna._

import Win32._

private[kernel] final class MessageCallback extends Callback {

  private def dequeueMsg: LRESULT = new LRESULT(0)

  def callback(hwnd: HWND, uMsg: Int, wParam: WPARAM, lParam: LPARAM): LRESULT = uMsg match {
    case WM_CREATE =>
      dequeueMsg
    case WM_DESTROY =>
      user32.PostQuitMessage(0)
      dequeueMsg
    case other => 
      user32.DefWindowProc(hwnd, uMsg, wParam, lParam)
  }
}

object Win32 {
  
  private[kernel] val kernel32 = Kernel32.INSTANCE
  private[kernel] val user32 = User32.INSTANCE

  //contains the session information
  case class Error(code: Int, message: String)
  case class Session(cls: WNDCLASSEX, hwnd: HWND, hdc: HDC)

  val WS_EX_LEFT = 0x00000000
  val WS_SYSMENU = 0x00080000L
  val CW_USEDEFAULT = 0
}

class Win32(name: String, width: Int, height: Int) {

  def checkError[A](action: String)(a: A): Error Xor A = {
    val err = kernel32.GetLastError()
    if(err == 0) a.right else Error(err, s"recieved $err while attemping to $action").left
  }

  def checkNull[A](action: String)(a: A): Error Xor A = if(a == null) Error(0, "$action unexpectedly returned null").left else a.right

  def register: Error Xor WNDCLASSEX = {
    val wc = new WNDCLASSEX
    wc.hInstance = kernel32.GetModuleHandle("")
    val cn = new WString(name)
    wc.lpszClassName = cn
    wc.lpfnWndProc = new MessageCallback
    user32.RegisterClassEx(wc)
    checkError("registering window class")(wc)
  }

  def createWindow(wndClass: WNDCLASSEX): Error Xor HWND = {
    val hwnd = user32.CreateWindowEx(
      WS_EX_LEFT,
      wndClass.lpszClassName,
      name,
      WS_BORDER | WS_CAPTION,
      CW_USEDEFAULT,
      CW_USEDEFAULT,
      width,
      height,
      null,
      null,
      wndClass.hInstance,
      null
    )
    checkError("creating window")(hwnd).flatMap(checkNull("creating window"))
  }

  /** starts the dispatch message loop on a different thread*/
  def dispatchMessages(hwnd: HWND): Task[Unit] = Task {
    val msg = new WinUser.MSG
    //While messages are still being put on the queue, the loop needs to continue
    while(user32.GetMessage(msg, hwnd, 0, 0) != 0) {
      user32.TranslateMessage(msg)
      user32.DispatchMessage(msg)
    }
  }(Strategy.fromFixedDaemonPool(1))

  def displayConnection(hwnd: HWND): Error Xor HDC = checkNull("getting display connection")(user32.GetDC(hwnd))

  /** creates the window session */
  def createSession(implicit s: Strategy): Task[Error Xor Session] = Task {
    for {
      cls <- register
      hwnd <- createWindow(cls)
      hdc <- displayConnection(hwnd)
    } yield Session(cls, hwnd, hdc)
  }

  def show: Reader[Session, Unit] = Reader(s => user32.ShowWindow(s.hwnd, SW_SHOWNORMAL))

  def session(implicit strategy: Strategy): Task[Session] = for {
    cs <- Task.start { createSession }
    s <- cs 
    session <- s.map(Task.now).leftMap(err => Task.fail(new IllegalStateException(err.toString)): Task[Session]).merge
    loop <- Task.start { dispatchMessages(session.hwnd) }
  } yield session

}
