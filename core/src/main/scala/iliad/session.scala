package iliad

import iliad.gl.EGL14

import scala.reflect._
import scala.concurrent.duration._

import cats._
import cats.data._
import cats.implicits._

import fs2._
import fs2.async.mutable._
import fs2.util._

import com.typesafe.scalalogging._

#+x11
import com.sun.jna.platform.unix.X11._
import com.sun.jna._

object Session extends LazyLogging {

  val pageSize: Int = 1024

  val session: BlockingPromise[(EGL14.EGLNativeWindowType, EGL14.EGLNativeDisplayType)] =
    new BlockingPromise[(EGL14.EGLNativeWindowType, EGL14.EGLNativeDisplayType)]

  private val x = iliad.platform.unix.X11.INSTANCE

  def lockDisplay(d: Display) = x.XLockDisplay(d)
  def unlockDisplay(d: Display) = x.XUnlockDisplay(d)

 def vsync: Stream[Task, Long] = {
   implicit val SS: Strategy = Strategy.fromFixedDaemonPool(1, "vsync-thread")
   implicit val S = Scheduler.fromFixedDaemonPool(1)
    val start = System.currentTimeMillis
    time.awakeEvery[Task]((1.0 / 30.0) seconds).map(_.toMillis + start)
  }

  private def initThreads(): Error Xor Unit = {
    val code = x.XInitThreads()
    if (code == 0) new Error("Failed to multi thread X11").left
    else ().right
  }

  private def openDisplay(): Error Xor Display =
    Option(x.XOpenDisplay(null)) match {
      case Some(d) => d.right
      case scala.None => new Error("Failed to open display").left
    }

  private def rootWindow(d: Display): Error Xor Window =
    Option(x.XRootWindow(d, x.XDefaultScreen(d))) match {
      case Some(w) => w.right
      case scala.None => new Error("Failed to find root window").left
    }

  private def createSimpleWindow(d: Display, root: Window, width: Int, height: Int): Error Xor Window = {
    val xOffset = 0
    val yOffset = 0
    val borderWidth = 1
    val border = 1
    val background = 0
    logger.debug(s"Creating window with width $width height $height")
    try {
      x.XCreateSimpleWindow(
            d,
            root,
            xOffset,
            yOffset,
            width,
            height,
            borderWidth,
            border,
            background
        )
        .right
    } catch {
      case e: Error =>
        new Error(s"Failed to create window: \n ${e.getMessage}").left
    }
  }

  private def addDeletionProtocol(d: Display, w: Window): Error Xor Unit = {
    logger.debug("Adding deletion protocol")
    val protocol = x.XInternAtom(d, "WM_DELETE_WINDOW", false)
    x.XSetWMProtocols(d, w, Array(protocol), 1) match {
      case 0 => new Error("Unable to set deletion protocol").left
      case _ => ().right
    }
  }

  private val inputMask = new NativeLong(
      ExposureMask | ButtonPressMask | ButtonReleaseMask | Button1MotionMask | LeaveWindowMask)

  private def addInputDetection(d: Display, w: Window): Unit = {
    logger.debug("Adding input detection")
    x.XSelectInput(d, w, inputMask)
  }

  private def showWindow(d: Display, w: Window): Unit = {
    logger.debug("Showing window")
    x.XMapWindow(d, w)
  }

  private def createWindow(width: Int, height: Int): Error Xor (Display, Window) =
    for {
      _ <- initThreads()
      d <- openDisplay()
      r <- rootWindow(d)
      w <- createSimpleWindow(d, r, width, height)
      _ <- addDeletionProtocol(d, w)
      _ = addInputDetection(d, w)
      _ = showWindow(d, w)
    } yield (d, w)

  private def destroyWindow(d: Display, w: Window): Unit = {
    logger.info("closing window")
    x.XDestroyWindow(d, w)
    x.XCloseDisplay(d)
  }

  private def handleEvents(d: Display, width: Int, height: Int): Unit = {
    var e = new XEvent()
    while (x.XCheckMaskEvent(d, inputMask, e)) {
      logger.debug("received XEvent")
      EventHandler.handleEvent(e, width, height)
      e = new XEvent()
    }
  }

  private def shouldClose(d: Display): Boolean = {
    x.XCheckTypedEvent(d, ClientMessage, new XEvent())
  }

  def start(width: Int, height: Int) = createWindow(width, height) match {
      case Xor.Right((d, w)) =>
        logger.info("Created window")
        session.set((w, d))

        var shouldDraw = true
        while (shouldDraw) {
          //only handleEvents every 70ms so EGL can have display
          Thread.sleep(70)
          x.XLockDisplay(d)
          handleEvents(d, width, height)
          shouldDraw = !shouldClose(d)
          x.XUnlockDisplay(d)
        }
        destroyWindow(d, w)
      case Xor.Left(err) =>
        session.set(err)
    }
}
#-x11

#+win32
import iliad.platform.win32.User32._

import com.sun.jna.platform.win32.WinDef._
import com.sun.jna.platform.win32._
import com.sun.jna.platform.win32.WinUser._
import com.sun.jna._

object Win32 {

  val kernel32 = Kernel32.INSTANCE
  val user32 = iliad.platform.win32.User32.INSTANCE

  //contains the session information
  case class Error(code: Int, message: String)
  case class Session(cls: WNDCLASSEX, hwnd: HWND, hdc: HDC)
}

object Session {
  val pageSize: Int = 1024

  val session: BlockingPromise[(EGL14.EGLNativeWindowType, EGL14.EGLNativeDisplayType)] =
    new BlockingPromise[(EGL14.EGLNativeWindowType, EGL14.EGLNativeDisplayType)]

  def vsync: Stream[Task, Long] = {
    implicit val SS: Strategy = Strategy.fromFixedDaemonPool(1, "vsync-thread")
    implicit val S = Scheduler.fromFixedDaemonPool(1)
    val start = System.currentTimeMillis
    time.awakeEvery[Task]((1.0 / 30.0) seconds).map(_.toMillis + start)
  }

  def start(name: String, width: Int, height: Int): Unit = {
    val win32 = new Win32(name, width, height)
    win32.session match {
      case Xor.Right(s) =>
        session.set((s.hwnd, s.hdc))
        win32.show.run(s)
        win32.main.run(s)
      case Xor.Left(err) =>
        session.set(new IllegalStateException(err.toString))
        System.exit(1)
    }
  }
}

private final class MessageCallback(width: Int, height: Int) extends Callback {

  private def dequeueMsg: LRESULT = new LRESULT(0)

  def callback(hwnd: HWND,
               uMsg: Int,
               wParam: WPARAM,
               lParam: LPARAM): LRESULT =
    uMsg match {
      case WM_DESTROY =>
        Win32.user32.PostQuitMessage(0)
        dequeueMsg
      case event =>
        if (EventHandler.handleEvent(hwnd, event, wParam, lParam, width, height)) dequeueMsg
        else Win32.user32.DefWindowProc(hwnd, uMsg, wParam, lParam)
    }
}

class Win32(name: String, width: Int, height: Int) {

  def checkError[A](action: String)(a: A): Win32.Error Xor A = {
    val err = Win32.kernel32.GetLastError()
    if (err == 0) a.right
    else Win32.Error(err, s"recieved $err while attemping to $action").left
  }

  def checkNull[A](action: String)(a: A): Win32.Error Xor A =
    if (a == null) Win32.Error(0, s"$action unexpectedly returned null").left
    else a.right

  def register: Win32.Error Xor WNDCLASSEX = {
    val wc = new WNDCLASSEX
    val cn = new WString(name)
    val hInstance = Win32.kernel32.GetModuleHandle("")
    wc.hInstance = hInstance
    wc.lpszClassName = cn
    wc.style = CS_HREDRAW | CS_VREDRAW | CS_OWNDC
    wc.lpfnWndProc = new MessageCallback(width, height)
    Win32.user32.RegisterClassEx(wc)
    checkError("registering window class")(wc)
  }

  def createWindow(wndClass: WNDCLASSEX): Win32.Error Xor HWND = {
    val hwnd = Win32.user32.CreateWindowEx(
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

  def displayConnection(hwnd: HWND): Win32.Error Xor HDC =
    checkNull("getting display connection")(Win32.user32.GetDC(hwnd))

  /** creates the window session */
  def session: Win32.Error Xor Win32.Session =
    for {
      cls <- register
      hwnd <- createWindow(cls)
      hdc <- displayConnection(hwnd)
    } yield Win32.Session(cls, hwnd, hdc)

  def show: Reader[Win32.Session, Unit] =
    Reader(s => Win32.user32.ShowWindow(s.hwnd, SW_SHOWNORMAL))
  def update: Reader[Win32.Session, Unit] = Reader(s => Win32.user32.UpdateWindow(s.hwnd))
  def foreground: Reader[Win32.Session, Unit] =
    Reader(s => Win32.user32.SetForegroundWindow(s.hwnd))

  /** starts the message loop, must be called on the main thread*/
  def loop: Reader[Win32.Session, Int] = Reader { s =>
    val msg = new WinUser.MSG
    @annotation.tailrec
    def go(ret: Int): Int = {
      if (ret > 0) {
        Win32.user32.TranslateMessage(msg)
        Win32.user32.DispatchMessage(msg)
        go(Win32.user32.GetMessage(msg, s.hwnd, 0, 0))
      } else ret
    }
    go(Win32.user32.GetMessage(msg, s.hwnd, 0, 0))
  }
  def close: Reader[Win32.Session, Boolean] = Reader { s =>
    Win32.user32.UnregisterClass(s.cls.lpszClassName, s.cls.hInstance)
  }

  def main: Reader[Win32.Session, Int] =
    for {
      _ <- update
      _ <- foreground
      code <- loop
      _ <- close
    } yield code
}
#-win32

#+android
import android.app.Activity
import android.app.Fragment
import android.os.Bundle
import android.util.Log
import android.view._
import android.graphics.Point
import android.support.v4.view.GestureDetectorCompat
import android.content.Context

object Session {
  val pageSize: Int = 1024

  private implicit val S: Strategy = Strategy.fromFixedDaemonPool(1, "vsync")

  private val _choreographer = Choreographer.getInstance

  private def vsync(s: Signal[Task, Long]): Unit =
    _choreographer.postFrameCallback {
      new Choreographer.FrameCallback() {
        def doFrame(frameTimeNanos: Long): Unit = {
          s.set(frameTimeNanos).unsafeAttemptRun.toXor match {
            case Xor.Right(_) =>
            case Xor.Left(err) =>
              throw new Error(
                s"Failed to set vsync signal at time $frameTimeNanos. ${err.getMessage}"
              )
          }
          _choreographer.postFrameCallback(this)
        }
      }
    }

  def vsync: Stream[Task, Long] =
    Stream.eval(async.signalOf[Task, Long](0L)).flatMap { s =>
      vsync(s)
      s.discrete
    }

  val session: BlockingPromise[(EGL14.EGLNativeWindowType, EGL14.EGLNativeDisplayType)] =
    new BlockingPromise[(EGL14.EGLNativeWindowType, EGL14.EGLNativeDisplayType)]
}

trait AndroidBootstrap extends Activity with AndroidEventHandler
    with LazyLogging {

  def mainXML: Int
  def fragmentXML: Int
  def subFragment: Int
  def subView: Int

  var detector: GestureDetectorCompat = _

  var _width: Int = _
  var _height: Int = _
  def width: Int = _width
  def height: Int = _height

  override def onCreate(savedInstanceState: Bundle) {
    logger.info("AndroidBootstrap.onCreate: creating activity")
    super.onCreate(savedInstanceState)
    setContentView(mainXML)

    val wm: WindowManager = getSystemService(Context.WINDOW_SERVICE).asInstanceOf[WindowManager];
    val display = wm.getDefaultDisplay();
    val size = new Point()
    display.getSize(size)
    _width = size.x
    _height = size.y

    detector = new GestureDetectorCompat(this, this)
    detector.setOnDoubleTapListener(this)

    if (savedInstanceState == null) {
      val transaction = getFragmentManager.beginTransaction()
      val fragment = new AndroidFragment(subView, fragmentXML)
      transaction.replace(subFragment, fragment)
      transaction.commit()
    }
  }

  override def onTouchEvent(event: MotionEvent): Boolean = {
    detector.onTouchEvent(event)      
    super.onTouchEvent(event)
  }

  override def onCreateOptionsMenu(menu: Menu): Boolean = true
  override def onPrepareOptionsMenu(menu: Menu): Boolean = true
  override def onOptionsItemSelected(item: MenuItem): Boolean = true
}

final class AndroidFragment(subView: Int, fragmentXML: Int) 
    extends Fragment with LazyLogging {

  override def onCreateView(inflater: LayoutInflater ,container: ViewGroup, savedInstanceState: Bundle) = {
    logger.info("AndroidFragment.onCreateView: creating view")
    inflater.inflate(fragmentXML, container, false)
  }

  override def onViewCreated(v: View, savedInstanceState: Bundle): Unit = {
    logger.info("AndroidFragment.onViewCreated: created view. Running app")
    val view = v.findViewById(subView).asInstanceOf[SurfaceView]
    Session.session.set((view.getHolder(), EGL14.EGL_DEFAULT_DISPLAY))
  }
}
#-android
