package iliad

#+x11

import iliad.gl.EGL14

import scala.reflect._
import scala.concurrent.duration._

import com.sun.jna.platform.unix.X11._
import com.sun.jna._

import cats.data._
import cats.implicits._

import fs2._
import fs2.async.mutable._
import fs2.util._

import com.typesafe.scalalogging._

object Session extends LazyLogging {

  val pageSize: Int = 1024

  val session: BlockingPromise[(EGL14.EGLNativeWindowType, EGL14.EGLNativeDisplayType)] =
    new BlockingPromise[(EGL14.EGLNativeWindowType, EGL14.EGLNativeDisplayType)]


  implicit val SS = Strategy.fromFixedDaemonPool(1, "vsync-thread")
  implicit val S = Scheduler.fromFixedDaemonPool(1)

  private val x = iliad.platform.unix.X11.INSTANCE

  def lockDisplay(d: Display) = x.XLockDisplay(d)
  def unlockDisplay(d: Display) = x.XUnlockDisplay(d)

 def vsync: Stream[Task, Long] = {
    implicit val SS: Strategy = Strategy.fromFixedDaemonPool(1, "vsync-thread")
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
