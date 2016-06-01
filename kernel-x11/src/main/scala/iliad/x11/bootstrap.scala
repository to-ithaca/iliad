package iliad
package x11

import iliad.kernel._
import iliad.kernel.utils.vectord._
import iliad.kernel.platform.unix._

import com.sun.jna._


import cats._
import cats.data._
import cats.implicits._

import org.slf4j._

trait X11Bootstrap extends X11EventHandler { app: IliadApp =>

  private val log = LoggerFactory.getLogger(classOf[X11Bootstrap])

  def width: Int
  def height: Int
  def viewDimensions: Vec2i = v"$width $height"

  private val x = X11.INSTANCE

  private def openDisplay(): Error Xor X11.Display = Option(x.XOpenDisplay(null)) match {
    case Some(d) => d.right
    case None => new Error("Failed to open display").left
  }

  private def rootWindow(d: X11.Display): Error Xor X11.Window = Option(x.XRootWindow(d, x.XDefaultScreen(d))) match {
    case Some(w) => w.right
    case None => new Error("Failed to find root window").left
  }

  private def createSimpleWindow(d: X11.Display, root: X11.Window): Error Xor X11.Window = {
    val xOffset = 0
    val yOffset = 0
    val borderWidth = 1
    val border = 1
    val background = 0
    log.debug("Creating window with width {} height {}", viewDimensions(0), viewDimensions(1))
    try {
      x.XCreateSimpleWindow(
        d, root,
        xOffset, yOffset, viewDimensions(0), viewDimensions(1),
        borderWidth, border, background
      ).right
    } catch {
      case e: Error =>
        new Error(s"Failed to create window: \n ${e.getMessage}").left
    }    
}

  private def addDeletionProtocol(d: X11.Display, w: X11.Window): Error Xor Unit = {
    log.debug("Adding deletion protocol")
    val protocol = x.XInternAtom(d, "WM_DELETE_WINDOW", false)
    x.XSetWMProtocols(d, w, Array(protocol), 1) match {
      case 0 => new Error("Unable to set deletion protocol").left
      case _ => ().right
    }
  }

  private val inputMask = new NativeLong(X11.ExposureMask | X11.ButtonPressMask)

  private def addInputDetection(d: X11.Display, w: X11.Window): Unit = {
    log.debug("Adding input detection")
    x.XSelectInput(d, w, inputMask)
  }

  private def showWindow(d: X11.Display, w: X11.Window): Unit = {
    log.debug("Showing window")
    x.XMapWindow(d, w)
  }

  private def createWindow : Error Xor (X11.Display, X11.Window) = for {
    d <- openDisplay()
    r <- rootWindow(d)
    w <- createSimpleWindow(d, r)
    _ <- addDeletionProtocol(d, w)
    _ = addInputDetection(d, w)
    _ = showWindow(d, w)
  } yield (d, w)

  private def destroyWindow(d: X11.Display, w: X11.Window): Unit = {
    x.XDestroyWindow(d, w)
    x.XCloseDisplay(d)
  }

  private def handleAllEvents(d: X11.Display): Boolean = {
    val e = new X11.XEvent()
    x.XNextEvent(d, e)
    e.`type` match {
      case X11.ClientMessage =>
        log.info("Closing window")
        false
      case other => 
        handleEvent(e)
        true
    }
  }

  def main(args: Array[String]): Unit = {
    createWindow match {
      case Xor.Right((d, w)) =>
        log.info("Created window")
        app.run()
        var shouldDraw = true
        while(shouldDraw) {
          shouldDraw = handleAllEvents(d)
        }
        destroyWindow(d, w)
      case Xor.Left(err) => 
        log.error("Failed to create window - exiting application")
        throw err
        System.exit(1)
    }
  }
}
