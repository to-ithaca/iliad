package iliad.x11

import iliad.kernel._
import iliad.kernel.utils.vectord._
import com.sun.jna.platform.unix.X11._
import iliad.kernel.platform.unix.X11

import com.sun.jna._


import cats._
import cats.data._
import cats.implicits._

trait IliadBootstrap extends X11EventHandler { app: IliadApp =>

  def width: Int
  def height: Int
  def viewDimensions: Vec2i = v"$width $height"

  private val x = iliad.kernel.platform.unix.X11.INSTANCE

  private def openDisplay(): Error Xor Display = Option(x.XOpenDisplay(null)) match {
    case Some(d) => d.right
    case scala.None => new Error("Failed to open display").left
  }

  private def rootWindow(d: Display): Error Xor Window = Option(x.XRootWindow(d, x.XDefaultScreen(d))) match {
    case Some(w) => w.right
    case scala.None => new Error("Failed to find root window").left
  }

  private def createSimpleWindow(d: Display, root: Window): Error Xor Window = {
    val xOffset = 0
    val yOffset = 0
    val borderWidth = 1
    val border = 1
    val background = 0
    println(s"Creating window with width ${viewDimensions(0)} height ${viewDimensions(1)}")
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

<<<<<<< Updated upstream
  private def addDeletionProtocol(d: X11.Display, w: X11.Window): Error Xor Unit = {
    println("Adding deletion protocol")
=======
  private def addDeletionProtocol(d: Display, w: Window): Error Xor Unit = {
    log.debug("Adding deletion protocol")
>>>>>>> Stashed changes
    val protocol = x.XInternAtom(d, "WM_DELETE_WINDOW", false)
    x.XSetWMProtocols(d, w, Array(protocol), 1) match {
      case 0 => new Error("Unable to set deletion protocol").left
      case _ => ().right
    }
  }

  private val inputMask = new NativeLong(ExposureMask | ButtonPressMask)

<<<<<<< Updated upstream
  private def addInputDetection(d: X11.Display, w: X11.Window): Unit = {
    println("Adding input detection")
    x.XSelectInput(d, w, inputMask)
  }

  private def showWindow(d: X11.Display, w: X11.Window): Unit = {
    println("Showing window")
=======
  private def addInputDetection(d: Display, w: Window): Unit = {
    log.debug("Adding input detection")
    x.XSelectInput(d, w, inputMask)
  }

  private def showWindow(d: Display, w: Window): Unit = {
    log.debug("Showing window")
>>>>>>> Stashed changes
    x.XMapWindow(d, w)
  }

  private def createWindow : Error Xor (Display, Window) = for {
    d <- openDisplay()
    r <- rootWindow(d)
    w <- createSimpleWindow(d, r)
    _ <- addDeletionProtocol(d, w)
    _ = addInputDetection(d, w)
    _ = showWindow(d, w)
  } yield (d, w)

  private def destroyWindow(d: Display, w: Window): Unit = {
    x.XDestroyWindow(d, w)
    x.XCloseDisplay(d)
  }

  private def handleAllEvents(d: Display): Boolean = {
    val e = new XEvent()
    x.XNextEvent(d, e)
    e.`type` match {
<<<<<<< Updated upstream
      case X11.ClientMessage =>
        println("Closing window")
=======
      case ClientMessage =>
        log.info("Closing window")
>>>>>>> Stashed changes
        false
      case other => 
        handleEvent(e)
        true
    }
  }

  def main(args: Array[String]): Unit = {
    createWindow match {
      case Xor.Right((d, w)) =>
        app.run()
        var shouldDraw = true
        while(shouldDraw) {
          shouldDraw = handleAllEvents(d)
        }
        destroyWindow(d, w)
<<<<<<< Updated upstream
      case Xor.Left(err) => throw err
=======
      case Xor.Left(err) => 
        log.error("Failed to create window - exiting application")
        throw err
>>>>>>> Stashed changes
    }
  }
}
