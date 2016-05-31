package iliad
package x11

import iliad.kernel._
import iliad.kernel.utils.vectord._

import com.sun.jna._
import com.sun.jna.platform.unix._


import cats._
import cats.data._
import cats.implicits._

trait IliadBootstrap { app: IliadApp =>

  val viewDimensions: Vec2i

  val handler: X11EventHandler = new X11EventHandler(viewDimensions)

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
    try {
      x.XCreateSimpleWindow(
        d, root,
        xOffset, yOffset, viewDimensions(0), viewDimensions(1),
        borderWidth, border, background
      ).right
    } catch {
      case e: Error => e.left
    }    
}

  private def addDeletionProtocol(d: X11.Display, w: X11.Window): Error Xor Unit = {
    val protocol = x.XInternAtom(d, "WM_DELETE_WINDOW", false)
    x.XSetWMProtocols(d, w, Array(protocol), 1) match {
      case 0 => new Error("Unable to set deletion protocol").left
      case _ => ().right
    }
  }

  private val inputMask = new NativeLong(X11.ExposureMask | X11.ButtonPressMask)

  private def addInputDetection(d: X11.Display, w: X11.Window): Unit = {
    x.XSelectInput(d, w, inputMask)
  }

  private def showWindow(d: X11.Display, w: X11.Window): Unit = {
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

  private def handleEvents(d: X11.Display): Boolean = {
    val e = new X11.XEvent()
    x.XNextEvent(d, e)
    e.`type` match {
      case X11.ClientMessage =>
        println("Closing window")
        false
      case other => 
        handler.handleEvent(e)
        true
    }
  }

  def main(args: Array[String]): Unit = {
    createWindow match {
      case Xor.Right((d, w)) =>
        app.run()
        var shouldDraw = true
        while(shouldDraw) {
          shouldDraw = handleEvents(d)
        }
        destroyWindow(d, w)
      case Xor.Left(err) => throw err
    }
  }
}
