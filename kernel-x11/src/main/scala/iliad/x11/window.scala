package iliad
package x11

import com.sun.jna._
import com.sun.jna.platform.unix._


import cats._
import cats.data._
import cats.implicits._

trait MainWindow {

  /* **/
  def width: Int
  def height: Int 

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
        xOffset, yOffset, width, height,
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

  private val inputMask = new NativeLong(X11.ExposureMask | X11.ButtonPressMask | X11.ButtonReleaseMask | X11.Button1MotionMask)

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

  def handleEvents(d: X11.Display): Boolean = {
    val e = new X11.XEvent()
    x.XNextEvent(d, e)
    e.`type` match {
      case X11.ClientMessage =>
        println("Closing window")
        false
      case other => 
        println(s"Unhandled event of type $other")
        true
    }
  }

  def main(args: Array[String]): Unit = {
    createWindow match {
      case Xor.Right((d, w)) => 
        var shouldDraw = true
        while(shouldDraw) {
          shouldDraw = handleEvents(d)
        }
        destroyWindow(d, w)
      case Xor.Left(err) => throw err
    }
  }
}


trait EventReceiver {

  def receiveEvent(e: X11.XEvent) = {
    e.`type` match {
      case X11.ButtonPress =>
        e.readField("xbutton")
      case X11.ButtonRelease =>
        e.readField("xbutton")
    }
  }
}
