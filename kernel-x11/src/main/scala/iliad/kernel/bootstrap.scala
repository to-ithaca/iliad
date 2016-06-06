package iliad
package kernel

import iliad.kernel.utils.vectord._
import iliad.kernel.platform.unix.X11

import iliad.kernel.egl._
import iliad.kernel.gl.GL

import com.sun.jna.platform.unix.X11._
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

  private def addDeletionProtocol(d: Display, w: Window): Error Xor Unit = {
    log.debug("Adding deletion protocol")
    val protocol = x.XInternAtom(d, "WM_DELETE_WINDOW", false)
    x.XSetWMProtocols(d, w, Array(protocol), 1) match {
      case 0 => new Error("Unable to set deletion protocol").left
      case _ => ().right
    }
  }

  private val inputMask = new NativeLong(ExposureMask | ButtonPressMask)

  private def addInputDetection(d: Display, w: Window): Unit = {
    log.debug("Adding input detection")
    x.XSelectInput(d, w, inputMask)
  }

  private def showWindow(d: Display, w: Window): Unit = {
    log.debug("Showing window")
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
      case ClientMessage =>
        log.info("Closing window")
        false
      case other => 
        handleEvent(e)
        true
    }
  }

  //TODO: tidy this code up
  def setupEGL(d: Display, w: Window): EGL.Session[EGL14.EGLDisplay, EGL14.EGLConfig, EGL14.EGLSurface, EGL14.EGLContext] = {
    val eglRunner= EGL.debuggingLogging[EGL14.EGLNativeDisplayType, EGL14.EGLNativeWindowType, EGL14.EGLDisplay, EGL14.EGLConfig, EGL14.EGLSurface, EGL14.EGLContext]

    val writer = eglRunner.setupPrimaryContext(d, w, EGL14.EGL_NO_CONTEXT).run(EGL14).value
    writer.written.foreach(s => log.info("EGL log {}", s))
    writer.value match {
      case Xor.Left(err) =>
        log.error("Failed to create EGL session - exiting application {}", err)
        throw new Error(err)
      case Xor.Right(session) =>
        log.info("Successfully created EGL session {}", session)
        session
    }
  }

  def setupGL: Unit = {
    import iliad.kernel.gl._
    val gl = GL.debugAndLog(GL.DebuggerConfig(Set.empty), GL.LoggerConfig(Set.empty))
    val cmds = for {
      _ <- gl.clear(GL_COLOR_BUFFER_BIT)
      _ <- gl.clearColor(0f, 1f, 0f, 1f)
      _ <- gl.clear(GL_COLOR_BUFFER_BIT)
    } yield ()

    val writer = cmds.run(GLES30).value
    writer.written.foreach(s => log.info("GL log {}", s))

    writer.value match {
      case Xor.Left(err) =>
        log.error("Failed to create GL - exiting application {}", err)
        throw new IllegalStateException(err)
      case Xor.Right(_) =>
        log.info("Successfully called GL commands")
    }
  }

  def swapBuffers(display: EGL14.EGLDisplay, surface: EGL14.EGLSurface): Unit = {
    val eglRunner= EGL.debuggingLogging[EGL14.EGLNativeDisplayType, EGL14.EGLNativeWindowType, EGL14.EGLDisplay, EGL14.EGLConfig, EGL14.EGLSurface, EGL14.EGLContext]

    val writer = eglRunner.swapBuffers(display, surface).run(EGL14).value
    writer.written.foreach(s => log.info("EGL log {}", s))
    writer.value match {
      case Xor.Left(err) =>
        log.error("Failed to swap EGL buffers - exiting application {}", err)
        throw new Error(err)
      case Xor.Right(_) =>
        log.info("Successfully swapped EGL buffers")
    }

  }

  def main(args: Array[String]): Unit = {   
    createWindow match {
      case Xor.Right((d, w)) =>
        log.info("Created window")
        val session = setupEGL(d, w)
        setupGL
        swapBuffers(session.display, session.surface)
        app.run()
        var shouldDraw = true
        while(shouldDraw) {
          shouldDraw = handleAllEvents(d)
        }
        destroyWindow(d, w)
      case Xor.Left(err) =>
        log.error("Failed to create window - exiting application")
        throw err
    }

  }
}
