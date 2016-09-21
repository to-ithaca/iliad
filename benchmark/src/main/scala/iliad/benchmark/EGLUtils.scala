package iliad
package benchmark

import iliad.gl._

import cats._
import cats.data._
import cats.implicits._

object EGLUtils {
  case class Session(surface: EGL14.EGLSurface, display: EGL14.EGLDisplay, context: EGL14.EGLContext)

  private val configAttrs: Attributes[ConfigAttrib, ConfigAttribValue] = 
    Attributes(
      ConfigAttrib(EGL_LEVEL, 0),
      ConfigAttrib(EGL_SURFACE_TYPE, EGL_PBUFFER_BIT),
      ConfigAttrib(EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT.value),
      ConfigAttrib(EGL_CONFORMANT, EGL_OPENGL_ES3_BIT),
      ConfigAttrib(EGL_BLUE_SIZE, 8),
      ConfigAttrib(EGL_GREEN_SIZE, 8),
      ConfigAttrib(EGL_RED_SIZE, 8),
      ConfigAttrib(EGL_ALPHA_SIZE, 8),
      ConfigAttrib(EGL_BUFFER_SIZE, 32),
      ConfigAttrib(EGL_DEPTH_SIZE, 16)
    )

  private val pbufferAttrs: Attributes[PBufferAttrib, PBufferAttribValue] = Attributes(
    PBufferAttrib(EGL_WIDTH, 1),
    PBufferAttrib(EGL_HEIGHT, 1),
    PBufferAttrib(EGL_TEXTURE_FORMAT, EGL_TEXTURE_RGBA),
    PBufferAttrib(EGL_TEXTURE_TARGET, EGL_TEXTURE_2D)
  )

  private val contextAttrs: Attributes[ContextAttrib, ContextAttribValue] =
    Attributes(ContextAttrib(EGL_CONTEXT_CLIENT_VERSION, 3))

  def session(): EGLError Xor Session = {
    val prg = (for {
      dpy <- XorT(EGL.initialise(EGL14.EGL_DEFAULT_DISPLAY))
      cfg <- XorT(EGL.config(dpy, configAttrs).map(_.toRightXor(EGLConfigError(configAttrs)))).leftWiden[EGLError]
      sfc <- XorT(EGL.pbufferSurface(dpy, cfg, pbufferAttrs)).leftWiden[EGLError]
      ctx <- XorT(EGL.context(dpy, cfg, contextAttrs)).leftWiden[EGLError]
      _ <- XorT(EGL.makeCurrent(dpy, sfc, sfc, ctx)).leftWiden[EGLError]
      _ <- XorT(EGL.swapInterval(dpy, 0)).leftWiden[EGLError]
    } yield Session(sfc, dpy, ctx)).value

    prg.foldMap(EGL.logInterpreter).run(EGL14).flatMap(identity)
  }

  def destroy(s: Session): EGLError Xor Unit = {
    val prg = (for {
      _ <- XorT(EGL.destroySurface(s.display, s.surface)).leftWiden[EGLError]
      _ <- XorT(EGL.destroyContext(s.display, s.context)).leftWiden[EGLError]
    } yield()).value
    prg.foldMap(EGL.logInterpreter).run(EGL14).flatMap(identity)
  }

  def swapBuffers(s: Session): EGLError Xor Unit = {
    val prg = EGL.swapBuffers(s.display, s.surface).map(_.leftWiden[EGLError])
    prg.foldMap(EGL.logInterpreter).run(EGL14).flatMap(identity).void
  }
}
