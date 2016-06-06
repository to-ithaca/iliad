package iliad
package kernel

import iliad.kernel.platform.{EGL14Library => Lib}
import iliad.kernel.platform.unix._
import com.sun.jna.platform.unix.X11

@jna[EGL14Library] trait EGL14Binding

object EGL14 extends Lib with EGL14Binding {

  type EGLConfig = iliad.kernel.platform.unix.EGLConfig
  type EGLSurface = iliad.kernel.platform.unix.EGLSurface
  type EGLDisplay = iliad.kernel.platform.unix.EGLDisplay
  type EGLContext = iliad.kernel.platform.unix.EGLContext
  type EGLClientBuffer = iliad.kernel.platform.unix.EGLClientBuffer

  val EGL_NO_CONTEXT = new EGLContext
  val EGL_NO_DISPLAY = new EGLDisplay
  val EGL_NO_SURFACE = new EGLSurface
  val EGL_DEFAULT_DISPLAY = new X11.Display

  type EGLNativeDisplayType = X11.Display
  type EGLNativeWindowType = X11.Window
  type EGLNativePixmapType = X11.Pixmap
}
