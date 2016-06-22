package iliad
package kernel

import iliad.kernel.platform.{EGL14Library => Lib}
import iliad.kernel.platform.win32._

import com.sun.jna.platform.win32.WinDef

@jna[EGL14Library]
trait EGL14Binding

object EGL14 extends Lib with EGL14Binding {

  type EGLConfig = iliad.kernel.platform.win32.EGLConfig
  type EGLSurface = iliad.kernel.platform.win32.EGLSurface
  type EGLDisplay = iliad.kernel.platform.win32.EGLDisplay
  type EGLContext = iliad.kernel.platform.win32.EGLContext
  type EGLClientBuffer = iliad.kernel.platform.win32.EGLClientBuffer

  val EGL_NO_CONTEXT = new EGLContext
  val EGL_NO_DISPLAY = new EGLDisplay
  val EGL_NO_SURFACE = new EGLSurface
  val EGL_DEFAULT_DISPLAY = new WinDef.HDC

  type EGLNativeDisplayType = WinDef.HDC
  type EGLNativeWindowType = WinDef.HWND
  type EGLNativePixmapType = WinDef.HBITMAP
}
