package iliad
package gl

import java.nio.IntBuffer

/*
object EGL14Library {

  /** Promotes path dependent types to higher kinded types */
  type Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx] = EGL14Library {
    type EGLNativeDisplayType = NDisp
    type EGLNativeWindowType = NWin
    type EGLDisplay = Disp
    type EGLConfig = Cfg
    type EGLSurface = Sfc
    type EGLContext = Ctx
  }
}*/

trait EGL14Library {

  type EGLDisplay
  type EGLSurface
  type EGLConfig
  type EGLContext
  type EGLClientBuffer

  type EGLNativeDisplayType
  type EGLNativeWindowType
  type EGLNativePixmapType
 
  def EGL_DEFAULT_DISPLAY: EGLNativeDisplayType
  def EGL_NO_DISPLAY: EGLDisplay
  def EGL_NO_CONTEXT: EGLContext
  def EGL_NO_SURFACE: EGLSurface

  def eglChooseConfig(dpy: EGLDisplay,
                      attrib_list: Array[Int],
                      configs: Array[EGLConfig],
                      config_size: Int,
                      num_config: IntBuffer): Boolean
  def eglCopyBuffers(dpy: EGLDisplay,
                     surface: EGLSurface,
                     pixmap: EGLNativePixmapType): Boolean
  def eglCreateContext(dpy: EGLDisplay,
                       config: EGLConfig,
                       share_context: EGLContext,
                       attrib_list: Array[Int]): EGLContext
  def eglCreatePbufferSurface(dpy: EGLDisplay,
                              config: EGLConfig,
                              attrib_list: Array[Int]): EGLSurface
  def eglCreatePixmapSurface(dpy: EGLDisplay,
                             config: EGLConfig,
                             pixmap: EGLNativePixmapType,
                             attrib_list: Array[Int]): EGLSurface
  def eglCreateWindowSurface(dpy: EGLDisplay,
                             config: EGLConfig,
                             win: EGLNativeWindowType,
                             attrib_list: Array[Int]): EGLSurface
  def eglDestroyContext(dpy: EGLDisplay, ctx: EGLContext): Boolean
  def eglDestroySurface(dpy: EGLDisplay, surface: EGLSurface): Boolean
  def eglGetConfigAttrib(dpy: EGLDisplay,
                         config: EGLConfig,
                         attribute: Int,
                         value: IntBuffer): Boolean
  def eglGetConfigs(dpy: EGLDisplay,
                    configs: Array[EGLConfig],
                    config_size: Int,
                    num_config: IntBuffer): Boolean
  def eglGetCurrentDisplay(): EGLDisplay
  def eglGetCurrentSurface(readdraw: Int): EGLSurface
  def eglGetDisplay(display_id: EGLNativeDisplayType): EGLDisplay
  def eglGetError(): Int
  def eglGetProcAddress(procname: String): Unit
  def eglInitialize(dpy: EGLDisplay,
                    major: IntBuffer,
                    minor: IntBuffer): Boolean
  def eglMakeCurrent(dpy: EGLDisplay,
                     draw: EGLSurface,
                     read: EGLSurface,
                     ctx: EGLContext): Boolean
  def eglQueryContext(dpy: EGLDisplay,
                      ctx: EGLContext,
                      attribute: Int,
                      value: IntBuffer): Boolean
  def eglQueryString(dpy: EGLDisplay, name: Int): String
  def eglQuerySurface(dpy: EGLDisplay,
                      surface: EGLSurface,
                      attribute: Int,
                      value: IntBuffer): Boolean
  def eglSwapBuffers(dpy: EGLDisplay, surface: EGLSurface): Boolean
  def eglTerminate(dpy: EGLDisplay): Boolean
  def eglWaitGL(): Boolean
  def eglWaitNative(engine: Int): Boolean
  def eglBindTexImage(dpy: EGLDisplay,
                      surface: EGLSurface,
                      buffer: Int): Boolean
  def eglReleaseTexImage(dpy: EGLDisplay,
                         surface: EGLSurface,
                         buffer: Int): Boolean
  def eglSurfaceAttrib(dpy: EGLDisplay,
                       surface: EGLSurface,
                       attribute: Int,
                       value: Int): Boolean
  def eglSwapInterval(dpy: EGLDisplay, interval: Int): Boolean
  def eglBindAPI(api: Int): Boolean
  def eglCreatePbufferFromClientBuffer(dpy: EGLDisplay,
                                       buftype: Int,
                                       buffer: EGLClientBuffer,
                                       config: EGLConfig,
                                       attrib_list: Array[Int]): EGLSurface
  def eglQueryAPI(): Int
  def eglReleaseThread(): Boolean
  def eglWaitClient(): Boolean
  def eglGetCurrentContext(): EGLContext
}

#+x11

import com.sun.jna.platform.unix.X11

@jna[iliad.platform.unix.EGL14Library]
trait EGL14Binding

object EGL14 extends EGL14Library with EGL14Binding {

  type EGLConfig = iliad.platform.unix.EGLConfig
  type EGLSurface = iliad.platform.unix.EGLSurface
  type EGLDisplay = iliad.platform.unix.EGLDisplay
  type EGLContext = iliad.platform.unix.EGLContext
  type EGLClientBuffer = iliad.platform.unix.EGLClientBuffer

  val EGL_NO_CONTEXT = new EGLContext
  val EGL_NO_DISPLAY = new EGLDisplay
  val EGL_NO_SURFACE = new EGLSurface
  val EGL_DEFAULT_DISPLAY = new X11.Display

  type EGLNativeDisplayType = X11.Display
  type EGLNativeWindowType = X11.Window
  type EGLNativePixmapType = X11.Pixmap
}

#-x11
