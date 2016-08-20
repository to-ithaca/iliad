package iliad
package gl

import java.nio.IntBuffer

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

@jna[iliad.platform.desktop.EGL14Library]
trait EGL14Binding

object EGL14 extends EGL14Library with EGL14Binding {

  type EGLConfig = iliad.platform.desktop.EGLConfig
  type EGLSurface = iliad.platform.desktop.EGLSurface
  type EGLDisplay = iliad.platform.desktop.EGLDisplay
  type EGLContext = iliad.platform.desktop.EGLContext
  type EGLClientBuffer = iliad.platform.desktop.EGLClientBuffer

  val EGL_NO_CONTEXT = new EGLContext
  val EGL_NO_DISPLAY = new EGLDisplay
  val EGL_NO_SURFACE = new EGLSurface
  val EGL_DEFAULT_DISPLAY = new X11.Display

  type EGLNativeDisplayType = X11.Display
  type EGLNativeWindowType = X11.Window
  type EGLNativePixmapType = X11.Pixmap
}
#-x11

#+win32
import com.sun.jna.platform.win32.WinDef

@jna[iliad.platform.desktop.EGL14Library]
trait EGL14Binding

object EGL14 extends EGL14Library with EGL14Binding {

  type EGLConfig = iliad.platform.desktop.EGLConfig
  type EGLSurface = iliad.platform.desktop.EGLSurface
  type EGLDisplay = iliad.platform.desktop.EGLDisplay
  type EGLContext = iliad.platform.desktop.EGLContext
  type EGLClientBuffer = iliad.platform.desktop.EGLClientBuffer

  val EGL_NO_CONTEXT = new EGLContext
  val EGL_NO_DISPLAY = new EGLDisplay
  val EGL_NO_SURFACE = new EGLSurface
  val EGL_DEFAULT_DISPLAY = new WinDef.HDC

  type EGLNativeDisplayType = WinDef.HDC
  type EGLNativeWindowType = WinDef.HWND
  type EGLNativePixmapType = WinDef.HBITMAP
}
#-win32

#+android
import java.nio._

@bridge[android.opengl.EGL14] trait EGL14Binding

object EGL14 extends EGL14Library with EGL14Binding {

  type EGLConfig = android.opengl.EGLConfig
  type EGLSurface = android.opengl.EGLSurface
  type EGLDisplay = android.opengl.EGLDisplay
  type EGLContext = android.opengl.EGLContext
  type EGLClientBuffer = Int

  val EGL_NO_CONTEXT = android.opengl.EGL14.EGL_NO_CONTEXT
  val EGL_NO_DISPLAY = android.opengl.EGL14.EGL_NO_DISPLAY
  val EGL_NO_SURFACE = android.opengl.EGL14.EGL_NO_SURFACE
  val EGL_DEFAULT_DISPLAY = android.opengl.EGL14.EGL_DEFAULT_DISPLAY 

  type EGLNativeDisplayType = Int
  type EGLNativeWindowType = android.view.SurfaceHolder
  type EGLNativePixmapType = Int

  def eglChooseConfig(dpy: EGLDisplay, attrib_list: Array[Int], configs: Array[EGLConfig], config_size: Int, num_config: IntBuffer): Boolean = {
    val nc = new Array[Int](1)
    val r = eglChooseConfig(dpy, attrib_list, 0, configs, 0, config_size, nc, 0)
    num_config.put(nc)
    num_config.rewind()
    r
  }

  def eglCreateContext(dpy: EGLDisplay, config: EGLConfig, share_context: EGLContext, attrib_list: Array[Int]): EGLContext =
    eglCreateContext(dpy, config, share_context, attrib_list, 0)

  def eglCreatePbufferSurface(dpy: EGLDisplay, config: EGLConfig, attrib_list: Array[Int]): EGLSurface =
    eglCreatePbufferSurface(dpy, config, attrib_list, 0)

   def eglCreatePixmapSurface(dpy: EGLDisplay, config: EGLConfig, pixmap: EGLNativePixmapType, attrib_list: Array[Int]): EGLSurface =
    eglCreatePixmapSurface(dpy, config, pixmap, attrib_list, 0)

  def eglCreateWindowSurface(dpy: EGLDisplay, config: EGLConfig, win: EGLNativeWindowType, attrib_list: Array[Int]): EGLSurface = 
    eglCreateWindowSurface(dpy, config, win, attrib_list, 0)

  def eglGetConfigAttrib(dpy: EGLDisplay, config: EGLConfig, attribute: Int, value: IntBuffer): Boolean = {
    val v = new Array[Int](1)
    val r = eglGetConfigAttrib(dpy, config, attribute, v, 0)
    value.put(v)
    value.rewind()
    r
  }

  def eglGetConfigs(dpy: EGLDisplay, configs: Array[EGLConfig], config_size: Int, num_config: IntBuffer): Boolean = {
    val nc = new Array[Int](1)
    val r = eglGetConfigs(dpy, configs, 0, config_size, nc, 0)
    num_config.put(nc)
    num_config.rewind()
    r
  }

  def eglGetProcAddress(procname: String): Unit = {}

  def eglInitialize(dpy: EGLDisplay, major: IntBuffer, minor: IntBuffer): Boolean = {
    val v = new Array[Int](2)
    val r = eglInitialize(dpy, v, 0, v, 1)
    major.put(v(0))
    minor.put(v(1))
    major.rewind()
    minor.rewind()
    r
  }

  def eglQueryContext(dpy: EGLDisplay, ctx: EGLContext, attribute: Int, value: IntBuffer): Boolean = {
    val v = new Array[Int](1)
    val r = eglQueryContext(dpy, ctx, attribute, v, 0)
    value.put(v)
    value.rewind()
    r
  }

  def eglQuerySurface(dpy: EGLDisplay, surface: EGLSurface, attribute: Int, value: IntBuffer): Boolean = {
    val v = new Array[Int](1)
    val r = eglQuerySurface(dpy, surface, attribute, v, 0)
    value.put(v)
    value.rewind()
    r
  }

  def eglCreatePbufferFromClientBuffer(dpy: EGLDisplay, buftype: Int, buffer: EGLClientBuffer, config: EGLConfig, attrib_list: Array[Int]): EGLSurface =
    eglCreatePbufferFromClientBuffer(dpy, buftype, buffer, config, attrib_list, 0)
}

#-android
