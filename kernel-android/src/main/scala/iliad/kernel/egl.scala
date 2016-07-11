package iliad
package kernel

import iliad.kernel.platform.{EGL14Library => Lib}

import android.opengl.{EGL14 => EGL14Library}

import java.nio._

@bridge[EGL14Library] trait EGL14Binding

object EGL14 extends Lib with EGL14Binding {

  type EGLConfig = android.opengl.EGLConfig
  type EGLSurface = android.opengl.EGLSurface
  type EGLDisplay = android.opengl.EGLDisplay
  type EGLContext = android.opengl.EGLContext
  type EGLClientBuffer = Int

  val EGL_NO_CONTEXT = EGL14Library.EGL_NO_CONTEXT
  val EGL_NO_DISPLAY = EGL14Library.EGL_NO_DISPLAY
  val EGL_NO_SURFACE = EGL14Library.EGL_NO_SURFACE
  val EGL_DEFAULT_DISPLAY = EGL14Library.EGL_DEFAULT_DISPLAY 

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
