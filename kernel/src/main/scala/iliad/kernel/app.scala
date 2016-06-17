package iliad
package kernel

import scala.reflect._

trait IliadApp {
  def run(): Unit
}

trait ScreenDependencies {
  type NativeWindow
  type NativeDisplay
  type NativePixmap

  val session: BlockingPromise[(NativeWindow, NativeDisplay)] = new  BlockingPromise[(NativeWindow, NativeDisplay)]
}

trait EGLDependencies extends ScreenDependencies {

  type EGLDisplay
  type EGLSurface
  type EGLContext
  type EGLConfig

  implicit val ct: ClassTag[EGLConfig]
  
  val EGL14: platform.EGL14Library.Aux[
    NativeDisplay, 
    NativeWindow, 
    EGLDisplay,
    EGLConfig,
    EGLSurface,
    EGLContext
  ]
}

//Ideally our only implementation inside here would be the EGL14Library
