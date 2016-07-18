package iliad
package kernel

import scala.reflect._

import fs2.util._

trait IliadApp {
  def run(): Unit
}

trait ScreenDependencies {
  type NativeWindow
  type NativeDisplay
  type NativePixmap

  val session: BlockingPromise[(NativeWindow, NativeDisplay)] =
    new BlockingPromise[(NativeWindow, NativeDisplay)]

  def lockDisplay: Option[NativeDisplay => Unit] = None
  def unlockDisplay: Option[NativeDisplay => Unit] = None

  def vsync: fs2.Stream[Task, Long]
}

trait GLDependencies extends ScreenDependencies {

  type EGLDisplay
  type EGLSurface
  type EGLContext
  type EGLConfig

  implicit def configClassTag: ClassTag[EGLConfig]

  val EGL14: platform.EGL14Library.Aux[
      NativeDisplay,
      NativeWindow,
      EGLDisplay,
      EGLConfig,
      EGLSurface,
      EGLContext
  ]

  val GLES30: platform.GLES30Library

  val pageSize: Int
}

//Ideally our only implementation inside here would be the EGL14Library
