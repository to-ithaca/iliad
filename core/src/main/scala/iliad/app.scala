package iliad

import scala.reflect._
import iliad.gl._

import cats.implicits._
import cats.data._

import fs2._
import fs2.util._

trait EGLBootstrap extends kernel.EGLDependencies {

  private val EGL: EGLPRG[NativeDisplay, NativeWindow, EGLDisplay, EGLConfig, EGLSurface, EGLContext] =
    new EGLPRG
  private val Interpreter: EGLInterpreter[NativeDisplay, NativeWindow, EGLDisplay, EGLConfig, EGLSurface, EGLContext] =
    new EGLInterpreter

  def initialise(implicit S: Strategy): Task[(EGLDisplay, EGLSurface, EGLContext)] = session.task.flatMap { 
    case (window, display) =>
      val prg = (for {
        dpy <- XorT(EGL.initialise(display))
        cfg <- XorT(EGL.config(dpy, ???).map(Xor.fromOption(_, "Cannot find for attributes: attrs")))
        sfc <- XorT(EGL.windowSurface(dpy, cfg, window, ???))
        ctx <- XorT(EGL.context(dpy, cfg, ???))
      } yield (dpy, sfc, ctx)).value

      prg.foldMap(Interpreter).run(EGL14).bimap(err => Task.fail(new Error(err)), Task.now).merge
  }


}
