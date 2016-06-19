package iliad

import scala.reflect._
import iliad.gl._

import cats.~>
import cats.implicits._
import cats.data._

import fs2._
import fs2.util._

trait EGLBootstrap extends kernel.EGLDependencies {

  private val EGLP: EGLPRG[NativeDisplay, NativeWindow, EGLDisplay, EGLConfig, EGLSurface, EGLContext] =
    new EGLPRG
  private val Interpreter: EGLInterpreter[NativeDisplay, NativeWindow, EGLDisplay, EGLConfig, EGLSurface, EGLContext] =
    new EGLInterpreter

  def EGLS(cattrs: Attributes[ConfigAttrib, ConfigAttribValue], wattrs: Attributes[WindowAttrib, WindowAttribValue], cxattrs: Attributes[ContextAttrib, ContextAttribValue])
      (implicit S: Strategy): Task[(EGLDisplay, EGLSurface, EGLContext)] = session.task.flatMap {
    case (window, display) =>
      val prg = (for {
        dpy <- XorT(EGLP.initialise(display))
        cfg <- XorT(EGLP.config(dpy, cattrs).map(Xor.fromOption(_, s"Cannot find for attributes: $cattrs")))
        sfc <- XorT(EGLP.windowSurface(dpy, cfg, window, wattrs))
        ctx <- XorT(EGLP.context(dpy, cfg, cxattrs))
      } yield (dpy, sfc, ctx)).value
      prg.foldMap(Interpreter).run(EGL14).bimap(err => Task.fail(new Error(err)), Task.now).merge
  }

  lazy val EGL: Stream[Task, (EGLDisplay, EGLSurface, EGLContext)] =
    Stream.eval(EGLS(???, ???, ???)(Strategy.fromFixedDaemonPool(1, "egl-thread")))

  private def GLSink[F[_], G[_] : cats.Monad](interpreter: GL ~> G, unwrap: G[Unit] => String Xor Unit)(egl: Stream[F, (EGLDisplay, EGLSurface, EGLContext)], prg: Stream[F, GL.DSL[Unit]]): Stream[F, G[Unit]] = {
    for {
      dddd <- egl.take(1)
      f <- prg
    } yield {
      f.foldMap(interpreter)
    }
  }

  lazy val GLS: Pipe2[Task, (EGLDisplay, EGLSurface, EGLConfig), GL[Unit], Unit] = GLSink(GLInterpreter)

  



  //glSink


}
