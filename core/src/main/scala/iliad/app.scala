package iliad

import scala.reflect._
import iliad.gl._

import cats._
import cats.~>
import cats.implicits._
import cats.data._

import fs2._
import fs2.util._

import freek._

import iliad.kernel.platform.EGL14Library

import com.typesafe.scalalogging._

trait GLBootstrap extends kernel.GLDependencies with LazyLogging {

  private val EGLP: EGLPRG[NativeDisplay,
                           NativeWindow,
                           EGLDisplay,
                           EGLConfig,
                           EGLSurface,
                           EGLContext] = new EGLPRG


  /*This needs to be lazy to defer the creation of the classTag until after the $init 
   of the subclass is called */
  lazy val Interpreter: EGLInterpreter[NativeDisplay,
                                          NativeWindow,
                                          EGLDisplay,
                                          EGLConfig,
                                          EGLSurface,
                                          EGLContext] = new EGLInterpreter

 lazy val LogEGLInterpreter: EGL[
   NativeDisplay, 
   NativeWindow, 
   EGLDisplay, 
   EGLConfig, 
   EGLSurface, 
   EGLContext, 
   ?
 ] ~> Reader[
        EGL14Library.Aux[
          NativeDisplay, 
          NativeWindow, 
          EGLDisplay, 
          EGLConfig, 
          EGLSurface, 
          EGLContext], 
   ?] = Interpreter.compose(new EffectfulLogInterpreter[EGL[
    NativeDisplay, 
    NativeWindow, 
    EGLDisplay, 
    EGLConfig, 
    EGLSurface,
    EGLContext, ?]](_.toString))

  private def EGLTask(cattrs: Attributes[ConfigAttrib, ConfigAttribValue],
           wattrs: Attributes[WindowAttrib, WindowAttribValue],
           cxattrs: Attributes[ContextAttrib, ContextAttribValue])(
      implicit S: Strategy): Task[(EGLDisplay, EGLSurface, EGLContext)] =
    session.task.flatMap {
      case (window, display) =>
        lockDisplay.foreach(_(display))
        val prg = (for {
          dpy <- XorT(EGLP.initialise(display))
          cfg <- XorT(
                    EGLP
                      .config(dpy, cattrs)
                      .map(_.toRightXor(
                              s"Cannot find for attributes: $cattrs")))
          sfc <- XorT(EGLP.windowSurface(dpy, cfg, window, wattrs))
          ctx <- XorT(EGLP.context(dpy, cfg, cxattrs))
          _ <- XorT(EGLP.makeCurrent(dpy, sfc, sfc, ctx))
        } yield (dpy, sfc, ctx)).value
        val r = prg.foldMap(LogEGLInterpreter)
          .run(EGL14)
          .bimap(err => Task.fail(new Error(err)), Task.now)
          .merge[Task[(EGLDisplay, EGLSurface, EGLContext)]]
        unlockDisplay.foreach(_(display))
        r
    }

  def EGL: Stream[Task, (EGLDisplay, EGLSurface, EGLContext)] =
    Stream.eval(
        EGLTask(Attributes(
          ConfigAttrib(EGL_LEVEL, 0),
          ConfigAttrib(EGL_SURFACE_TYPE, EGL_WINDOW_BIT),
          ConfigAttrib(EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT),
          ConfigAttrib(EGL_BLUE_SIZE, 8),
          ConfigAttrib(EGL_GREEN_SIZE, 8),
          ConfigAttrib(EGL_RED_SIZE, 8),
          ConfigAttrib(EGL_ALPHA_SIZE, 8),
          ConfigAttrib(EGL_BUFFER_SIZE, 32),
          ConfigAttrib(EGL_DEPTH_SIZE, 24)

        ), Attributes.empty, Attributes(
          ContextAttrib(EGL_CONTEXT_CLIENT_VERSION, 3)
        ))(Strategy.fromFixedDaemonPool(1, "egl-thread")))



  private def GLSink[F[_], G[_]: cats.Monad, F2[_] <: CoproductK[_]](
      interpreter: freek.Interpreter[F2, LoadDraw.PRG[G, ?]])(
      egl: Stream[F, (EGLDisplay, EGLSurface, EGLContext)],
      prg: Stream[F, LoadDraw.DSL[Unit]])(
      implicit sub: SubCop[LoadDraw.LoadDraw, F2])
    : Stream[F, LoadDraw.PRG[G, Unit]] = {
    for {
      _ <- egl.take(1)
      f <- prg
    } yield f.interpret(interpreter)
  }



  lazy val GL: Pipe2[
      Task, (EGLDisplay, EGLSurface, EGLContext), LoadDraw.DSL[Unit], Unit] =
    (egl, gl) => {
      val prg = GLSink(LoadDraw.runner(iliad.gl.GL.debugLog))(egl, gl)
      prg
        .map(_.run(GLES30))
        .mapAccumulate((Cached.State.empty, Current.State.empty).right[Error]) { (prev, cmd) =>
        val next = prev.flatMap { s =>
          val (ls, x) = cmd.run(s).value.run
          ls.foreach(logger.debug(_))

          x.bimap(new Error(_), _._1)
        }
        (next, next.bimap(Task.fail, _ => Task.now(())).merge[Task[Unit]])
      }.map(_._2).flatMap(Stream.eval)

    }
}
