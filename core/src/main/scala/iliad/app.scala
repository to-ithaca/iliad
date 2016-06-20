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

trait EGLBootstrap extends kernel.EGLDependencies {

  private val EGLP: EGLPRG[NativeDisplay,
                           NativeWindow,
                           EGLDisplay,
                           EGLConfig,
                           EGLSurface,
                           EGLContext] = new EGLPRG
  private val Interpreter: EGLInterpreter[NativeDisplay,
                                          NativeWindow,
                                          EGLDisplay,
                                          EGLConfig,
                                          EGLSurface,
                                          EGLContext] = new EGLInterpreter

  def EGLS(cattrs: Attributes[ConfigAttrib, ConfigAttribValue],
           wattrs: Attributes[WindowAttrib, WindowAttribValue],
           cxattrs: Attributes[ContextAttrib, ContextAttribValue])(
      implicit S: Strategy): Task[(EGLDisplay, EGLSurface, EGLContext)] =
    session.task.flatMap {
      case (window, display) =>
        val prg = (for {
          dpy <- XorT(EGLP.initialise(display))
          cfg <- XorT(
                    EGLP
                      .config(dpy, cattrs)
                      .map(_.toRightXor(
                              s"Cannot find for attributes: $cattrs")))
          sfc <- XorT(EGLP.windowSurface(dpy, cfg, window, wattrs))
          ctx <- XorT(EGLP.context(dpy, cfg, cxattrs))
        } yield (dpy, sfc, ctx)).value
        prg
          .foldMap(Interpreter)
          .run(EGL14)
          .bimap(err => Task.fail(new Error(err)), Task.now)
          .merge
    }

  lazy val EGL: Stream[Task, (EGLDisplay, EGLSurface, EGLContext)] =
    Stream.eval(
        EGLS(???, ???, ???)(Strategy.fromFixedDaemonPool(1, "egl-thread")))
}

trait GLBootstrap extends kernel.GLDependencies with kernel.EGLDependencies {

  private def GLSink[F[_], G[_]: cats.Monad, F2[_] <: CoproductK[_]](
      interpreter: freek.Interpreter[F2, LoadDraw.PRG[G, ?]])(
      egl: Stream[F, (EGLDisplay, EGLSurface, EGLConfig)],
      prg: Stream[F, LoadDraw.DSL[Unit]])(
      implicit sub: SubCop[LoadDraw.LoadDraw, F2])
    : Stream[F, LoadDraw.PRG[G, Unit]] = {
    for {
      _ <- egl.take(1)
      f <- prg
    } yield f.interpret(interpreter)
  }

  lazy val GLS: Pipe2[
      Task, (EGLDisplay, EGLSurface, EGLConfig), LoadDraw.DSL[Unit], Unit] =
    (egl, gl) => {
      val initial: (Cached.CachedState, Current.CurrentState) = ???
      val prg = GLSink(LoadDraw.runner(GL.debugLog))(egl, gl)
      prg
        .map(_.run(GLES30))
        .mapAccumulate(initial) { (s, cmd) =>
          val (log, xor) = cmd.run(s).value.run
          println(log.mkString("\n"))
          xor match {
            case Xor.Left(error) =>
              throw new Error(s"Something went wrong: $error")
            case Xor.Right(nextS) => nextS
          }
        }
        .map(_ => ())
    }
}

trait MyApp extends EGLBootstrap with GLBootstrap {

  val loadDraw: Stream[Task, LoadDraw.DSL[Unit]] = ???
}
