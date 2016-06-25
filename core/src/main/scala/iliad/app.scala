package iliad

import scala.reflect._
import iliad.gl._

import cats._
import cats.~>
import cats.implicits._
import cats.data._

import fs2._
import fs2.util._
import fs2.async.mutable._

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
  ] ~> Reader[EGL14Library.Aux[NativeDisplay,
                               NativeWindow,
                               EGLDisplay,
                               EGLConfig,
                               EGLSurface,
                               EGLContext],
              ?] = Interpreter.compose(
      new EffectfulLogInterpreter[EGL[NativeDisplay,
                                      NativeWindow,
                                      EGLDisplay,
                                      EGLConfig,
                                      EGLSurface,
                                      EGLContext,
                                      ?]](_.toString))

  private def EGLTask(s: (NativeWindow, NativeDisplay))(cattrs: Attributes[ConfigAttrib, ConfigAttribValue],
                      wattrs: Attributes[WindowAttrib, WindowAttribValue],
                      cxattrs: Attributes[ContextAttrib, ContextAttribValue])(
      implicit S: Strategy): Task[(EGLDisplay, EGLSurface, EGLContext)] =
    s match {
      case (window, display) =>
        lockDisplay.foreach(_ (display))
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
        val r = prg
          .foldMap(LogEGLInterpreter)
          .run(EGL14)
          .bimap(err => Task.fail(new Error(err)), Task.now)
          .merge[Task[(EGLDisplay, EGLSurface, EGLContext)]]
        unlockDisplay.foreach(_ (display))
        r
    }

  val EGLStrategy = Strategy.fromFixedDaemonPool(1, "egl-thread")

  def EGL(s: (NativeWindow, NativeDisplay)): Stream[Task, (EGLDisplay, EGLSurface, EGLContext)] =
    Stream.eval(
        EGLTask(s)(Attributes(
                    ConfigAttrib(EGL_LEVEL, 0),
                    ConfigAttrib(EGL_SURFACE_TYPE, EGL_WINDOW_BIT),
                    ConfigAttrib(EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT),
                    ConfigAttrib(EGL_BLUE_SIZE, 8),
                    ConfigAttrib(EGL_GREEN_SIZE, 8),
                    ConfigAttrib(EGL_RED_SIZE, 8),
                    ConfigAttrib(EGL_ALPHA_SIZE, 8),
                    ConfigAttrib(EGL_BUFFER_SIZE, 32),
                    ConfigAttrib(EGL_DEPTH_SIZE, 24)
                ),
                Attributes.empty,
                Attributes(
                    ContextAttrib(EGL_CONTEXT_CLIENT_VERSION, 3)
                ))(EGLStrategy))


  private def executeGL: Pipe[Task, LoadDraw.DSL[Unit], Unit] = _.map { gl =>
    val interpreter = LoadDraw.runner(iliad.gl.GL.debugLog)
    gl.interpret(interpreter).run(GLES30)
    }.mapAccumulate((Cached.State.empty, Current.State.empty).right[Error]) { 
      (prev, cmd) => 
      val next = prev.flatMap { s =>
        val (ls, x) = cmd.run(s).value.run
        ls.foreach(logger.debug(_))
        x.bimap(new Error(_), _._1)
      }
      (next, next.bimap(Task.fail, _ => Task.now(())).merge[Task[Unit]]) 
    }.map(_._2).evalMap(identity)


  private def drawFrame(session: (NativeWindow, NativeDisplay), 
    d: (EGLDisplay, EGLSurface, EGLContext),
    gl: Stream[Task, LoadDraw.DSL[Unit]])(implicit S: Strategy): Stream[Task, Unit] =
    Stream.eval(async.signalOf[Task, Long](0L)).flatMap { s =>
      vsync(s)
      s.discrete.flatMap ( _ => for {
        _ <- gl.through(executeGL)
        _ <- swapBuffers(session._2, d._1, d._2)
      } yield ())
    }

  private def swapBuffers(nd: NativeDisplay, d: EGLDisplay, s: EGLSurface): Stream[Task, Unit] = {
    lockDisplay.foreach(_ (nd))
    val t =  Stream.eval(EGLP.swapBuffers(d, s)
    .foldMap(LogEGLInterpreter)
    .run(EGL14)
    .bimap(err => Task.fail(new Error(err)), _ => Task.now(()))
    .merge[Task[Unit]]
    )
    unlockDisplay.foreach(_ (nd))
    t
  }
  val GLPipe : Pipe[Task, LoadDraw.DSL[Unit], Unit] = gl => for {
    s <- Stream.eval(session.task(EGLStrategy))
    d <- EGL(s)
    _ <- drawFrame(s, d, gl)(EGLStrategy)
    } yield ()
}
