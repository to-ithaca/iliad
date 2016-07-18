package iliad

import iliad.gfx._

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

import CatsExtra._

trait GLBootstrap extends kernel.GLDependencies with LazyLogging {

  def graph: State[Graph.Constructor, Unit]
  def algorithm: Algorithm

  private def graphicsConfig: Stream[Task, Graphics.Config] = Stream.eval {
    Construct(graph) match {
      case Validated.Invalid(err) =>
        Task.fail(new Error(err.unwrap.mkString("\n")))
      case Validated.Valid(g) =>
        Task.now(Graphics.Config(pageSize, g, algorithm))
    }
  }

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
  ] ~> ReaderT[Xor[String, ?],
               EGL14Library.Aux[NativeDisplay,
                                NativeWindow,
                                EGLDisplay,
                                EGLConfig,
                                EGLSurface,
                                EGLContext],
               ?] = new EGLDebugInterpreter(
      Interpreter
        .compose(
            new EffectfulLogBefore[EGL[NativeDisplay,
                                       NativeWindow,
                                       EGLDisplay,
                                       EGLConfig,
                                       EGLSurface,
                                       EGLContext,
                                       ?]](_.toString))
        .andThen(new EffectfulLogAfter))

  private def EGLTask(s: (NativeWindow, NativeDisplay))(
      cattrs: Attributes[ConfigAttrib, ConfigAttribValue],
      wattrs: Attributes[WindowAttrib, WindowAttribValue],
      cxattrs: Attributes[ContextAttrib, ContextAttribValue])(
      implicit S: Strategy): Task[(EGLDisplay, EGLSurface, EGLContext)] =
    s match {
      case (window, display) =>
        val prg = (for {
          dpy <- XorT(EGLP.initialise(display))
          _ <- XorT.right(EGLP.properties(dpy))
          cfg <- XorT(
                    EGLP
                      .config(dpy, cattrs)
                      .map(_.toRightXor(
                              s"Cannot find for attributes: $cattrs")))
          _ <- XorT.right(EGLP.configAttribs(dpy, cfg))
          sfc <- XorT(EGLP.windowSurface(dpy, cfg, window, wattrs))
          ctx <- XorT(EGLP.context(dpy, cfg, cxattrs))
          _ <- XorT(EGLP.makeCurrent(dpy, sfc, sfc, ctx))
        } yield (dpy, sfc, ctx)).value
        eglExecute(display, prg)
    }

  def eglExecute[A](d: NativeDisplay, dsl: EGLP.DSL[String Xor A]): Task[A] = {
    lockDisplay.foreach(_ (d))
    val t = dsl
      .foldMap(LogEGLInterpreter)
      .run(EGL14)
      .flatMap(identity)
      .bimap(err => Task.fail(new Error(err)), Task.now)
      .merge[Task[A]]
    unlockDisplay.foreach(_ (d))
    t
  }

  val EGLStrategy = Strategy.fromFixedDaemonPool(1, "egl-thread")

  def EGL(s: (NativeWindow, NativeDisplay))
    : Stream[Task, (EGLDisplay, EGLSurface, EGLContext)] =
    Stream.eval(
        EGLTask(s)(Attributes(
                       ConfigAttrib(EGL_LEVEL, 0),
                       ConfigAttrib(EGL_SURFACE_TYPE, EGL_WINDOW_BIT),
                       ConfigAttrib(EGL_RENDERABLE_TYPE,
                                    EGL_OPENGL_ES3_BIT.value),
                       ConfigAttrib(EGL_CONFORMANT, EGL_OPENGL_ES3_BIT),
                       ConfigAttrib(EGL_BLUE_SIZE, 8),
                       ConfigAttrib(EGL_GREEN_SIZE, 8),
                       ConfigAttrib(EGL_RED_SIZE, 8),
                       ConfigAttrib(EGL_ALPHA_SIZE, 8),
                       ConfigAttrib(EGL_BUFFER_SIZE, 32),
                       ConfigAttrib(EGL_DEPTH_SIZE, 16)
                   ),
                   Attributes.empty,
                   Attributes(
                       ContextAttrib(EGL_CONTEXT_CLIENT_VERSION, 3)
                   ))(EGLStrategy))

  private def swapBuffers(nd: NativeDisplay,
                          d: EGLDisplay,
                          s: EGLSurface): Stream[Task, Boolean] =
    Stream.eval(eglExecute(nd, EGLP.swapBuffers(d, s)))

  private def aggregateRight[F[_]: Async, A, B]
    : Pipe2[F, A, B, (A, Vector[B])] =
    (fa, fb) =>
      (fa either fb)
        .mapAccumulate(Vector.empty[B]) { (bs, i) =>
          i.toXor match {
            case Xor.Left(a) => (Vector.empty, Some((a, bs)))
            case Xor.Right(b) => (bs :+ b, None)
          }
        }
        .map(_._2)
        .filter(_.nonEmpty)
        .map(_.get)

  private def aggregate(implicit S: Strategy)
    : Pipe[Task, List[Graphics], (Long, List[Graphics])] =
    q =>
      (vsync through2 q)(aggregateRight).map {
        case (at, cmds) => (at, cmds.flatten.toList)
    }

  private def run(cfg: Graphics.Config, gs: List[Graphics])(s: Graphics.State)
    : Error Xor (Graphics.State, XorT[GL.DSL, String, Unit]) =
    Graphics(gs).run(cfg).run(s).leftMap(new Error(_))

  private def run[A](gl: GL.DSL[String Xor A],
                     s: GL.State): Xor[Error, GL.State] = {
    val interpreter = GL.runner(iliad.gl.OpenGL.debugLog)
    val prg = gl.interpret(interpreter)
    val (log, xor) = prg.run(GLES30).run(s).value.run
    log.foreach(l => logger.debug(l))
    xor.flatMap {
      case (nextS, xxor) => xxor.map(_ => nextS)
    }.leftMap(new Error(_))
  }

  private def run(cfg: Graphics.Config,
                  gls: GL.State,
                  us: Animation.Values,
                  gs: Graphics.State): Xor[Error, GL.State] =
    run(Graphics.draws(gs, us).run(cfg).value, gls)

  val AnimS = Strategy.fromFixedDaemonPool(8, "animation")

  private def run(at: Long,
                  anim: Animation.State): Stream[Task, Animation.Values] = {
    implicit val S = AnimS
    val s = Stream.emits(anim.toList).map {
      case (n, fs) =>
        Stream.eval(Task {
          val us = Animation.calculate(at, fs)
          (n, us)
        })
    }
    Stream.eval(concurrent.join(8)(s).runLog).map(_.toMap)
  }

  val GLPipe: Pipe[Task, List[Graphics], Unit] = graphics =>
    for {
      s <- Stream.eval(session.task(EGLStrategy))
      d <- EGL(s)
      cfg <- graphicsConfig
      gls <- (graphics through aggregate(EGLStrategy))
              .mapAccumulate(
                  Graphics.empty(cfg.graph).right[Error]
              ) { (prev, t) =>
                t match {
                  case (at, gs) =>
                    val xor = prev.flatMap(run(cfg, gs))
                    val next = xor.map(_._1)
                    val out = xor.map {
                      case (n, gl) => (at, gl.value, n)
                    }.bimap(Task.fail, o => Task.now(o))
                      .merge[Task[(Long,
                                   GL.DSL[String Xor Unit],
                                   Graphics.State)]]
                    (next, out)
                }
              }
              .map(_._2)
              .evalMap(identity)
      (at, loadDsl, graphics) = gls
      _ <- run(at, graphics.animation)
            .mapAccumulate(GL.empty.right[Error]) { (prev, ns) =>
              val next = for {
                p <- prev
                x <- run(loadDsl, p)
                xx <- run(cfg, x, ns, graphics)
              } yield xx
              val out =
                next.bimap(Task.fail, _ => Task.now(())).merge[Task[Unit]]
              (next, out)
            }
            .map(_._2)
            .evalMap(identity)
      _ <- swapBuffers(s._2, d._1, d._2)
    } yield ()
}
