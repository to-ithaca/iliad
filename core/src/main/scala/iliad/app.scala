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
import Fs2Extra._

trait GLBootstrap extends kernel.GLDependencies with LazyLogging {

  def graph: State[Graph.Constructor, Unit]

  def graphTraversal: GraphTraversal

  private def graphicsConfig: Task[Graphics.Config] =
    Construct
      .validate(graph)
      .map(Graphics.Config(pageSize, _, graphTraversal))
      .leftMap(_.unwrap.mkString("\n"))
      .task

  private val EGLP: EGLPRG[NativeDisplay,
                           NativeWindow,
                           EGLDisplay,
                           EGLConfig,
                           EGLSurface,
                           EGLContext] = new EGLPRG

  /*This needs to be lazy to defer the creation of the classTag until after the $init
   of the subclass is called */
  private lazy val LogEGLInterpreter: EGL[
      NativeDisplay,
      NativeWindow,
      EGLDisplay,
      EGLConfig,
      EGLSurface,
      EGLContext,
      ?
  ] ~> ReaderT[Xor[EGLError, ?],
               EGL14Library.Aux[NativeDisplay,
                                NativeWindow,
                                EGLDisplay,
                                EGLConfig,
                                EGLSurface,
                                EGLContext],
               ?] = EGLInterpreter.logInterpreter

  private lazy val GLInterpreter = 
    GL.runner(OpenGL.run)

  private def EGLTask(window: NativeWindow, display: NativeDisplay)(
      cattrs: Attributes[ConfigAttrib, ConfigAttribValue],
      wattrs: Attributes[WindowAttrib, WindowAttribValue],
      cxattrs: Attributes[ContextAttrib, ContextAttribValue])(
      implicit S: Strategy): Task[(EGLDisplay, EGLSurface, EGLContext)] = {
    val prg = (for {
      dpy <- XorT(EGLP.initialise(display))
      _ <- XorT.right(EGLP.properties(dpy))
      cfg <- XorT(
                EGLP
                  .config(dpy, cattrs)
                  .map(_.toRightXor(EGLConfigError(cattrs))))
              .leftWiden[EGLError]
      _ <- XorT.right(EGLP.configAttribs(dpy, cfg))
      sfc <- XorT(EGLP.windowSurface(dpy, cfg, window, wattrs))
              .leftWiden[EGLError]
      ctx <- XorT(EGLP.context(dpy, cfg, cxattrs)).leftWiden[EGLError]
      _ <- XorT(EGLP.makeCurrent(dpy, sfc, sfc, ctx)).leftWiden[EGLError]
    } yield (dpy, sfc, ctx)).value
    eglExecute(display, prg)
  }

  private def eglExecute[A](d: NativeDisplay,
                            dsl: EGLP.DSL[EGLError Xor A]): Task[A] = {
    lockDisplay.foreach(_ (d))
    val t = dsl.foldMap(LogEGLInterpreter).run(EGL14).flatMap(identity).task
    unlockDisplay.foreach(_ (d))
    t
  }

  val EGLStrategy = Strategy.fromFixedDaemonPool(1, "egl-thread")

  private def EGL(
      w: NativeWindow,
      d: NativeDisplay): Task[(EGLDisplay, EGLSurface, EGLContext)] =
    EGLTask(w, d)(
        Attributes(
            ConfigAttrib(EGL_LEVEL, 0),
            ConfigAttrib(EGL_SURFACE_TYPE, EGL_WINDOW_BIT),
            ConfigAttrib(EGL_RENDERABLE_TYPE, EGL_OPENGL_ES3_BIT.value),
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
        ))(EGLStrategy)

  private def swapBuffers(nd: NativeDisplay,
                          d: EGLDisplay,
                          s: EGLSurface): Task[Boolean] =
    eglExecute(nd, XorT(EGLP.swapBuffers(d, s)).leftWiden[EGLError].value)

  private def aggregateRight[F[_]: Async, A, B]: Pipe2[F, A, B, (A, List[B])] =
    (fa, fb) =>
      (fa either fb)
        .mapAccumulate2(List.empty[B]) { (bs, i) =>
          i.toXor match {
            case Xor.Left(a) => (Nil, Some((a, bs.reverse)))
            case Xor.Right(b) => (b :: bs, None)
          }
        }
        .filter(_.nonEmpty)
        .map(_.get)

  private def aggregate(implicit S: Strategy)
    : Pipe[Task, List[Graphics], (Long, List[Graphics])] =
    q =>
      (vsync through2 q)(aggregateRight).map {
        case (at, cmds) => (at, cmds.flatten.toList)
    }

  private def run(cfg: Graphics.Config, gs: List[Graphics])(
      s: Graphics.State)
    : Error Xor (Graphics.State, XorT[GL.DSL, GLError, Unit]) =
    Graphics(gs).run(cfg).run(s).leftMap(s => new Error(s.toString))

  //TODO: encapsulate the handling of the effect in the runner
  private def runDebug[A](gl: GL.DSL[IliadError Xor A],
                     s: GL.State): Xor[Error, GL.State] = {
    val prg = gl.interpret(GL.runner(OpenGL.debugLog))
    val (log, xor) = prg.run(GLES30).run(s).value.run
    log.foreach(l => logger.debug(l))
    xor.flatMap {
      case (nextS, xxor) => xxor.map(_ => nextS)
    }.leftMap(s => new Error(s.toString))
  }

 private def run[A](gl: GL.DSL[IliadError Xor A],
                     s: GL.State): Xor[Error, GL.State] = {
    val prg = gl.interpret(GLInterpreter)
    val (nextS, xor) = prg.run(GLES30).run(s)
    xor.leftMap(s => new Error(s.toString)).map(_ => nextS)
  }


  private def run(cfg: Graphics.Config,
                  gls: GL.State,
                  us: UniformCache.Values,
                  gs: Graphics.State): Xor[Error, GL.State] =
    run(Graphics.draws(gs, us).run(cfg).value, gls)

  private def run(
      at: Long,
      us: UniformCache.State): (UniformCache.State, UniformCache.Values) =
    UniformCache.values(at).run(us).value

  val GLPipe: Pipe[Task, List[Graphics], Unit] = graphics =>
    for {
      (nw, nd) <- Stream.eval(session.task(EGLStrategy))
      (d, sfc, ctx) <- Stream.eval(EGL(nw, nd))
      cfg <- Stream.eval(graphicsConfig)
      _ <- (graphics through aggregate(EGLStrategy))
            .mapAccumulate2(
                (Graphics.empty(cfg.graph), GL.empty).right[Error]
            ) { (prev, t) =>
              val (at, gs) = t
              val xor = for {
                p <- prev
                (prevGr, prevGl) = p
                q <- run(cfg, gs)(prevGr)
                (nextGr, loadCmds) = q
                midGl <- run(loadCmds.leftWiden[IliadError].value, prevGl)
                (nextUc, us) = run(at, nextGr.uniformCache)
                nextGl <- run(cfg, midGl, us, nextGr)
              } yield {
                (nextGr.copy(uniformCache = nextUc), nextGl)
              }
              (xor, xor.task)
            }
            .eval
            .flatMap { _ =>
              Stream.eval(swapBuffers(nd, d, sfc))
            }
    } yield ()
}
