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

import CatsExtra._

trait GLBootstrap extends kernel.GLDependencies with LazyLogging {

  def graph: State[GraphModel.Graph.Constructor, Unit]
  def addGraphCmds(q: Queue[Task, Graphics.DSL[Unit]]): Unit
  def pageSize: Int = 1024

  def graphConstructor: Stream[Task, GraphModel.Graph.Constructed] = Stream.eval {
    GraphConstruction.construct(graph) match {
      case Validated.Invalid(err) => Task.fail(new Error(err.unwrap.mkString("\n")))
      case Validated.Valid(g) => Task.now(g)
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

/*
  private def transformXS[F[_]: cats.Monad, S, L, R](x: XorT[StateT[XorT[F, L, ?], S,?], L, R]):
      StateT[XorT[F, L, ?], S, R] =
    x.value.transformF {
      (fa: XorT[F, L, (S, Xor[L, R])]) =>
      fa.flatMap {
        case (s, xx) => XorT.fromXor(xx.map(a => (s, a)))
      }
    }


  private def executeGL: Pipe[Task, XorT[CachedGL.DSL[?], String, Unit], Unit] =
    _.map { gl =>
      val interpreter = CachedGL.runner(iliad.gl.GL.debugLog)
      val xs = gl.transformF(_.interpret(interpreter).run(GLES30))
      transformXS(xs)
    }.mapAccumulate((Cached.State.empty, Current.State.empty).right[Error]) {
      (prev, cmd) =>
          val next = prev.flatMap { s =>
            val (ls, x) = cmd.runS(s).value.run
            ls.foreach(logger.debug(_))
            x.leftMap(new Error(_))
          }
          (next, next.bimap(Task.fail, _ => Task.now(())).merge[Task[Unit]])
      }
      .map(_._2)
      .evalMap(identity)
 */
// type Pipe2[F[_],-I,-I2,+O] = (Stream[F,I], Stream[F,I2]) => Stream[F,O]

  private def aggregateRight[F[_]: Async, A, B]: Pipe2[F, A, B, (A, Vector[B])] =
    (fa, fb) =>
    (fa either fb).mapAccumulate(Vector.empty[B]) { (bs, i) =>
      i.toXor match {
        case Xor.Left(a) => (Vector.empty, Some((a, bs)))
        case Xor.Right(b) => (bs :+ b, None)
      }
    }.map(_._2)
      .filter(_.nonEmpty)
      .map(_.get)

  private def setUpAsync(implicit S: Strategy):
      Pipe[Task, Graphics.DSL[Unit], List[Graphics.DSL[Unit]]] = q =>
    (vsync through2 q)(aggregateRight).map(_._2.toList)

  private def glExecute[A](dsl: CachedGL.DSL[String Xor A], s: (Cached.State, Current.State)):
      Xor[String, ((Cached.State, Current.State), A)] = {
    val interpreter = CachedGL.runner(iliad.gl.GL.debugLog)
    val prg = dsl.interpret(interpreter)
    val (log, xor) = prg.run(GLES30).run(s).value.run
    log.foreach(l => logger.debug(l))
    xor.flatMap {
      case (nextS, xxor) => xxor.map(a => (nextS, a))
    }
  }

  private def drawFrame(cmds: List[Graphics.DSL[Unit]])(
    prev: (GraphModel.Graph.Instance, (Cached.State, Current.State))):
      Error Xor (GraphModel.Graph.Instance, (Cached.State, Current.State)) = prev match {
    case (graph, glState) =>
      val interpreter = Graphics.runner(pageSize)
      val dsl = cmds.traverse(_.interpret(interpreter))
        .run(graph)
        .value
      glExecute(dsl, glState).flatMap {
        case (midGlState, (nextGraph, _)) =>
          val midDsl = GraphTransform.parse(GraphTransform(nextGraph.ordered.toList)).sequenceUnit.value
          glExecute(midDsl, midGlState).map {
            case (nextGlState, _) => (nextGraph, nextGlState)
          }
      }.leftMap(new Error(_))
  }

  val GLPipe: Pipe[Task, Graphics.DSL[Unit], Unit] = gl =>
    Stream.eval(session.task(EGLStrategy)).flatMap { s =>
      EGL(s).flatMap { d =>
        graphConstructor.flatMap { gc =>
          (gl through setUpAsync(EGLStrategy)).mapAccumulate(
            (gc.instance, (Cached.State.empty, Current.State.empty)).right[Error]) {
            (prev, cmds) =>
            val next = prev.flatMap(drawFrame(cmds))
            // val next = prev.flatMap {
            //   case (graph, glState) =>
            //     val interpreter = Graphics.runner(pageSize)
            //     val free = cmds.traverse(_.interpret(interpreter))
            //       .run(graph)
            //       .value
            //     glExecute(free, glState).flatMap {
            //       case (midGlState, (nextGraph, _)) =>
            //         val free = GraphTransform.parse(GraphTransform(nextGraph.ordered.toList)).sequenceUnit.value
            //         glExecute(free, midGlState).map {
            //           case (nextGlState, _) => (nextGraph, nextGlState)
            //         }
            
            (next, next.bimap(Task.fail, _ => Task.now(())).merge[Task[Unit]])
          }.map(_._2)
            .evalMap(identity).flatMap { _ =>
            swapBuffers(s._2, d._1, d._2).map(_ => ())
          }
        }
      }
    }
}

/*
object Test extends App {
  import scala.concurrent.duration._

  implicit val TStrategy = Strategy.fromFixedDaemonPool(1, "t-thread")
  implicit val TScheduler = Scheduler.fromFixedDaemonPool(4)

  case class SubMiddle(i: Int)
  case class Middle(i: Int)
  case class Initial(m: Middle)

  case class Input(i: Int)

  def increment(s: SubMiddle, m: Middle): SubMiddle = {
    println(s"incrementing $s by $m")
    s.copy(i = s.i + m.i)
  }

  def increment(m: Middle, i: List[Input]): Middle = {
    println(s"incrementing $m by $i")
    val add = i.map(_.i).sum
    m.copy(i = m.i + add)
  }

  val signal: Stream[Task, Signal[Task, Long]] = Stream.eval(async.signalOf[Task, Long](0L))
  val queue: Stream[Task, Queue[Task, Input]] = Stream.eval(async.boundedQueue[Task, Input](10))

  def vsync(s: Signal[Task, Long]): Unit = {
    (for {
      t <- s.get
      _ <- s.set(t + 5L).schedule(1 second)
    } yield vsync(s)).unsafeRunAsync(msg => ()) 
  }

  def addInput(q: Queue[Task, Input]): Unit = {
    q.enqueue1(Input(3)).unsafeRunAsync(msg => ())
  }

  val inputs: Stream[Task, List[Input]] = Stream.eval(Task.delay {
    println("inputting")
    Input(3)
  }).mapAccumulate(List.empty[Input]){(l, i) =>
    val nn = i :: l
    (nn, nn)
  }.map(_._2)

  val initialise: Stream[Task, Initial] = Stream.eval(Task.now {
    println("initializing stream")
    Initial(Middle(0))
  })
  sealed trait Collecting {
    def is: List[Input]
  }
  case class Collect(is: List[Input]) extends Collecting
  case class Both(t: Long, is: List[Input]) extends Collecting

   val rs2 = initialise.flatMap { init =>
    queue.flatMap { q =>
      println("queue")
      addInput(q)
      signal.flatMap { s =>
        println("signal")
        vsync(s)
        s.discrete either q.dequeue
      }
    }.mapAccumulate(Collect(Nil).asInstanceOf[Collecting]) { (c, e) =>
     e.toXor match {
       case Xor.Left(t) => (Both(t, Nil), Both(t, c.is))
       case Xor.Right(i) => (Collect(c.is :+ i), Collect(c.is :+ i))
     }
   }.map(_._2).filter({
     case c: Collect => false
     case c: Both => true
   }).mapAccumulate(init.m){ (m, b) =>
      println(s"got $b")
     val n = increment(m, b.is)
     (n, n)
   }.map(_._1).mapAccumulate(SubMiddle(0)){ (s, m) =>
     val n = increment(s, m)
     (n, n)
    }
   }

  rs2.run.unsafeRunFor(10 seconds)
}
 */
