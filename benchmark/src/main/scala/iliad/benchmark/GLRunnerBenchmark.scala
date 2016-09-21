package iliad
package benchmark

import iliad.gfx._
import iliad.gl._
import iliad.implicits._

import cats._
import cats.data._
import cats.implicits._

import org.openjdk.jmh.annotations.{Benchmark, Setup, TearDown, State, Scope}

object GLRunnerBenchmarkUtils {

  case class Session(session: EGLUtils.Session, runner: GLRunner, 
    cfg: Graphics.Config, glState: GL.State, gfxState: Graphics.State)

  def setup(runner: GLRunner): Throwable Xor Session = {

    for {
      g <- GLUtils.graph().leftMap(e => new Error(e.toString))
      cfg = Graphics.Config(
        pageSize = 1024,
        graph = g,
        graphTraversal = GraphTraversal.ordered,
        dimensions = v"128 128")
      s <- EGLUtils.session().leftWiden[Throwable]
      l <- GLUtils.loadGfx.leftMap(e => new Error(e.toString))
      ab <- run(l)(Graphics.empty(g), cfg)
      (gfxState, glCmds) = ab
      glState <- run(runner, glCmds, GL.empty(cfg.dimensions))
    } yield Session(s, runner, cfg, glState, gfxState)
  }

  def show(s: Session, at: Long, gfx: List[GFX]): Throwable Xor RenderState = for {
    ab <- run(gfx)(s.gfxState, s.cfg)
    (gfxState, glCmds) = ab
    glState <- run(s.runner, glCmds, s.glState)
  } yield {
    val (us, values) = UniformCache.values(at).run(gfxState.uniformCache).value
    RenderState(s.session, s.runner, s.cfg, gfxState.copy(uniformCache = us), glState, values)
  }

  private def run(gs: List[GFX])(s: Graphics.State, cfg: Graphics.Config):
      Error Xor (Graphics.State, GL.DSL[IliadError Xor Unit]) =
    Graphics(gs).run(cfg).run(s)
      .leftMap(s => new Error(s.toString))
      .map {
      case (s, xort) => (s, xort.leftWiden[IliadError].value)      }

  private def run[A](runner: GLRunner, gl: GL.DSL[IliadError Xor A], s: GL.State): Xor[Error, GL.State] = {
    val xor = runner.run(gl, s)
    xor.flatMap {
      case (nextS, xxor) => xxor.map(_ => nextS)
    }.leftMap(s => new Error(s.toString))
  }

  case class RenderState(
    session: EGLUtils.Session,
    runner: GLRunner,
    cfg: Graphics.Config,
    graphicsState: Graphics.State, glState: GL.State, values: UniformCache.Values)

  def drawFrame(s: RenderState): RenderState = {
    val xor = for {
      gls <- run(s.runner, Graphics.draws(s.graphicsState, s.values).run(s.cfg).value, s.glState)
      _ <- EGLUtils.swapBuffers(s.session)
    } yield gls
    xor match {
      case Xor.Right(gls) => s.copy(glState = gls)
      case Xor.Left(err) => sys.error(s"Failed to draw frame: $err")
    }
  }
}

@State(Scope.Thread)
class GLBasicRunnerBenchmark {

  var session: GLRunnerBenchmarkUtils.Session = _

  @Setup
  def setup(): Unit = { 
    session = GLRunnerBenchmarkUtils.setup(GLBasicRunner) match {
      case Xor.Right(s) => s
      case Xor.Left(err) => throw err
    }
  }

  @TearDown
  def tearDown(): Unit = {
    EGLUtils.destroy(session.session)
  }

  @Benchmark
  def stateMonad(): Unit = {
    val N = 200
    val gfx = (0 until N).flatMap(id => BasicModel(id, v"1.0 1.0 1.0").showFirst).toList
    val rs = GLRunnerBenchmarkUtils.show(session, 100L, gfx) match {
      case Xor.Right(s) => s
      case Xor.Left(err) => throw err
    }

    (0 until 1000).foldLeft(rs){ (s, _) => GLRunnerBenchmarkUtils.drawFrame(s) }
  }
}
