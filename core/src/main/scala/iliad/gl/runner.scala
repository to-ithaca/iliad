package iliad
package gl

import cats._
import cats.data._
import cats.implicits._

import com.typesafe.scalalogging._

import freek._

sealed trait GLRunner {
  def run[A](dsl: GL.DSL[A], s: GL.State): Xor[GLError, (GL.State, A)]
}

object GLDebugLogRunner extends GLRunner with LazyLogging {
  
  private val interpreter = GL.runner(OpenGL.debugLog)

  def run[A](dsl: GL.DSL[A], s: GL.State): Xor[GLError, (GL.State, A)] = {
    val prg = dsl.interpret(interpreter)
    val (log, xor) = prg.run(GLES30).run(s).value.run
    log.foreach(l => logger.debug(l))
    xor
  }
}

object GLBasicRunner extends GLRunner {

  private val interpreter = GL.runner(iliad.gl.OpenGL.run)

  def run[A](dsl: GL.DSL[A], s: GL.State): Xor[GLError, (GL.State, A)] = {
    val prg = dsl.interpret(interpreter)
    prg.run(GLES30).run(s).right
  }
}
