package iliad
package gl

import iliad.kernel.platform.GLES30Library

import cats._
import cats.arrow._
import cats.data._
import cats.free._
import cats.implicits._

import freek._

object GLInterpreter
    extends NaturalTransformation[GL, Reader[GLES30Library, ?]] {
  def apply[A](gl: GL[A]): Reader[GLES30Library, A] = gl match {
    case GLCreateShader(t) => Reader(_.glCreateShader(t.value))
    case GLShaderSource(shader, sources) =>
      Reader(
          _.glShaderSource(shader,
                           sources.size,
                           sources.toArray,
                           sources.map(_.length).toArray))
    case GLCompileShader(shader) => Reader(_.glCompileShader(shader))
    case GLCreateProgram => Reader(_.glCreateProgram())
    case GLAttachShader(program, shader) =>
      Reader(_.glAttachShader(program, shader))
    case GLLinkProgram(program) => Reader(_.glLinkProgram(program))
  }
}

object GLLogInterpreter extends NaturalTransformation[GL, GL.LogEffect] {

  private val logAfter: Id ~> Writer[List[String], ?] =
    new (Id ~> Writer[List[String], ?]) {
      def apply[A](a: Id[A]): Writer[List[String], A] =
        a.writer(List(s"returned $a"))
    }

  def apply[A](gl: GL[A]): GL.LogEffect[A] = {
    List(s"called $gl").tell.liftT[ReaderT[?[_], GLES30Library, ?]] >>
    GLInterpreter(gl).transform(logAfter)
  }
}
