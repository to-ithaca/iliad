package iliad
package gl

import iliad.kernel.platform.GLES30Library

import cats._
import cats.data._
import cats.free._
import cats.implicits._

object GLInterpreter extends (GL ~> Reader[GLES30Library, ?]) {
  def apply[A](gl: GL[A]): Reader[GLES30Library, A] = gl match {

    case GLGetError => Reader(_.glGetError())

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

object GLLogInterpreter extends (GL ~> GL.LogEffect) {

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

object GLDebugInterpreter extends (GL ~> GL.DebugEffect) {

  private val lift: Writer[List[String], ?] ~> XorT[Writer[List[String], ?], String, ?] = 
    new (Writer[List[String], ?] ~> XorT[Writer[List[String], ?], String, ?]) {
      def apply[A](w: Writer[List[String], A]): XorT[Writer[List[String], ?], String, A] = {
        XorT.right(w)
      }
    }

  private val _errorCodes: Set[ErrorCode] = SealedEnum.values
 
  private def onError(method: String)(value: Int): String Xor Unit =
    if(value == GL_NO_ERROR.value) ().right
    else _errorCodes.find(_.value == value) match {
      case Some(code) => s"Call failed with error $code".left
      case None => s"Call failed with unknown error $value".left
    }
 
  private def debug(method: String): GL.DebugEffect[Unit] =
    GL.getError.foldMap(GLLogInterpreter).transform(lift).mapF(_.subflatMap(onError(method)))

  private def onCompileError(log: Option[String]): String Xor Unit = 
    log.map(l => s"Compilation failed with error $l").toLeftXor(())

  private def shaderLog(shader: Int) = 
    GL.getShaderInfoLog(shader).foldMap(GLLogInterpreter).transform(lift).mapF(_.subflatMap(onCompileError))

  def apply[A](gl: GL[A]): GL.DebugEffect[A] = gl match {

    case GLCompileShader(shader) => for {
      _ <- GLLogInterpreter(gl).transform(lift)
      _ <- shaderLog(shader)
      _ <- debug(gl.toString)
    } yield ()

    case _ => for {
      a <- GLLogInterpreter(gl).transform(lift)
      _ <- debug(gl.toString)
    } yield a
  }
}
