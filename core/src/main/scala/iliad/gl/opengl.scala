package iliad
package gl

import iliad.kernel.platform.GLES30Library

import cats._
import cats.data._
import cats.free._, Free._

object GL extends GLFunctions {

  type DSL[A] = Free[GL, A]
  
  type LogEffect[A] = ReaderT[Writer[List[String], ?], GLES30Library, A]
  type DebugEffect[A] = ReaderT[XorT[Writer[List[String], ?], String, ?], GLES30Library, A]

  val run: GL ~> ReaderT[Id, GLES30Library, ?] = GLInterpreter
  val log: GL ~> LogEffect = GLLogInterpreter
}

sealed trait GL[A]

case object GLGetError extends GL[Int]

case class GLGetShaderiv(shader: Int, pname: ShaderParameter) extends GL[Int]
case class GLGetShaderInfoLog(shader: Int, maxLength: Int) extends GL[String]

case class GLCreateShader(`type`: ShaderType) extends GL[Int]
case class GLShaderSource(shader: Int, sources: List[String]) extends GL[Unit]
case class GLCompileShader(shader: Int) extends GL[Unit]
case object GLCreateProgram extends GL[Int]
case class GLAttachShader(program: Int, shader: Int) extends GL[Unit]
case class GLLinkProgram(program: Int) extends GL[Unit]

sealed trait GLFunctions {

  import GL.DSL

  val getError: DSL[Int] = liftF(GLGetError)

  private def getShaderiv(shader: Int, pname: ShaderParameter): DSL[Int] = liftF(GLGetShaderiv(shader, pname))
  private def getShaderLogLength(shader: Int): DSL[Int] = getShaderiv(shader, GL_INFO_LOG_LENGTH)

  //TODO: string ops with whiteSpaceOption 
  def getShaderInfoLog(shader: Int): DSL[Option[String]] = for {
    l <- getShaderLogLength(shader)
    s <- liftF(GLGetShaderInfoLog(shader, l)).map(s => if(s.isEmpty()) None else Some(s))
  } yield s

  private val createProgram: DSL[Int] = liftF(GLCreateProgram)
  private val createVertexShader: DSL[Int] = liftF(GLCreateShader(GL_VERTEX_SHADER))
  private val createFragmentShader: DSL[Int] = liftF(GLCreateShader(GL_FRAGMENT_SHADER))
  private def shaderSource(shader: Int, sources: List[String]): DSL[Unit] =
    liftF(GLShaderSource(shader, sources))
  private def compileShader(shader: Int): DSL[Unit] =
    liftF(GLCompileShader(shader))
  private def attachShader(program: Int, shader: Int): DSL[Unit] =
    liftF(GLAttachShader(program, shader))
  private def linkProgram(program: Int): DSL[Unit] = liftF(GLLinkProgram(program))

  def makeVertexShader(source: String): DSL[Int] =
    for {
      id <- createVertexShader
      _ <- shaderSource(id, List(source))
      _ <- compileShader(id)
    } yield id

  def makeFragmentShader(source: String): DSL[Int] =
    for {
      id <- createFragmentShader
      _ <- shaderSource(id, List(source))
      _ <- compileShader(id)
    } yield id

  def makeProgram(vertexId: Int, fragmentId: Int): DSL[Int] = for {
    id <- createProgram
    _ <- attachShader(id, vertexId)
    _ <- attachShader(id, fragmentId)
    _ <- linkProgram(id)
  } yield id
}

//TODO: add a debugger which checks for the program as well
