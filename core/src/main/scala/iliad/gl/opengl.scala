package iliad
package gl

import cats.free._, Free._

object GL {

  type DSL[A] = Free[GL, A]

  val createProgram: DSL[Int] = liftF(GLCreateProgram)
  val createVertexShader: DSL[Int] = liftF(GLCreateShader(???))
  val createFragmentShader: DSL[Int] = liftF(GLCreateShader(???))

}

sealed trait GL[A]

case class GLCreateShader(`type`: ShaderType) extends GL[Int]
case class GLShaderSource(shader: Int, sources: List[String]) extends GL[Int]
case class GLCompileShader(shader: Int) extends GL[Unit]
case object GLCreateProgram extends GL[Int]
