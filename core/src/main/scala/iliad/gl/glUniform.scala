package iliad
package gl

import iliad.syntax.all._
import iliad.std.all._

import simulacrum.typeclass

@typeclass
trait GLUniform[A] {
  def uniform(name: String, value: A): Uniform
}
case class Uniform(name: String, bind: Int => OpenGL.DSL[Unit])

object GLUniform {

  implicit val intIsUniform: GLUniform[Int] = new IntIsUniform
  implicit val floatIsUniform: GLUniform[Float] = new FloatIsUniform
  implicit val vec2iIsUniform: GLUniform[Vec2i] = new Vec2iIsUniform
  implicit val vec2fIsUniform: GLUniform[Vec2f] = new Vec2fIsUniform
  implicit val vec3iIsUniform: GLUniform[Vec3i] = new Vec3iIsUniform
  implicit val vec4iIsUniform: GLUniform[Vec4i] = new Vec4iIsUniform
  implicit val vec4fIsUniform: GLUniform[Vec4f] = new Vec4fIsUniform
}

private final class IntIsUniform extends GLUniform[Int] {
  def uniform(name: String, value: Int): Uniform =
    Uniform(name, OpenGL.bindUniform1i(_, value))
}

private final class FloatIsUniform extends GLUniform[Float] {
  def uniform(name: String, value: Float): Uniform =
    Uniform(name, OpenGL.bindUniform1f(_, value))
}

private final class Vec2iIsUniform extends GLUniform[Vec2i] {
  def uniform(name: String, value: Vec2i): Uniform =
    Uniform(name, OpenGL.bindUniform2i(_, value))
}

private final class Vec2fIsUniform extends GLUniform[Vec2f] {
  def uniform(name: String, value: Vec2f): Uniform =
    Uniform(name, OpenGL.bindUniform2f(_, value))
}

private final class Vec3iIsUniform extends GLUniform[Vec3i] {
  def uniform(name: String, value: Vec3i): Uniform =
    Uniform(name, OpenGL.bindUniform3i(_, value))
}

private final class Vec3fIsUniform extends GLUniform[Vec3f] {
  def uniform(name: String, value: Vec3f): Uniform =
    Uniform(name, OpenGL.bindUniform3f(_, value))
}

private final class Vec4iIsUniform extends GLUniform[Vec4i] {
  def uniform(name: String, value: Vec4i): Uniform =
    Uniform(name, OpenGL.bindUniform4i(_, value))
}

private final class Vec4fIsUniform extends GLUniform[Vec4f] {
  def uniform(name: String, value: Vec4f): Uniform =
    Uniform(name, OpenGL.bindUniform4f(_, value))
}
