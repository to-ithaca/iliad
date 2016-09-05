package iliad
package gl

import iliad.algebra._
import iliad.std.all._
import iliad.implicits._

import simulacrum.typeclass

import cats.functor._
import cats.implicits._

@typeclass
trait GLUniform[A] {
  def uniform(name: String, value: A): Uniform.Value
}

object GLUniform {

  implicit lazy val glUniformContravariant: Contravariant[GLUniform] = new GLUniformContravariant

  implicit lazy val intIsUniform: GLUniform[Int] = new IntIsUniform
  implicit lazy val vec2iIsUniform: GLUniform[Vec2i] = new Vec2iIsUniform
  implicit lazy val vec3iIsUniform: GLUniform[Vec3i] = new Vec3iIsUniform
  implicit lazy val vec4iIsUniform: GLUniform[Vec4i] = new Vec4iIsUniform

  implicit lazy val floatIsUniform: GLUniform[Float] = new FloatIsUniform
  implicit lazy val vec2fIsUniform: GLUniform[Vec2f] = new Vec2fIsUniform
  implicit lazy val vec3fIsUniform: GLUniform[Vec3f] = new Vec3fIsUniform
  implicit lazy val vec4fIsUniform: GLUniform[Vec4f] = new Vec4fIsUniform

  implicit lazy val doubleIsUniform: GLUniform[Double] = floatIsUniform.contramap(_.toFloat)
  implicit lazy val vec2dIsUniform: GLUniform[Vec2d] = vec2fIsUniform.contramap(_.cmap)
  implicit lazy val vec3dIsUniform: GLUniform[Vec3d] = vec3fIsUniform.contramap(_.cmap)
  implicit lazy val vec4dIsUniform: GLUniform[Vec4d] = vec4fIsUniform.contramap(_.cmap)

  implicit lazy val mat2fIsUniform: GLUniform[Mat2f] = new Mat2fIsUniform
  implicit lazy val mat3fIsUniform: GLUniform[Mat3f] = new Mat3fIsUniform
  implicit lazy val mat4fIsUniform: GLUniform[Mat4f] = new Mat4fIsUniform

  implicit lazy val mat2dIsUniform: GLUniform[Mat2d] = mat2fIsUniform.contramap(_.cmap)
  implicit lazy val mat3dIsUniform: GLUniform[Mat3d] = mat3fIsUniform.contramap(_.cmap)
  implicit lazy val mat4dIsUniform: GLUniform[Mat4d] = mat4fIsUniform.contramap(_.cmap)

}

private final class IntIsUniform extends GLUniform[Int] {
  def uniform(name: String, value: Int): Uniform.Value =
    Uniform(name, OpenGL.bindUniform1i(_, value))
}

private final class FloatIsUniform extends GLUniform[Float] {
  def uniform(name: String, value: Float): Uniform.Value =
    Uniform(name, OpenGL.bindUniform1f(_, value))
}

private final class Vec2iIsUniform extends GLUniform[Vec2i] {
  def uniform(name: String, value: Vec2i): Uniform.Value =
    Uniform(name, OpenGL.bindUniform2i(_, value))
}

private final class Vec2fIsUniform extends GLUniform[Vec2f] {
  def uniform(name: String, value: Vec2f): Uniform.Value =
    Uniform(name, OpenGL.bindUniform2f(_, value))
}

private final class Vec3iIsUniform extends GLUniform[Vec3i] {
  def uniform(name: String, value: Vec3i): Uniform.Value =
    Uniform(name, OpenGL.bindUniform3i(_, value))
}

private final class Vec3fIsUniform extends GLUniform[Vec3f] {
  def uniform(name: String, value: Vec3f): Uniform.Value =
    Uniform(name, OpenGL.bindUniform3f(_, value))
}

private final class Vec4iIsUniform extends GLUniform[Vec4i] {
  def uniform(name: String, value: Vec4i): Uniform.Value =
    Uniform(name, OpenGL.bindUniform4i(_, value))
}

private final class Vec4fIsUniform extends GLUniform[Vec4f] {
  def uniform(name: String, value: Vec4f): Uniform.Value =
    Uniform(name, OpenGL.bindUniform4f(_, value))
}

private final class Mat2fIsUniform extends GLUniform[Mat2f] {
  def uniform(name: String, value: Mat2f): Uniform.Value =
    Uniform(name, OpenGL.bindUniformMatrix2f(_, value))
}

private final class Mat3fIsUniform extends GLUniform[Mat3f] {
  def uniform(name: String, value: Mat3f): Uniform.Value =
    Uniform(name, OpenGL.bindUniformMatrix3f(_, value))
}

private final class Mat4fIsUniform extends GLUniform[Mat4f] {
  def uniform(name: String, value: Mat4f): Uniform.Value =
    Uniform(name, OpenGL.bindUniformMatrix4f(_, value))
}

private final class GLUniformContravariant extends Contravariant[GLUniform] {
  def contramap[A, B](fa: GLUniform[A])(f: B => A): GLUniform[B] = new GLUniform[B] {
    def uniform(name: String, value: B): Uniform.Value = 
      fa.uniform(name, f(value))
  }
}
