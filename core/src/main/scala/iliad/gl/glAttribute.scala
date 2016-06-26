package iliad
package gl

import iliad.syntax.all._
import iliad.std.all._

import simulacrum.typeclass

import shapeless._
import shapeless.ops.nat._

@typeclass
trait AttribTypeOf[A] {
  def attribType: VertexAttribType
}

object AttribTypeOf {
  implicit val floatAttribTypeOf: AttribTypeOf[Float] = new IsAttribTypeOf(
      GL_FLOAT)
  implicit val intAttribTypeOf: AttribTypeOf[Int] = new IsAttribTypeOf(GL_INT)
}

private final class IsAttribTypeOf[N](a: VertexAttribType)
    extends AttribTypeOf[N] {
  val attribType = a
}

@typeclass
trait GLAttribute[A] {
  def attribute(name: String): Attribute.Constructor
}

object GLAttribute {
  implicit val floatIsAttribute: GLAttribute[Float] = new NumberIsAttribute
  implicit val vec2fIsAttribute: GLAttribute[Vec2f] = new VectorIsAttribute
  implicit val vec3fIsAttribute: GLAttribute[Vec3f] = new VectorIsAttribute
  implicit val vec4fIsAttribute: GLAttribute[Vec4f] = new VectorIsAttribute

  implicit val intIsAttribute: GLAttribute[Int] = new NumberIsAttribute
  implicit val vec2iIsAttribute: GLAttribute[Vec2i] = new VectorIsAttribute
  implicit val vec3iIsAttribute: GLAttribute[Vec3i] = new VectorIsAttribute
  implicit val vec4iIsAttribute: GLAttribute[Vec4i] = new VectorIsAttribute
}

private final class VectorIsAttribute[N <: Nat: ToInt, A: AttribTypeOf: SizeOf]
    extends GLAttribute[VectorD[N, A]] {
  val n = ToInt[N].apply()
  def attribute(name: String): Attribute.Constructor =
    Attribute
      .Constructor(name, SizeOf[A].byteSize * n, n, AttribTypeOf[A].attribType)
}

private final class NumberIsAttribute[A: AttribTypeOf: SizeOf]
    extends GLAttribute[A] {
  def attribute(name: String): Attribute.Constructor =
    Attribute
      .Constructor(name, SizeOf[A].byteSize, 1, AttribTypeOf[A].attribType)
}
