package iliad
package gl

import iliad.syntax.all._
import iliad.std.all._

import simulacrum.typeclass

import shapeless._
import shapeless.ops.nat._

@typeclass
trait VertexAttribTypeOf[A] {
  def attribType: VertexAttribType
}

object VertexAttribTypeOf {
  implicit val floatVertexAttribTypeOf: VertexAttribTypeOf[Float] =
    new NumberIsVertexAttribTypeOf(GL_FLOAT)
}

private final class NumberIsVertexAttribTypeOf[N](a: VertexAttribType)
    extends VertexAttribTypeOf[N] {
  val attribType = a
}

@typeclass
trait GLAttribute[A] {
  def attribute(name: String): Attribute.Constructor
}

object GLAttribute {
  implicit val vec2fIsAttribute: GLAttribute[Vec2f] = new VectorIsAttribute
}

private final class VectorIsAttribute[
    N <: Nat: ToInt, A: VertexAttribTypeOf: SizeOf]
    extends GLAttribute[VectorD[N, A]] {
  val n = ToInt[N].apply()
  def attribute(name: String): Attribute.Constructor =
    Attribute.Constructor(name,
                          SizeOf[A].byteSize * n,
                          n,
                          VertexAttribTypeOf[A].attribType)
}
