package iliad
package syntax

import iliad.std.all._
import iliad.kernel._

import shapeless._
import shapeless.ops.nat._

trait VectorDCoreSyntax {

  def attributeType[N <: Nat, A](
      implicit SA: SizeOf[A],
      VA: VertexAttribTypeConversion[A],
      toInt: ToInt[N]): AttributeType[VectorD[N, A]] =
    new AttributeType[VectorD[N, A]] {
      def baseType: VertexAttribType = VA.baseType
      def byteSize: Int = SA.byteSize * toInt()
      def elementSize: Int = toInt()
    }

  implicit val vec2fAttributeType = attributeType[nat._2, Float]
}
