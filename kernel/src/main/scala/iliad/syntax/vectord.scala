package iliad
package syntax

import shapeless._

trait VectorDSyntax extends VectorContextSyntax {
  type Vec2i = VectorD[nat._2, Int]
  type Vec3i = VectorD[nat._3, Int]
  type Vec4i = VectorD[nat._4, Int]
  type Vec2f = VectorD[nat._2, Float]
  type Vec3f = VectorD[nat._3, Float]
  type Vec4f = VectorD[nat._4, Float]
}
