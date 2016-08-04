package iliad
package syntax

import iliad.std.all._
import iliad.kernel._

import shapeless._
import shapeless.ops.nat._

import shapeless._

trait VectorDSyntax extends VectorContextSyntax {
  type Vec2i = VectorD[nat._2, Int]
  type Vec3i = VectorD[nat._3, Int]
  type Vec4i = VectorD[nat._4, Int]
  type Vec2f = VectorD[nat._2, Float]
  type Vec3f = VectorD[nat._3, Float]
  type Vec4f = VectorD[nat._4, Float]
  type Vec2d = VectorD[nat._2, Double]
  type Vec3d = VectorD[nat._3, Double]
  type Vec4d = VectorD[nat._4, Double]
  type Vec2[A] = VectorD[nat._2, A]
  type Vec3[A] = VectorD[nat._3, A]
  type Vec4[A] = VectorD[nat._4, A]
}
