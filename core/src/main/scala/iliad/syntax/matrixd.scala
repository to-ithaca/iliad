package iliad
package syntax

import shapeless._

trait MatrixDSyntax extends MatrixContextSyntax {
  type Mat2i = MatrixD[nat._2, nat._2, Int]
  type Mat3i = MatrixD[nat._3, nat._3, Int]
  type Mat4i = MatrixD[nat._4, nat._4, Int]
  type Mat2f = MatrixD[nat._2, nat._2, Float]
  type Mat3f = MatrixD[nat._3, nat._3, Float]
  type Mat4f = MatrixD[nat._4, nat._4, Float]
}
