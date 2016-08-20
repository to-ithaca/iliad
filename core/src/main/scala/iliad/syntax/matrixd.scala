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
  type Mat2[A] = MatrixD[nat._2, nat._2, A]
  type Mat3[A] = MatrixD[nat._3, nat._3, A]
  type Mat4[A] = MatrixD[nat._4, nat._4, A]

  type Mat4Algebra[A] = MatrixAlgebra[nat._4, A]
}
