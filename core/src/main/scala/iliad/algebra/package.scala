package iliad

import shapeless._

package object algebra {
  type Mat2i = Matrix[nat._2, nat._2, Int]
  type Mat3i = Matrix[nat._3, nat._3, Int]
  type Mat4i = Matrix[nat._4, nat._4, Int]
  type Mat2f = Matrix[nat._2, nat._2, Float]
  type Mat3f = Matrix[nat._3, nat._3, Float]
  type Mat4f = Matrix[nat._4, nat._4, Float]
  type Mat2[A] = Matrix[nat._2, nat._2, A]
  type Mat3[A] = Matrix[nat._3, nat._3, A]
  type Mat4[A] = Matrix[nat._4, nat._4, A]
}
