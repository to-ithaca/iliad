package iliad

import shapeless._

package object algebra {

  //Matrix alias
  type Mat2[A] = Matrix[nat._2, nat._2, A]
  type Mat3[A] = Matrix[nat._3, nat._3, A]
  type Mat4[A] = Matrix[nat._4, nat._4, A]
  type Mat2i = Mat2[Int]
  type Mat3i = Mat3[Int]
  type Mat4i = Mat4[Int]
  type Mat2f = Mat2[Float]
  type Mat3f = Mat3[Float]
  type Mat4f = Mat4[Float]

  type Vec2[A] = Vector[nat._2, A]
  type Vec3[A] = Vector[nat._3, A]
  type Vec4[A] = Vector[nat._4, A]
  type Vec2i = Vec2[Int]
  type Vec3i = Vec3[Int]
  type Vec4i = Vec4[Int]
  type Vec2f = Vec2[Float]
  type Vec3f = Vec3[Float]
  type Vec4f = Vec4[Float]
  type Vec2d = Vec2[Double]
  type Vec3d = Vec3[Double]
  type Vec4d = Vec4[Double]

}
