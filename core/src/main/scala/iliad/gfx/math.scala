package iliad
package gfx

import iliad.syntax.vectord._
import iliad.syntax.matrixd._

import shapeless._

import spire.math._
import spire.algebra._
import spire.implicits._

case class AxisAngle[A: Trig: Ring](axis: Vec3[A], θ: A) {
  def matrix: RotationMatrix[A] = {
    val one = Ring[A].one
    val zero = Ring[A].zero

    val c0 = Trig[A].cos(θ)
    val c1 = one - c0
    val c2 = Trig[A].sin(θ)
    val x = axis.x
    val y = axis.y
    val z = axis.z

    RotationMatrix[A](
        m"""${c0 + x * x * c1}   ${x * y * c1 - z * c2} ${x * z * c1 + y * c2} $zero
        ${x * y * c1 + z * c2} ${c0 + y * y * c1}   ${y * z * c1 - x * c2} $zero
        ${x * z * c1 - y * c2} ${z * y * c1 + x * c2} ${c0 + z * z * c1}   $zero
        $zero            $zero            $zero            $one"""
    )
  }

  def rotate(v: Vec3[A])(implicit MA: MatrixAlgebra[nat._4, A]): Vec3[A] =
    matrix.rotate(v)
}

case class RotationMatrix[A: AdditiveMonoid](matrix: Mat4[A]) {
  def rotate(v: Vec3[A])(implicit MA: MatrixAlgebra[nat._4, A]): Vec3[A] =
    matrix.times(v.padZero(4)).dropUntil(3)
}

case class TransformationMatrix[A: Ring](matrix: Mat4[A]) {
  def transform(v: Vec3[A])(implicit MA: MatrixAlgebra[nat._4, A]): Vec4[A] =
    matrix.times(v.padOne(4))
}

/** Sign is in a right handed rotation system */
sealed abstract class Rotation(val sign: Sign)
object Rotation {
  case object Anticlockwise extends Rotation(Sign.Positive)
  case object Clockwise extends Rotation(Sign.Negative)
}
