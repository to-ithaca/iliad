package iliad
package gfx

import iliad.algebra._
import iliad.algebra.syntax.vector._
import iliad.algebra.syntax.matrix._

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
        mat"""${c0 + x * x * c1}   ${x * y * c1 - z * c2} ${x * z * c1 + y * c2} $zero
        ${x * y * c1 + z * c2} ${c0 + y * y * c1}   ${y * z * c1 - x * c2} $zero
        ${x * z * c1 - y * c2} ${z * y * c1 + x * c2} ${c0 + z * z * c1}   $zero
        $zero            $zero            $zero            $one"""
    )
  }

  def rotate(v: Vec3[A])(implicit MM: MatrixMultiplicativeGroup[Matrix, nat._4, nat._4, A]): Vec3[A] =
    matrix.rotate(v)

  def fraction(f: A): AxisAngle[A] =
    AxisAngle(axis, θ * f)
}

object AxisAngle {
  def apply[A: Trig: Ring](v: Vec4[A]): AxisAngle[A] =
    AxisAngle(v.dropUntil(3), v.w)

  def apply[A](from: Vec3[A], to: Vec3[A])(implicit R: Ring[A], E: Eq[A], T: Trig[A], 
    N: NormedVectorSpace[Vec3[A], A]): AxisAngle[A] = {
    val a = from × to
    val n = a.norm
    val axis = if (n === R.zero) Vector.basis[Z, _3D, A] else a :/ n
    val θ = T.acos(from ⋅ to)
    AxisAngle(axis, θ)
  }
}

case class RotationMatrix[A: AdditiveMonoid](matrix: Mat4[A]) {
  def rotate(v: Vec3[A])(implicit MM: MatrixMultiplicativeGroup[Matrix, nat._4, nat._4, A]): Vec3[A] =
    (matrix * v.padZero(4)).dropUntil(3)
}

/** Sign is in a right handed rotation system */
sealed abstract class Rotation(val sign: Sign)
object Rotation {
  case object Anticlockwise extends Rotation(Sign.Positive)
  case object Clockwise extends Rotation(Sign.Negative)
}
