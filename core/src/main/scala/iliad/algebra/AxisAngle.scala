package iliad
package algebra

import monocle.std.tuple2._

import spire.algebra._
import spire.implicits._

import shapeless._

import syntax.matrix._

import AxisAngle._

final class AxisAngleOps[A](val value: AxisAngle[A]) extends AnyVal {
  def e = value._2
  def θ = value._1

  def matrix(implicit G: Ring[A], T: Trig[A]): OMat3[A] = {
    val one = G.one
    val zero = G.zero
    val c0 = T.cos(θ)
    val c1 = one - c0
    val c2 = T.sin(θ)
    val x = e.x
    val y = e.y
    val z = e.z
    ortho"""${c0 + x * x * c1}   ${x * y * c1 - z * c2} ${x * z * c1 + y * c2}
            ${x * y * c1 + z * c2} ${c0 + y * y * c1}   ${y * z * c1 - x * c2}
            ${x * z * c1 - y * c2} ${z * y * c1 + x * c2} ${c0 + z * z * c1}"""
  }

  def *:(a: A)(implicit G: MultiplicativeSemigroup[A]): AxisAngle[A] = 
    _θ[A].modify(a * _)(value)

  def /:(a: A)(implicit G: MultiplicativeGroup[A]): AxisAngle[A] = 
    _θ[A].modify(_ / a)(value)

  def *(v: Vec3[A])(implicit G0: MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, A], G1: Ring[A], T: Trig[A]): Vec3[A] =
    matrix * v
}

object AxisAngle {
  def _e[A] = tuple2Field2[A, Vec3[A]].second
  def _θ[A] = tuple2Field1[A, Vec3[A]].first

  def apply[A](θ: A, e: Vec3[A]): AxisAngle[A] = (θ, e)

  def rotation[A](x: Vec3[A], y: Vec3[A])(implicit G: Ring[A], E: Eq[A], T: Trig[A], N: NormedVectorSpace[Vec3[A], A]): AxisAngle[A] = {
    val a = x × y
    val n = a.norm
    val e = if (n === G.zero) Vector.basis[Z, _3D, A] else a :/ n
    val θ = T.acos(x ⋅ y)
    (θ, e)
  }

  def rotation[A](x: Vec2[A], y: Vec2[A])(implicit G: Ring[A], E: Eq[A], T: Trig[A], N: NormedVectorSpace[Vec3[A], A], ev: DummyImplicit): AxisAngle[A] = {
    val z = G.zero
    rotation(x :+ z, y :+ z)
  }
}
