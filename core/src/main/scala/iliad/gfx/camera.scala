package iliad
package gfx

import iliad.syntax.vectord._
import iliad.syntax.matrixd._

import spire.math._
import spire.algebra._
import spire.implicits._

import monocle._
import monocle.macros._
import monocle.syntax.all._

import com.typesafe.scalalogging._

/** Perspective camera */
case class Camera[A: Trig: Field: NRoot](position: Vec3[A],
                                         pointAt: Vec3[A],
                                         up: Vec3[A],
                                         near: A,
                                         far: A,
                                         aspect: A,
                                         fov: A)
    extends LazyLogging {

  lazy val direction: Vec3[A] = (pointAt - position).normalize
  lazy val radiusVector: Vec3[A] = position - pointAt
  lazy val radial: Vec3[A] = radiusVector.normalize
  lazy val radius: A = radiusVector.norm
  lazy val sightDistance: A = far - near
  lazy val xAxis: Vec3[A] = direction cross up
  lazy val zAxis: Vec3[A] = up
  lazy val yAxis: Vec3[A] = direction

  private val zero = Field[A].zero
  private val one = Field[A].one

  private lazy val translateZ = -Field[A]
      .fromInt(2) * far * near / (far - near)
  private lazy val scaleZ = (far + near) / (far - near)

  private lazy val perspective: Mat4[A] = {
    val sw = one / Trig[A].tan(fov)
    val sh = sw * aspect

    m"""$sw    $zero  $zero   $zero
        $zero  $sh    $zero   $zero
        $zero  $zero  $scaleZ $translateZ
        $zero  $zero  $one    $zero"""
  }

  private lazy val translate: Mat4[A] = m"""$one  $zero $zero ${-position.x}
          $zero $one  $zero ${-position.y}
          $zero $zero $one  ${-position.z}
          $zero $zero $zero $one"""

  private def rotate: Mat4[A] = {
    val right = up.cross(direction).normalize
    val top = direction.cross(right).normalize

    m"""${right.x}      ${right.y}      ${right.z}      $zero
        ${top.x}        ${top.y}        ${top.z}        $zero
        ${direction.x}  ${direction.y}  ${direction.z}  $zero
        $zero           $zero           $zero           $one"""
  }

  private def invertX: Mat4[A] =
    m"""${-one} $zero  $zero   $zero
        $zero   $one   $zero   $zero
        $zero   $zero  $one    $zero
        $zero   $zero  $zero   $one"""

  def matrix(implicit S: Mat4Algebra[A]): Mat4[A] =
    invertX.times(perspective.times(rotate.times(translate)))

  def peek(p: Vec3[A])(implicit MA: Mat4Algebra[A]): Vec3[A] = {
    val p1 = TransformationMatrix(matrix).transform(p)
    p1.dropUntil(3) :/ p1.w
  }

  def screenToWorld(p: Vec3[A])(implicit MA: Mat4Algebra[A]): Vec3[A] = {
    val w = translateZ / (p.z - scaleZ)
    (matrix.inverse.times(w *: p.padOne(4))).dropUntil(3)
  }

  def screenToWorld(p: Vec2[A])(
      implicit MA: Mat4Algebra[A],
      P: spire.algebra.PartialOrder[A]): BoundedLine[A] = {
    val pFar = v"${p.x} ${p.y} $one"
    val pNear = v"${p.x} ${p.y} ${-one}"
    BoundedLine(screenToWorld(pNear), screenToWorld(pFar))
  }
}

object Camera extends LazyLogging {

  private def _position[A: Trig: Field: NRoot]: Lens[Camera[A], Vec3[A]] =
    GenLens[Camera[A]](_.position)

  def panAroundZ[A: Trig: Field: NRoot](speed: A, rotation: Rotation)(
      t0: Long,
      c0: Camera[A])(implicit MA: Mat4Algebra[A]): Long => Camera[A] = {
    val axis = VectorD.zAxis[A]
    val r = c0.position - c0.pointAt
    val f = (t: Long) => {
      val dt = Field[A].fromInt((t - t0).toInt)
      val dθ = speed * dt * rotation.sign
      AxisAngle(axis, dθ).rotate(r) + c0.pointAt
    }
    (t: Long) =>
      c0 &|-> _position set f(t)
  }

  def scrollAroundZ[A: Trig: Field: NRoot](s0: A, rotation: Rotation, λ: A)(
      t0: Long,
      c0: Camera[A])(implicit MA: Mat4Algebra[A]): Long => Camera[A] = {
    val axis = VectorD.zAxis[A]
    val f = (t: Long) => {
      val dt = Field[A].fromInt((t - t0).toInt)
      val dθ = s0 / λ * (Field[A].one - (-λ * dt).exp) * rotation.sign
      AxisAngle(axis, dθ).rotate(c0.radiusVector) + c0.pointAt
    }
    (t: Long) =>
      c0 &|-> _position set f(t)
  }

  def panToZBy[A: Trig: Fractional](speed: A, θ: A)(t0: Long, c0: Camera[A])(
      implicit MA: Mat4Algebra[A]): Long => Camera[A] = {
    val zAxis = VectorD.zAxis[A]
    val axis = (c0.radial cross zAxis).normalize
    val endPosition = AxisAngle(axis, θ).rotate(c0.radiusVector) + c0.pointAt

    val f = (t: Long) => {
      val dt = Field[A].fromInt((t - t0).toInt)
      val dθ = speed * dt * θ.sign
      if (dθ.abs > θ.abs) endPosition
      else AxisAngle(axis, dθ).rotate(c0.radiusVector) + c0.pointAt
    }

    (t: Long) =>
      c0 &|-> _position set f(t)
  }
}
