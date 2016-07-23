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

/** Perspective camera */
case class Camera[A: Trig: Numeric](position: Vec3[A],
                                    pointAt: Vec3[A],
                                    up: Vec3[A],
                                    near: A,
                                    far: A,
                                    aspect: A,
                                    fov: A) {

  private val zero = Numeric[A].zero
  private val one = Numeric[A].one

  private lazy val perspective: Mat4[A] = {
    val scale = (far + near) / (far - near)
    val translate = -Numeric[A].fromInt(2) * far * near / (far - near)
    val sw = one / Trig[A].tan(fov)
    val sh = sw * aspect

    m"""$sw    $zero  $zero   $zero
        $zero  $sh    $zero   $zero
        $zero  $zero  $scale $translate
        $zero  $zero  $one    $zero"""
  }

  private lazy val translate: Mat4[A] = m"""$one  $zero $zero ${-position.x}
          $zero $one  $zero ${-position.y}
          $zero $zero $one  ${-position.z}
          $zero $zero $zero $one"""

  private def rotate(implicit F: Field[A]): Mat4[A] = {
    val direction = (pointAt - position).normalize
    val right = up.cross(direction).normalize
    val top = direction.cross(right).normalize

    m"""${right.x}      ${right.y}      ${right.z}      $zero
        ${top.x}        ${top.y}        ${top.z}        $zero
        ${direction.x}  ${direction.y}  ${direction.z}  $zero
        $zero           $zero           $zero           $one"""
  }

  lazy val direction: Vec3[A] = pointAt - position

  def matrix(implicit S: MultiplicativeSemigroup[Mat4[A]],
             F: Field[A]): Mat4[A] =
    perspective.times(rotate.times(translate))
}

object Camera {
  
  private def _position[A: Trig: Numeric]: Lens[Camera[A], Vec3[A]] =
    GenLens[Camera[A]](_.position)

  private def scrollPosition[A: Numeric: Trig](axis: Vec3[A],
                                    v: A,
                                    t0: Long,
                                    λ: A,
                                    p0: Vec3[A])(t: Long)(
      implicit MV: Mat4VProd[A]): Vec3[A] = {
    val dt = Numeric[A].fromInt((t - t0).toInt)
    val θ = v / λ * (Numeric[A].one - (-λ * dt).exp)
    AxisAngle(axis, θ).rotate(p0)
  }

  def scroll[A: Numeric: Trig: Field](v0: Vec3[A],
                                      t0: Long,
                                      λ: A,
                                      c: Camera[A])(
      implicit MV: Mat4VProd[A],
      MS: MultiplicativeSemigroup[Mat4[A]]): Long => Camera[A] = {
    val axis = (v0 cross c.direction).normalize
    val f = scrollPosition(axis, v0.norm, t0, λ, c.position)(_)
    (t: Long) => c &|-> _position set f(t)
  }
}
