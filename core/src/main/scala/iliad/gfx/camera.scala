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



/** Perspective camera 
  * aspect is width / height
  * */
case class Camera[A: Fractional : Trig](position: Vec3[A],
                                        focalPoint: Vec3[A],
                                         up: Vec3[A],
                                         near: A,
                                         far: A,
                                         aspect: A,
                                         width: A)
    extends LazyLogging {

  lazy val displacement: Vec3[A] = focalPoint - position
  lazy val tanFov = width / radius
  lazy val fov = Trig[A].atan(tanFov)

  lazy val direction: Vec3[A] = displacement.normalize
  lazy val radiusVector: Vec3[A] = - displacement
  lazy val radial: Vec3[A] = - direction
  lazy val radius: A = radiusVector.norm
  lazy val sightDistance: A = far - near
  lazy val xAxis: Vec3[A] = direction cross up
  lazy val zAxis: Vec3[A] = up
  lazy val yAxis: Vec3[A] = direction

  private val zero = Field[A].zero
  private val one = Field[A].one

  val nearOffset: A = radius + near
  val farOffset: A = radius + far

  private lazy val translateZ = -Field[A]
      .fromInt(2) * farOffset * nearOffset / (farOffset - nearOffset)
  private lazy val scaleZ = (farOffset + nearOffset) / (farOffset - nearOffset)

  private lazy val perspective: Mat4[A] = {
    val sw = one / tanFov
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

  def screenToWorld(p: InputEvent.Point)(
      implicit MA: Mat4Algebra[A],
      P: spire.algebra.PartialOrder[A]): BoundedLine[A] = 
    screenToWorld(p.windowCoord.as[A])
}

case class CameraFunction[A](dt: Option[A], _apply: (A, Camera[A]) => Camera[A]) {
  def apply(at: A, c: Camera[A]): Camera[A] = _apply(at, c)
}

object CameraFunction {

  def unbounded[A: Fractional](f: (A, Camera[A]) => Camera[A]): CameraFunction[A] =
    CameraFunction(None, f)
}

object Camera extends LazyLogging with CameraFunctions {

  def start[A : Fractional](t0: Long, initial: Camera[A], cf: CameraFunction[A]): Long => Camera[A] =
    (t: Long) => 
  if(t <= t0) initial
  else {
    val at = ConvertableTo[A].fromLong(t - t0)
    cf(cf.dt.filter(_ < at).getOrElse(at), initial)
  }

  def from[A : Fractional](prev: Long => Camera[A], at: Long, cf: CameraFunction[A]):
      Long => Camera[A] = {
    val initial = prev(at)
    start(at, initial, cf)
  }
}

sealed trait CameraFunctions {

  private def _position[A: Trig : Fractional]: Lens[Camera[A], Vec3[A]] = GenLens[Camera[A]](_.position)
  private def _focalPoint[A: Trig : Fractional]: Lens[Camera[A], Vec3[A]] = GenLens[Camera[A]](_.focalPoint)
  private def _near[A: Trig: Fractional]: Lens[Camera[A], A] = GenLens[Camera[A]](_.near)
  private def _far[A: Trig: Fractional]: Lens[Camera[A], A] = GenLens[Camera[A]](_.far)
  private def _up[A: Trig: Fractional]: Lens[Camera[A], Vec3[A]] = GenLens[Camera[A]](_.up)

  def panAroundZ[A: Trig: Fractional](speed: A, rotation: Rotation)
    (implicit MA: Mat4Algebra[A]): CameraFunction[A] =
    CameraFunction.unbounded((t: A, c0: Camera[A]) => {
      val dθ = speed * t * rotation.sign
      val p = AxisAngle(VectorD.zAxis[A], dθ).rotate(c0.radiusVector) + c0.focalPoint
      c0 &|-> _position set p
    })


  def scrollAroundZ[A: Trig: Fractional](s0: A, rotation: Rotation, λ: A)
    (implicit MA: Mat4Algebra[A]): CameraFunction[A] =
    CameraFunction.unbounded((t: A, c0: Camera[A]) => {
      val dθ = s0 / λ * (Field[A].one - (-λ * t).exp) * rotation.sign
      val p = AxisAngle(VectorD.zAxis[A], dθ).rotate(c0.radiusVector) + c0.focalPoint
      c0 &|-> _position set p
    })

  def panVerticallyBy[A: Trig: Fractional](speed: A, θ: A)
    (implicit MA: Mat4Algebra[A]): CameraFunction[A] = 
    CameraFunction(Some(θ.abs / speed), (t: A, c0: Camera[A]) => {
      val axis = (c0.radial cross VectorD.zAxis[A]).normalize
      val dθ = speed * t * θ.sign
      val p = AxisAngle(axis, dθ).rotate(c0.radiusVector) + c0.focalPoint
      c0 &|-> _position set p
    })

  def zoom[A: Trig: Fractional](dt: A, rEnd: A)
    (implicit MA: Mat4Algebra[A]): CameraFunction[A] =
    CameraFunction(Some(dt), (t: A, c: Camera[A]) => {
        val dr = (rEnd - c.radius) * t / dt
        val p = c.position - (c.direction :* dr)
        c &|-> _position set p
      })

  def interpolate[A: Trig: Fractional](dt: A, pEnd: Vec3[A], upEnd: Vec3[A], fEnd: Vec3[A])
    (implicit M: Mat4Algebra[A]): CameraFunction[A] =
    CameraFunction(Some(dt), (t: A, c: Camera[A]) => {
      val f = t / dt
      val up = AxisAngle(c.up.rotate(upEnd)).fraction(f).rotate(c.up)
      val position = c.position + ((pEnd - c.position) :* f)
      val focalPoint = c.focalPoint + ((fEnd - c.focalPoint) :* f)
      ((_position[A] set position) compose
      (_focalPoint[A] set focalPoint) compose
      (_up[A] set up))(c)
    })
}
