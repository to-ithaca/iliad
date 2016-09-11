package iliad
package gfx

import shapeless.nat

import iliad.algebra._
import iliad.algebra.syntax.matrix._
import iliad.algebra.syntax.vector._
import iliad.algebra.syntax.axisAngle._

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
case class Camera[A](position: Vec3[A],
                     focalPoint: Vec3[A],
                     up: Vec3[A],
                     near: A,
                     far: A,
                     aspect: A,
                     width: A)
    extends LazyLogging {

  def displacement(implicit G: AdditiveGroup[A]): Vec3[A] = focalPoint - position
  def tanFov(implicit F: Fractional[A], N: NormedVectorSpace[Vec3[A], A]) = width / radius
  def fov(implicit F: Fractional[A], T: Trig[A], N: NormedVectorSpace[Vec3[A], A]) = T.atan(tanFov)

  def direction(implicit G: AdditiveGroup[A], N: NormedVectorSpace[Vec3[A], A]): Vec3[A] = displacement.normalize
  def radiusVector(implicit G: AdditiveGroup[A]): Vec3[A] = - displacement
  def radial(implicit G: AdditiveGroup[A], N: NormedVectorSpace[Vec3[A], A]): Vec3[A] = - direction
  def radius(implicit G: AdditiveGroup[A], N: NormedVectorSpace[Vec3[A], A]): A = radiusVector.norm
  def sightDistance(implicit G: AdditiveGroup[A]): A = far - near
  def xAxis(implicit G: Rng[A], N: NormedVectorSpace[Vec3[A], A]): Vec3[A] = direction × up
  lazy val zAxis: Vec3[A] = up
  def yAxis(implicit G: AdditiveGroup[A], N: NormedVectorSpace[Vec3[A], A]): Vec3[A] = direction

  def zero(implicit R: Ring[A]) = R.zero
  def one(implicit R: Ring[A]) = R.one

  def nearOffset(implicit G: AdditiveGroup[A], N: NormedVectorSpace[Vec3[A], A]): A = radius + near
  def farOffset(implicit G: AdditiveGroup[A], N: NormedVectorSpace[Vec3[A], A]): A = radius + far

  private def translateZ(implicit F: Fractional[A], N: NormedVectorSpace[Vec3[A], A]) = 
    - F.fromInt(2) * farOffset * nearOffset / (farOffset - nearOffset)

  private def scaleZ(implicit F: Fractional[A], N: NormedVectorSpace[Vec3[A], A]) = 
    (farOffset + nearOffset) / (farOffset - nearOffset)

  private def perspective(implicit F: Fractional[A], T: Trig[A], N: NormedVectorSpace[Vec3[A], A]): Mat4[A] = {
    val sw = one / tanFov
    val sh = sw * aspect

    mat"""$sw    $zero  $zero   $zero
          $zero  $sh    $zero   $zero
          $zero  $zero  $scaleZ $translateZ
          $zero  $zero  $one    $zero"""
  }

  def translate(implicit R: Ring[A]): Mat4[A] = 
    mat"""$one  $zero $zero ${-position.x}
          $zero $one  $zero ${-position.y}
          $zero $zero $one  ${-position.z}
          $zero $zero $zero $one"""

  def rotate(implicit R: Ring[A], N: NormedVectorSpace[Vec3[A], A]): Mat4[A] = {
    val right = (up × direction).normalize
    val top = (direction × right).normalize

    mat"""${right.x}      ${right.y}      ${right.z}      $zero
        ${top.x}        ${top.y}        ${top.z}        $zero
        ${direction.x}  ${direction.y}  ${direction.z}  $zero
        $zero           $zero           $zero           $one"""
  }

  def invertX(implicit R: Ring[A]): Mat4[A] =
    mat"""${-one} $zero  $zero   $zero
        $zero   $one   $zero   $zero
        $zero   $zero  $one    $zero
        $zero   $zero  $zero   $one"""

  def matrix(implicit F: Fractional[A], T: Trig[A], 
    N: NormedVectorSpace[Vec3[A], A], S: MultiplicativeSemigroup[Mat4[A]]): Mat4[A] =
    invertX * (perspective * (rotate * translate))

  def peek(p: Vec3[A])(implicit F: Fractional[A], T: Trig[A], 
    N: NormedVectorSpace[Vec3[A], A], MA: MultiplicativeSemigroup[Mat4[A]],
MM: MatrixMultiplicativeGroup[Matrix, nat._4, nat._4, A]): Vec3[A] = {
    val p1 = matrix * p.padOne(4)
    p1.dropUntil(3) :/ p1.w
  }

  def screenToWorld(p: Vec3[A])(implicit F: Fractional[A], T: Trig[A], 
    N: NormedVectorSpace[Vec3[A], A], MA: SquareMatrixMultiplicativeGroup[Mat4[A], A], 
    MM: MatrixMultiplicativeGroup[Matrix, nat._4, nat._4, A]): Vec3[A] = {
    val w = translateZ / (p.z - scaleZ)
    (matrix.inverse * (w *: p.padOne(4))).dropUntil(3)
  }

  def screenToWorld(p: Vec2[A])(
      implicit F: Fractional[A], T: Trig[A], 
    N: NormedVectorSpace[Vec3[A], A], MA: SquareMatrixMultiplicativeGroup[Mat4[A], A], 
    MM: MatrixMultiplicativeGroup[Matrix, nat._4, nat._4, A]
): LineSegment3[A] = {
    val pFar = v"${p.x} ${p.y} $one"
    val pNear = v"${p.x} ${p.y} ${-one}"
    LineSegment3(screenToWorld(pNear), screenToWorld(pFar))
  }

  def screenToWorld(p: InputEvent.Point)(
      implicit F: Fractional[A], T: Trig[A], 
    N: NormedVectorSpace[Vec3[A], A], MA: SquareMatrixMultiplicativeGroup[Mat4[A], A],
MM: MatrixMultiplicativeGroup[Matrix, nat._4, nat._4, A]): LineSegment3[A] = 
    screenToWorld(p.windowCoord.cmap[A])
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

  val Anticlockwise = Sign.Positive
  val Clockwise = Sign.Negative

  private def _position[A: Trig : Fractional]: Lens[Camera[A], Vec3[A]] = GenLens[Camera[A]](_.position)
  private def _focalPoint[A: Trig : Fractional]: Lens[Camera[A], Vec3[A]] = GenLens[Camera[A]](_.focalPoint)
  private def _near[A: Trig: Fractional]: Lens[Camera[A], A] = GenLens[Camera[A]](_.near)
  private def _far[A: Trig: Fractional]: Lens[Camera[A], A] = GenLens[Camera[A]](_.far)
  private def _up[A: Trig: Fractional]: Lens[Camera[A], Vec3[A]] = GenLens[Camera[A]](_.up)
  private def _width[A: Trig: Fractional]: Lens[Camera[A], A] = GenLens[Camera[A]](_.width)

  def panAroundZ[A: Trig: Fractional](speed: A, rotation: Sign)
    (implicit MA: MultiplicativeSemigroup[Mat4[A]], MM: MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, A]): CameraFunction[A] =
    CameraFunction.unbounded((t: A, c0: Camera[A]) => {
      val dθ = speed * t * rotation
      val p = (dθ, Vector.basis[Z, _3D, A]) * c0.radiusVector + c0.focalPoint
      c0 &|-> _position set p
    })


  def scrollAroundZ[A: Trig: Fractional](s0: A, rotation: Sign, λ: A)
    (implicit MA: MultiplicativeSemigroup[Mat3[A]], MM: MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, A]): CameraFunction[A] =
    CameraFunction.unbounded((t: A, c0: Camera[A]) => {
      val dθ = s0 / λ * (Field[A].one - (-λ * t).exp) * rotation
      val p = AxisAngle(dθ, Vector.basis[Z, _3D, A]) * c0.radiusVector + c0.focalPoint
      c0 &|-> _position set p
    })

  def panVerticallyBy[A: Trig: Fractional](speed: A, θ: A)
    (implicit MA: MultiplicativeSemigroup[Mat3[A]], N: NormedVectorSpace[Vec3[A], A], MM: MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, A]): CameraFunction[A] = 
    CameraFunction(Some(θ.abs / speed), (t: A, c0: Camera[A]) => {
      val axis = (c0.radial × Vector.basis[Z, _3D, A]).normalize
      val dθ = speed * t * θ.sign
      val p = (dθ, axis) * c0.radiusVector + c0.focalPoint
      c0 &|-> _position set p
    })

  def zoom[A: Trig: Fractional](dt: A, rEnd: A)
    (implicit MA: MultiplicativeSemigroup[Mat4[A]], N: NormedVectorSpace[Vec3[A], A]): CameraFunction[A] =
    CameraFunction(Some(dt), (t: A, c: Camera[A]) => {
        val dr = (rEnd - c.radius) * t / dt
        val p = c.position - (c.direction :* dr)
        c &|-> _position set p
      })

  def interpolate[A: Trig: Fractional](dt: A, pEnd: Vec3[A], upEnd: Vec3[A], fEnd: Vec3[A])
    (implicit M: MultiplicativeSemigroup[Mat4[A]], N: NormedVectorSpace[Vec3[A], A], MM: MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, A]): CameraFunction[A] =
    CameraFunction(Some(dt), (t: A, c: Camera[A]) => {
      val f = t / dt
      val up = (f *: AxisAngle.rotation(c.up, upEnd)) * c.up
      val position = c.position + ((pEnd - c.position) :* f)
      val focalPoint = c.focalPoint + ((fEnd - c.focalPoint) :* f)
      ((_position[A] set position) compose
      (_focalPoint[A] set focalPoint) compose
      (_up[A] set up))(c)
    })

  def interpolate[A: Trig: Fractional](dt: A, pEnd: Vec3[A], upEnd: Vec3[A], fEnd: Vec3[A], wEnd: Double)
    (implicit M: MultiplicativeSemigroup[Mat4[A]], N: NormedVectorSpace[Vec3[A], A], MM: MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, A]): CameraFunction[A] =
    CameraFunction(Some(dt), (t: A, c: Camera[A]) => {
      val f = t / dt
      val up = (f *: AxisAngle.rotation(c.up, upEnd)) * c.up
      val position = c.position + ((pEnd - c.position) :* f)
      val focalPoint = c.focalPoint + ((fEnd - c.focalPoint) :* f)
      val width = f * (wEnd - c.width) + c.width
      ((_position[A] set position) compose
      (_focalPoint[A] set focalPoint) compose
      (_up[A] set up) compose
      (_width[A] set width))(c)
    })

}
 
