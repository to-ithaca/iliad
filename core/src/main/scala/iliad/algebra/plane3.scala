package iliad
package algebra

import spire._
import spire.math._
import spire.algebra._
import spire.implicits._

final class Plane3[A](val p0: Vec3[A], val normal: Vec3[A]) {
  def contains(p: Vec3[A])(implicit R: Ring[A]): Boolean =
    (p - p0) ⋅ normal == R.zero

  def intersection(l: Line3[A], θ: A)(implicit F: Fractional[A], T: Trig[A]): Option[Vec3[A]] = {
    val ln = l.direction ⋅ normal
    if (ln.abs < T.cos(θ)) None
    else {
      val d = (p0 - l.p0) ⋅ normal / ln
      val p = d *: l.direction + l.p0
      Some(p)
    }
  }

  def intersection(l: LineSegment3[A], θ: A)(implicit F: Fractional[A], T: Trig[A], 
    N: NormedVectorSpace[Vec3[A], A]): Option[Vec3[A]] =
    intersection(l.line, θ).filter(l.interior)

  def ===(that: Plane3[A])(implicit ea: Eq[A]): Boolean =
    p0 === that.p0 && normal === that.normal

  def map[B](f: A => B): Plane3[B] = Plane3(p0.map(f), normal.map(f))
}

object Plane3 {
  def apply[A](p: Vec3[A], normal: Vec3[A]): Plane3[A] =
    new Plane3(p, normal)

  implicit def plane3Eq[A](implicit ea: Eq[A]): Eq[Plane3[A]] = new Plane3Eq[A] {
    val EA = ea
  }

  implicit lazy val plane3Functor: cats.Functor[Plane3] = new cats.Functor[Plane3] {
    def map[A, B](fa: Plane3[A])(f: A => B): Plane3[B] = fa.map(f)
  }
}

private[algebra] sealed trait Plane3Eq[A] extends Eq[Plane3[A]] {
  implicit val EA: Eq[A]
  def eqv(x: Plane3[A], y: Plane3[A]): Boolean = x === y
}



final class PlaneSegment3[A](val x0y0: Vec3[A],
                             val x0y1: Vec3[A],
                             val x1y0: Vec3[A]) {

  lazy val xAxis: LineSegment3[A] = LineSegment3(x0y0, x1y0)
  lazy val yAxis: LineSegment3[A] = LineSegment3(x0y0, x0y1)
  def normal(implicit R: Ring[A], N: NormedVectorSpace[Vec3[A], A]): Vec3[A] = 
    xAxis.direction × yAxis.direction
  def plane(implicit R: Ring[A], N: NormedVectorSpace[Vec3[A], A]): Plane3[A] = 
    Plane3(x0y0, normal)

  def interior(p: Vec3[A])(implicit R: Ring[A], O: Order[A], N: NormedVectorSpace[Vec3[A], A]): Boolean =
    xAxis.interior(p) && yAxis.interior(p)

  def contains(p: Vec3[A])(implicit R: Ring[A], O: Order[A], N: NormedVectorSpace[Vec3[A], A]): Boolean =
    plane.contains(p) && interior(p)

  def intersection(l: Line3[A], θ: A)(implicit F: Fractional[A], T: Trig[A], 
    N: NormedVectorSpace[Vec3[A], A]): Option[Vec3[A]] =
    plane.intersection(l, θ).filter(interior)

  def intersection(l: LineSegment3[A], θ: A)(implicit F: Fractional[A], T: Trig[A],
    N: NormedVectorSpace[Vec3[A], A]): Option[Vec3[A]] =
    plane.intersection(l, θ).filter(interior)

  def ===(that: PlaneSegment3[A])(implicit ea: Eq[A]): Boolean =
    x0y0 === that.x0y0 && x1y0 === that.x1y0 && x0y1 === that.x0y1

  def map[B](f: A => B): PlaneSegment3[B] =
    PlaneSegment3(x0y0.map(f), x0y1.map(f), x1y0.map(f))
}

object PlaneSegment3 {

  def apply[A](x0y0: Vec3[A], x0y1: Vec3[A], x1y0: Vec3[A]): PlaneSegment3[A] =
    new PlaneSegment3(x0y0, x0y1, x1y0)

  def fromRect[A: AdditiveMonoid](rect: Rect[A], z: A) =
    new PlaneSegment3(rect.bottomLeft.pad(3, z),
      rect.topLeft.pad(3, z),
      rect.bottomRight.pad(3, z))

  implicit def planeSegment3Eq[A](implicit ea: Eq[A]): Eq[PlaneSegment3[A]] =
    new PlaneSegment3Eq[A] {
      val EA = ea
    }

  implicit lazy val planeSegment3Functor: cats.Functor[PlaneSegment3] = new cats.Functor[PlaneSegment3] {
    def map[A, B](fa: PlaneSegment3[A])(f: A => B): PlaneSegment3[B] = fa.map(f)
  }
}

private[algebra] sealed trait PlaneSegment3Eq[A] extends Eq[PlaneSegment3[A]] {
  implicit val EA: Eq[A]
  def eqv(x: PlaneSegment3[A], y: PlaneSegment3[A]): Boolean = x === y
}
