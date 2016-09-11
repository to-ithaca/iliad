package iliad
package algebra

import spire._
import spire.algebra._
import spire.math._
import spire.implicits._

final class Line3[A](val p0: Vec3[A], val direction: Vec3[A]) {

  def contains(p: Vec3[A])(implicit F: Fractional[A], N: NormedVectorSpace[Vec3[A], A]): Boolean =
    p == p0 || (direction ⋅ (p - p0).normalize).abs == F.one

  def ===(that: Line3[A])(implicit ea: Eq[A]): Boolean =
    p0 === that.p0 && direction === that.direction

  def map[B](f: A => B): Line3[B] = Line3(p0.map(f), direction.map(f))

  override def toString: String = s"Line3($p0, $direction)"
}

object Line3 {
  def apply[A](p: Vec3[A], direction: Vec3[A]): Line3[A] =
     new Line3(p, direction)

  implicit def lineEq[A](implicit ea: Eq[A]): Eq[Line3[A]] = new Line3Eq[A] {
    val EA = ea
  }

  implicit lazy val line3Functor: cats.Functor[Line3] = new cats.Functor[Line3] {
    def map[A, B](fa: Line3[A])(f: A => B): Line3[B] = fa.map(f)
  }
}

private[algebra] sealed trait Line3Eq[A] extends Eq[Line3[A]] {
  implicit val EA: Eq[A]
  def eqv(x: Line3[A], y: Line3[A]): Boolean = x === y
}


final class LineSegment3[A](val start: Vec3[A], val end: Vec3[A]) {

  def vector(implicit G: AdditiveGroup[A]): Vec3[A] = end - start

  def line(implicit G: AdditiveGroup[A], N: NormedVectorSpace[Vec3[A], A]): Line3[A] =
    Line3(start, vector.normalize)

  def length(implicit G: AdditiveGroup[A], N: NormedVectorSpace[Vec3[A], A]): A = vector.norm

  def contains(ds: A)(p: Vec3[A])(implicit F: Fractional[A], N: NormedVectorSpace[Vec3[A], A]): Boolean =
    line.contains(p) && interior(ds)(p)

  def interior(ds: A)(p: Vec3[A])(implicit R: Ring[A], O: Order[A], N: NormedVectorSpace[Vec3[A], A]): Boolean = {
    val l = (p - start) ⋅ direction
    l >= -ds && l <= (length + ds)
  }

  def direction(implicit G: AdditiveGroup[A], N: NormedVectorSpace[Vec3[A], A]): Vec3[A] = line.direction
  def midpoint(implicit R: Ring[A], N: NormedVectorSpace[Vec3[A], A]): Vec3[A] =
    (end + start) :/ R.fromInt(2)

  def ===(that: LineSegment3[A])(implicit ea: Eq[A]): Boolean =
    start === that.start && end === that.end

  def map[B](f: A => B): LineSegment3[B] =
    LineSegment3(start.map(f), end.map(f))

  override def toString: String = s"LineSegment3($start, $end)"
}

object LineSegment3 {
  def apply[A](start: Vec3[A], end: Vec3[A]): LineSegment3[A] = 
    new LineSegment3(start, end)

  implicit def lineSegmentEq[A](implicit ea: Eq[A]): Eq[LineSegment3[A]] =
    new LineSegment3Eq[A] {
      val EA = ea
    }

  implicit lazy val lineSegment3Functor: cats.Functor[LineSegment3] = new cats.Functor[LineSegment3] {
    def map[A, B](fa: LineSegment3[A])(f: A => B): LineSegment3[B] = fa.map(f)
  }
}

private[algebra] sealed trait LineSegment3Eq[A] extends Eq[LineSegment3[A]] {
  implicit val EA: Eq[A]
  def eqv(x: LineSegment3[A], y: LineSegment3[A]): Boolean = x === y
}
