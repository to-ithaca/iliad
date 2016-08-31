package iliad
package algebra

import iliad.algebra.syntax.vector._

import spire._
import spire.algebra._
import spire.math._
import spire.implicits._

final class Line2[A](val p0: Vec2[A], val direction: Vec2[A]) {

  /** Finds the point of intersection
    * @param θ the minumum angle between intersecting lines
    *          θ = 0 gives the most precise intersecion, but falls prey to zero errors
    * @param α the maximum angle between the line and an axis for the line to be 
    *          considered parallel to the x or y axis
    *          α = 0 gives the most precise description, but falls prey to zero errors
    */
  def intersection(o: Line2[A], θ: A, α: A)(
      implicit F: Fractional[A], T: Trig[A], M: Module[Vec2[A], A]): Option[Vec2[A]] = {
    if ((direction ⋅ o.direction).abs >= T.cos(θ)) None
    else {
      val β =
        if (o.direction.x.abs <= T.sin(α))
          (o.p0.x - p0.x) / direction.x
        else if (o.direction.y.abs <= T.sin(α))
          (o.p0.y - p0.y) / direction.y
        else {
          val dp = p0 - o.p0
          (dp.y * o.direction.x - dp.x * o.direction.y) /
          (direction.x * o.direction.y - direction.y * o.direction.x)
        }
      Some(p0 + (direction :* β))
    }
  }

  def intersects(o: Line2[A], θ: A, α: A)(implicit F: Fractional[A], T: Trig[A], M: Module[Vec2[A], A]): Boolean =
    intersection(o, θ, α).nonEmpty

  def normal(implicit G: AdditiveGroup[A]): Vec2[A] = v"${-direction.y} ${direction.x}"

  def distance(p: Vec2[A])(implicit F: Fractional[A]): A = ((p - p0) ⋅ normal).abs

  def contains(p: Vec2[A], ds: A)(implicit F: Fractional[A]): Boolean = distance(p) < ds

  def parallel(d: Vec2[A], α: A)(implicit F: Fractional[A], T: Trig[A]): Boolean =
    (direction ⋅ d).abs >= T.cos(α)

  def parallel(that: Line2[A], α: A)(implicit F: Fractional[A], T: Trig[A]): Boolean =
    parallel(that.direction, α)

  def colinearEqv(that: Line2[A], ds: A, α: A)(
      implicit F: Fractional[A], T: Trig[A]): Boolean =
    parallel(that, α) && contains(that.p0, ds)

  def equation(implicit F: Fractional[A]): String = {
    if (direction.x === F.zero) s"[x = ${p0.x}]"
    else {
      val m = direction.y / direction.x
      val c = p0.y - p0.x * m
      s"[y = $m x + $c]"
    }
  }

  def ===(that: Line2[A])(implicit ea: Eq[A]): Boolean =
    p0 === that.p0 && direction === that.direction

  def map[B](f: A => B): Line2[B] = 
    Line2(p0.map(f), direction.map(f))
}

object Line2 {

  def apply[A](p: Vec2[A], direction: Vec2[A]): Line2[A] = 
    new Line2(p, direction)

  def normalized[A: Fractional](p0: Vec2[A], direction: Vec2[A])(implicit N: NormedVectorSpace[Vec2[A], A]): Line2[A] =
    Line2(p0, direction.normalize)

  implicit def line2Eq[A](implicit ea: Eq[A]): Eq[Line2[A]] = new Line2Eq[A] {
    val EA = ea
  }

  implicit lazy val line2Functor: cats.Functor[Line2] = new cats.Functor[Line2] {
    def map[A, B](fa: Line2[A])(f: A => B): Line2[B] = fa.map(f)
  }
}

private[algebra] sealed trait Line2Eq[A] extends Eq[Line2[A]] {
  implicit val EA: Eq[A]
  def eqv(x: Line2[A], y: Line2[A]): Boolean = x === y
}



final class LineSegment2[A](val start: Vec2[A], val end: Vec2[A]) {
  def line(implicit G: AdditiveGroup[A], N: NormedVectorSpace[Vec2[A], A]): Line2[A] =
    Line2(start, (end - start).normalize)

  def pmap[B](f: Vec2[A] => Vec2[B]): LineSegment2[B] =
    LineSegment2(f(start), f(end))

  def map[B](f: A => B): LineSegment2[B] =
    new LineSegment2(start.map(f), end.map(f))

  def length(implicit G: AdditiveGroup[A], N: NormedVectorSpace[Vec2[A], A]): A = (end - start).norm

  /**
    * Returns true if a point on the infinite line is within the finite bounds
    *  @param ds the minimum distance from the end points that a point must lie
    *  @param p a point on the infinite line
    */
  def interior(ds: A)(p: Vec2[A])(implicit F: Ring[A], O: Order[A], N: NormedVectorSpace[Vec2[A], A]): Boolean = {
    val l = (p - start) ⋅ direction
    l >= ds && l <= (length - ds)
  }

  def parallel(d: Vec2[A], α: A)(implicit T: Trig[A],
                                 F: Fractional[A], N: NormedVectorSpace[Vec2[A], A]): Boolean =
    line.parallel(d, α)

  def intersection(o: LineSegment2[A], θ: A, α: A, ds: A)(
      implicit F: Fractional[A],
      T: Trig[A], N: NormedVectorSpace[Vec2[A], A]): Option[Vec2[A]] =
    line
      .intersection(o.line, θ, α)
      .filter(interior(ds))
      .filter(o.interior(ds))

  def intersects(o: LineSegment2[A], θ: A, α: A, ds: A)(
      implicit F: Fractional[A],
      T: Trig[A], N: NormedVectorSpace[Vec2[A], A]): Boolean =
    intersection(o, θ, α, ds).nonEmpty

  def direction(implicit F: AdditiveGroup[A], N: NormedVectorSpace[Vec2[A], A]): Vec2[A] = line.direction
  def distance(implicit F: AdditiveGroup[A], N: NormedVectorSpace[Vec2[A], A]): A = (end - start).norm
  def midpoint(implicit F: Fractional[A], V: VectorSpace[Vec2[A], A]): Vec2[A] =
    (end + start) :/ F.fromInt(2)

  def overlays(o: LineSegment2[A], ds: A, α: A)(implicit F: Fractional[A],
                                                T: Trig[A],
                                                EA: Eq[A], N: NormedVectorSpace[Vec2[A], A]): Boolean = {
    if (line.colinearEqv(o.line, ds, α)) {
      val l =
        if (o.direction ⋅ direction > F.zero) o
        else new LineSegment2(o.end, o.start)
      if ((l.start - start).norm < ds || (l.end - end).norm < ds) {
        true
      } else {
        val signs = Set(
            ((o.start - end) ⋅ direction).sign,
            ((o.end - end) ⋅ direction).sign,
            ((o.start - start) ⋅ direction).sign,
            ((o.end - start) ⋅ direction).sign
        ).filter(_ != Sign.Zero)
        signs.size != 1
      }
    } else false
  }

  /** Axis angle rotation which rotates v to this direction */
  def rotation(v: Vec2[A])(implicit F: Fractional[A],
                         T: Trig[A], N: NormedVectorSpace[Vec2[A], A]): Vec4[A] = {
    val v3 = v.padZero(3)
    val d3 = direction.padZero(3)
    val θ = T.acos(v3 ⋅ d3).abs * (v3 × d3).z.sign
    v"${F.zero} ${F.zero} ${F.one} $θ"
  }

  def ===(that: LineSegment2[A])(implicit ea: Eq[A]): Boolean =
    start === that.start && end === that.end

  def colinearEqv(that: LineSegment2[A])(
      implicit ea: Eq[A]): Boolean =
    ===(that) || (start === that.end && end === that.start)
}

object LineSegment2 {
  def apply[A](start: Vec2[A], end: Vec2[A]): LineSegment2[A] = 
     new LineSegment2(start, end)

  implicit def boundedLine2Eq[A](implicit ea: Eq[A]): Eq[LineSegment2[A]] =
    new LineSegment2Eq[A] {
      val EA = ea
    }

  implicit lazy val lineSegment2Functor: cats.Functor[LineSegment2] = new cats.Functor[LineSegment2] {
    def map[A, B](fa: LineSegment2[A])(f: A => B): LineSegment2[B] = fa.map(f)
  }
}

private[algebra] sealed trait LineSegment2Eq[A] extends Eq[LineSegment2[A]] {
  implicit val EA: Eq[A]
  def eqv(x: LineSegment2[A], y: LineSegment2[A]): Boolean = x === y
}
