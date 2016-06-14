package iliad

import cats._
import cats.implicits._

import spire.math._
import spire.implicits._

import shapeless._

import iliad.implicits._

/** Cartesian rectangle */
case class Rect[A](x0y0: VectorD[nat._2, A], dx: A, dy: A) {

  def bottomLeft: VectorD[nat._2, A] = x0y0
  def bottomRight(implicit NA: Numeric[A]): VectorD[nat._2, A] =
    v"${x0y0.x + dx} ${x0y0.y}"
  def topRight(implicit NA: Numeric[A]): VectorD[nat._2, A] =
    v"${x0y0.x + dx} ${x0y0.y + dy}"
  def topLeft(implicit NA: Numeric[A]): VectorD[nat._2, A] =
    v"${x0y0.x} ${x0y0.y + dy}"
  def width: A = dy
  def height: A = dx

  def contains(xy: VectorD[nat._2, A])(implicit NA: Numeric[A]): Boolean = {
    val x0y0 = bottomLeft
    val x1y1 = topRight
    xy.x > x0y0.x && xy.x < x1y1.x && xy.y > x1y1.x && xy.y < x1y1.y
  }
  def combine(that: Rect[A])(implicit NA: Numeric[A]): Rect[A] = {
    val cx0y0 = (this.bottomLeft map2 that.bottomLeft)(_ min _)
    val cx1y1 = (this.topRight map2 that.topRight)(_ max _)
    val dxdy = cx1y1 - cx0y0
    Rect(cx0y0, dxdy.x, dxdy.y)
  }
  def ===(that: Rect[A])(implicit EQ: Eq[A]): Boolean =
    (x0y0 === that.x0y0) && (dx === that.dx) && (dy === that.dy)
}

object Rect extends RectInstances

private[iliad] abstract class RectInstances {
  implicit def rectSemigroup[A](implicit na: Numeric[A]): Semigroup[Rect[A]] =
    new RectSemigroup[A] { val NA = na }
  implicit def rectEq[A](implicit ea: Eq[A]): Eq[Rect[A]] = new RectEq[A] {
    val EA = ea
  }
}

private[iliad] sealed trait RectSemigroup[A] extends Semigroup[Rect[A]] {
  implicit val NA: Numeric[A]
  def combine(x: Rect[A], y: Rect[A]): Rect[A] = x combine y
}

private[iliad] sealed trait RectEq[A] extends Eq[Rect[A]] {
  implicit val EA: Eq[A]
  def eqv(x: Rect[A], y: Rect[A]): Boolean = x === y
}
