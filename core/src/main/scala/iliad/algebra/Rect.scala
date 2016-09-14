package iliad
package algebra

import iliad.algebra.syntax.vector._

import spire._
import spire.algebra._
import spire.math._
import spire.implicits._


final class Rect[A](val x0y0: Vec2[A], val dimensions: Vec2[A]) {

  def width: A = dimensions.x
  def height: A = dimensions.y

  def bottomLeft: Vec2[A] = x0y0
  def bottomRight(implicit F: AdditiveMonoid[A]): Vec2[A] =
    bottomLeft + v"$width ${F.zero}"
  def topRight(implicit F: AdditiveMonoid[A]): Vec2[A] =
    bottomLeft + v"$width $height"
  def topLeft(implicit F: AdditiveMonoid[A]): Vec2[A] =
    bottomLeft + v"${F.zero} $height"

  def midpoint(implicit F: Fractional[A], N: NormedVectorSpace[Vec2[A], A]): Vec2[A] =
    (topRight + bottomLeft) :/ F.fromInt(2)

  def contains(xy: Vec2[A])(implicit F: AdditiveMonoid[A],
                            G: PartialOrder[A]): Boolean = {
    val x0y0 = bottomLeft
    val x1y1 = topRight
    xy.x > x0y0.x && xy.x < x1y1.x && xy.y > x0y0.y && xy.y < x1y1.y
  }
  def combine(that: Rect[A])(implicit F: AdditiveGroup[A],
                             G: Order[A]): Rect[A] = {
    val cx0y0 = (this.bottomLeft map2 that.bottomLeft)(_ min _)
    val cx1y1 = (this.topRight map2 that.topRight)(_ max _)
    val dxdy = cx1y1 - cx0y0
    Rect(cx0y0, dxdy)
  }

  def overlaps(that: Rect[A])(implicit F: AdditiveMonoid[A], G: PartialOrder[A], EQ: Eq[A]): Boolean =
    (this === that) ||
    contains(that.bottomLeft) || contains(that.topLeft) ||
  contains(that.topRight) || contains(that.bottomRight) ||
  that.contains(bottomLeft) || that.contains(topLeft) ||
  that.contains(bottomRight) || that.contains(topRight)

  def ===(that: Rect[A])(implicit EQ: Eq[A]): Boolean =
    (x0y0 === that.x0y0) && (width === that.width) && (height === that.height)

  def map[B](f: A => B): Rect[B] = Rect[B](x0y0.map(f), dimensions.map(f))

  def pmap[B](f: Vec2[A] => Vec2[B])(implicit F: AdditiveMonoid[A], G: AdditiveGroup[B]): Rect[B] = {
    val bl : Vec2[B] = f(bottomLeft)
    val dims = f(topRight) - bl
    Rect(bl, dims)
  }

  def scale(s: Vec2[A])(implicit F: Fractional[A]): Rect[A] = {
    val bl = bottomLeft.map2(s)(_ * _)
    val tr = topRight.map2(s)(_ * _)
    Rect(bl, tr - bl)
  }

  override def toString: String = s"Rect($x0y0, $dimensions)"
}

object Rect extends RectInstances {
  def apply[A](bottomLeft: Vec2[A], dimensions: Vec2[A]): Rect[A] = new Rect(bottomLeft, dimensions)
  def square[A](x0y0: Vec2[A], dw: A): Rect[A] = Rect(x0y0, v"$dw $dw")
  def centredAt[A](p: Vec2[A], dimensions: Vec2[A])(implicit F: Field[A], V: VectorSpace[Vec2[A], A]): Rect[A] = 
    Rect(p - (dimensions :/ F.fromInt(2)), dimensions)

  implicit lazy val rectFunctor: cats.Functor[Rect] = new cats.Functor[Rect] {
    def map[A, B](fa: Rect[A])(f: A => B): Rect[B] = fa.map(f)
  }
}

private[algebra] abstract class RectInstances {
  implicit def rectSemigroup[A](implicit f: AdditiveGroup[A],
                                g: Order[A]): Semigroup[Rect[A]] =
    new RectSemigroup[A] {
      val F = f
      val G = g
    }
  implicit def rectEq[A](implicit ea: Eq[A]): Eq[Rect[A]] = new RectEq[A] {
    val EA = ea
  }
}

private[algebra] sealed trait RectSemigroup[A] extends Semigroup[Rect[A]] {
  implicit val F: AdditiveGroup[A]
  implicit val G: Order[A]
  def op(x: Rect[A], y: Rect[A]): Rect[A] = x combine y
}

private[algebra] sealed trait RectEq[A] extends Eq[Rect[A]] {
  implicit val EA: Eq[A]
  def eqv(x: Rect[A], y: Rect[A]): Boolean = x === y
}

