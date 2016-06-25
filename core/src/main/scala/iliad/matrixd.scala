package iliad

import shapeless._
import shapeless.ops.nat._

import scala.reflect._

/** A MatrixD[N,M] has width N and height M
  *
  * e.g. a MatrixD[_3,_2] would have the form [ x x x
  *                                             x x x ] */
final class MatrixD[W <: Nat, H <: Nat, A] private[iliad] (_unsized: Vector[A],
                                                           val width: Int,
                                                           val height: Int) {

  import LTEq._

  private def unsized = _unsized

  /** Retrieves the element at column c and row r */
  def apply(c: Nat, r: Nat)(implicit evC: c.N <= W,
                            evR: r.N <= H,
                            toIntC: ToInt[c.N],
                            toIntR: ToInt[r.N]): A =
    unsized(toIntC() * height + toIntR())

  def map[B](f: A => B): MatrixD[W, H, B] =
    new MatrixD(unsized map f, width, height)

  def map2[B, C](that: MatrixD[W, H, B])(f: (A, B) => C) =
    that ap (this map f.curried)
  def ap[B](ff: MatrixD[W, H, A => B]): MatrixD[W, H, B] =
    new MatrixD(unsized.zip(ff.unsized).map { case (a, f) => f(a) },
                width,
                height)

  def plus(a: A)(implicit NA: Numeric[A]): MatrixD[W, H, A] =
    map(x => NA.plus(x, a))
  def negate(implicit NA: Numeric[A]): MatrixD[W, H, A] =
    map(x => NA.minus(NA.zero, x))
  def timesl(a: A)(implicit NA: Numeric[A]): MatrixD[W, H, A] =
    map(x => NA.times(x, a))

  def toArray(implicit ct: ClassTag[A]): Array[A] = unsized.toArray

  override def toString: String = {
    val as = (0 until height).map(row =>
          (0 until width).map(col => unsized(col * height + row)))

    val text = as.map(_.mkString(" ")).mkString("|")
    s"MatrixDim: $text"
  }
}

object MatrixD {
  def fill[W <: Nat, H <: Nat, A](a: A)(implicit toIntW: ToInt[W],
                                        toIntH: ToInt[H]): MatrixD[W, H, A] =
    new MatrixD(Vector.fill(toIntW() * toIntH())(a), toIntW(), toIntH())
  def zero[W <: Nat, H <: Nat, A](implicit toIntW: ToInt[W],
                                  toIntH: ToInt[H],
                                  NA: Numeric[A]): MatrixD[W, H, A] =
    fill(NA.zero)
  def identity[N <: Nat, A](implicit toInt: ToInt[N],
                            NA: Numeric[A]): MatrixD[N, N, A] = {
    val ones = (0 until toInt()).map(_ * (toInt() + 1))
    val id = (0 until toInt() * toInt())
      .map(i => if (ones.contains(i)) NA.one else NA.zero)
      .toVector
    new MatrixD(id, toInt(), toInt())
  }

  lazy val id4f = identity[nat._4, Float]
}
