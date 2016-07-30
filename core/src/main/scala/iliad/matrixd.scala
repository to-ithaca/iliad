package iliad

import iliad.kernel.platform.MatrixLibrary

import shapeless._
import shapeless.ops.nat._

import spire.math._
import spire.algebra._
import spire.implicits._

import scala.reflect._

import cats.implicits._

/** A MatrixD[N,M] has width N and height M
  *
  * e.g. a MatrixD[_3,_2] would have the form [ x x x
  *                                             x x x ]
  *  Matrices are stored in row-major order
  */
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

  def map2[B, C](that: MatrixD[W, H, B])(f: (A, B) => C): MatrixD[W, H, C] =
    that ap (this map f.curried)
  def ap[B](ff: MatrixD[W, H, A => B]): MatrixD[W, H, B] =
    new MatrixD(unsized.zip(ff.unsized).map { case (a, f) => f(a) },
                width,
                height)

  def plus(a: A)(implicit G: AdditiveGroup[A]): MatrixD[W, H, A] =
    map(x => G.plus(x, a))
  def unary_-(implicit G: AdditiveGroup[A]): MatrixD[W, H, A] =
    map(-_)
  def timesl(a: A)(implicit G: MultiplicativeSemigroup[A]): MatrixD[W, H, A] =
    map(x => G.times(x, a))
  def times(m: MatrixD[W, H, A])(
      implicit S: MultiplicativeSemigroup[MatrixD[W, H, A]])
    : MatrixD[W, H, A] = S.times(this, m)

 def times(v: VectorD[W, A])(implicit S: MatrixProduct[W, H, nat._1, A],
                              toIntW: ToInt[W],
                              toIntH: ToInt[H]): VectorD[H, A] =
   S.prod(this, v.matrix).vector

  def ===[AA <: A](that: MatrixD[W, H, AA])(implicit EA: cats.Eq[A]): Boolean =
    unsized === that.unsized

  def toArray(implicit ct: ClassTag[A]): Array[A] = unsized.toArray

  def vector(implicit ev: W =:= nat._1, toInt: ToInt[H]): VectorD[H, A] =
    VectorD.sized[H, A](unsized)

  override def toString: String = {
    val text = unsized
      .sliding(width, width)
      .map(row => row.mkString(" | "))
      .mkString("\n")

    s"""MatrixD:
$text"""
  }
}

object MatrixD {
  def fill[W <: Nat, H <: Nat, A](a: A)(implicit toIntW: ToInt[W],
                                        toIntH: ToInt[H]): MatrixD[W, H, A] =
    new MatrixD(Vector.fill(toIntW() * toIntH())(a), toIntW(), toIntH())
  def zero[W <: Nat, H <: Nat, A](implicit toIntW: ToInt[W],
                                  toIntH: ToInt[H],
                                  R: Ring[A]): MatrixD[W, H, A] =
    fill(R.zero)
  def identity[N <: Nat, A](implicit toInt: ToInt[N],
                            R: Ring[A]): MatrixD[N, N, A] = {
    val ones = (0 until toInt()).map(_ * (toInt() + 1))
    val id = (0 until toInt() * toInt())
      .map(i => if (ones.contains(i)) R.one else R.zero)
      .toVector
    new MatrixD(id, toInt(), toInt())
  }

  def sized[W <: Nat, H <: Nat, A](unsized: Vector[A])(
      implicit toIntW: ToInt[W],
      toIntH: ToInt[H]): MatrixD[W, H, A] =
    if (unsized.length != toIntW() * toIntH())
      throw new IllegalArgumentException(
          s"matrix $unsized does not have dimensions ${toIntW()} ${toIntH()}")
    else new MatrixD(unsized, toIntW(), toIntH())

  def sized[A](w: Nat, h: Nat, unsized: Vector[A])(
      implicit toIntW: ToInt[w.N],
      toIntH: ToInt[h.N]): MatrixD[w.N, h.N, A] =
    sized[w.N, h.N, A](unsized)

  lazy val id4f = identity[nat._4, Float]

  implicit def matrix4Group[A: Ring](
      implicit ma: MatrixAlgebra[nat._4, A])
    : Group[MatrixD[nat._4, nat._4, A]] with MultiplicativeSemigroup[
        MatrixD[nat._4, nat._4, A]] =
    new Matrix4Group[A] {
      def MA = ma
      def R = Ring[A]
    }
}

private[iliad] sealed trait MatrixDEq[W <: Nat, H <: Nat, A]
    extends cats.Eq[MatrixD[W, H, A]] {
  implicit val EA: cats.Eq[A]
  def eqv(x: MatrixD[W, H, A], y: MatrixD[W, H, A]): Boolean = x === y
}

private trait Matrix4Group[A]
    extends Group[MatrixD[nat._4, nat._4, A]]
    with MultiplicativeSemigroup[MatrixD[nat._4, nat._4, A]] {

  def MA: MatrixAlgebra[nat._4, A]
  implicit def R: Ring[A]

  def inverse(m: MatrixD[nat._4, nat._4, A]): MatrixD[nat._4, nat._4, A] =
    MA.inverse(m)

  def id: MatrixD[nat._4, nat._4, A] = MatrixD.identity[nat._4, A]

  def op(m0: MatrixD[nat._4, nat._4, A],
         m1: MatrixD[nat._4, nat._4, A]): MatrixD[nat._4, nat._4, A] =
    times(m0, m1)

  def times(m0: MatrixD[nat._4, nat._4, A],
            m1: MatrixD[nat._4, nat._4, A]): MatrixD[nat._4, nat._4, A] =
    MA.times(m0, m1)
}


trait MatrixProduct[W <: Nat, H0 <: Nat, W1 <: Nat, A] {
  def prod(m0: MatrixD[W, H0, A], m1: MatrixD[W1, W, A]): MatrixD[W1, H0, A]
}

object MatrixProduct {
  implicit def squareProduct[N <: Nat, A](
      implicit ma: MatrixAlgebra[N, A])
    : MatrixProduct[N, N, N, A] = new SquareMatrixProduct[N, A] {
    def MA = ma
  }

  implicit def matVProduct[N <: Nat, A](
      implicit ma: MatrixAlgebra[N, A])
    : MatrixProduct[N, N, nat._1, A] = new MatNx1Product[N, A] {
    def MA = ma
  }
}

private trait SquareMatrixProduct[N <: Nat, A]
    extends MatrixProduct[N, N, N, A] {
  def MA: MatrixAlgebra[N, A]

  def prod(m0: MatrixD[N, N, A], m1: MatrixD[N, N, A]): MatrixD[N, N, A] =
    MA.times(m0, m1)
}

private trait MatNx1Product[N <: Nat, A]
    extends MatrixProduct[N, N, nat._1, A] {
  def MA: MatrixAlgebra[N, A]

  def prod(m0: MatrixD[N, N, A],
           m1: MatrixD[nat._1, N, A]): MatrixD[nat._1, N, A] =
    MA.timesv(m0, m1)
}

object MatrixAlgebra {
  implicit def matrix4Algebra[A: ConvertableFrom : ConvertableTo](implicit l: MatrixLibrary): MatrixAlgebra[nat._4, A]
  = new Matrix4Algebra[A] {
    val L = l
    val T = ConvertableTo[A]
    val F = ConvertableFrom[A]
  }
}

trait MatrixAlgebra[N <: Nat, A] {
  def times(m0: MatrixD[N, N, A], m1: MatrixD[N, N, A]): MatrixD[N, N, A]
  def inverse(m0: MatrixD[N, N, A]): MatrixD[N, N, A]
  def timesv(m0: MatrixD[N, N, A], m1: MatrixD[nat._1, N, A]): MatrixD[nat._1, N, A]
}

private trait Matrix4Algebra[A] extends MatrixAlgebra[nat._4, A] {
  def L: kernel.platform.MatrixLibrary
  def T: ConvertableTo[A]
  def F: ConvertableFrom[A]

  def times(m0: MatrixD[nat._4, nat._4, A],
            m1: MatrixD[nat._4, nat._4, A]): MatrixD[nat._4, nat._4, A] =
    new MatrixD(
        L.multiplyMM(m0.map(F.toFloat).toArray, m1.map(F.toFloat).toArray)
          .toVector
          .map(T.fromFloat),
        m0.width,
        m0.height)

 def inverse(m: MatrixD[nat._4, nat._4, A]): MatrixD[nat._4, nat._4, A] =
    new MatrixD(L.invertM(m.map(F.toFloat).toArray).toVector.map(T.fromFloat),
                m.width,
                m.height)

  def timesv(m0: MatrixD[nat._4, nat._4, A],
    m1: MatrixD[nat._1, nat._4, A]): MatrixD[nat._1, nat._4, A] =
    MatrixD.sized(
        nat._1,
        nat._4,
        L.multiplyMV(m0.map(F.toFloat).toArray, m1.map(F.toFloat).toArray)
          .toVector
          .map(T.fromFloat))
}
