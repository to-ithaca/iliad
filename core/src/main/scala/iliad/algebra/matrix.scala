package iliad
package algebra

import shapeless._
import shapeless.ops.nat._

import spire.math._
import spire.algebra._
import spire.implicits._

import scala.annotation
import scala.reflect._

trait MatrixMultiplicativeGroup[F[_ <: Nat, _ <: Nat, _], N <: Nat, M <: Nat, A] extends Module[F[N, M, A], A] {

  implicit def scalar: Field[A]
 
  def product[N1 <: Nat : ToInt](x: F[N, M, A], y: F[N1, N, A]): F[N1, M, A]
  def transpose(x: F[N, M, A]): F[M, N, A]
}

trait SquareMatrixMultiplicativeGroup[M, A] extends MultiplicativeSemigroup[M] with Module[M, A] {

  implicit def scalar: Field[A]

  def id: M
  def inverse(x: M): M
  def det(x: M): A
  def trace(x: M): A
  def symmetric(x: M): Boolean
}


private[algebra] final class Matrix4fAlgebra 
    extends SquareMatrixMultiplicativeGroup[Mat4f, Float]
    with MatrixMultiplicativeGroup[Matrix, nat._4, nat._4, Float] {

#+android
import android.opengl.{Matrix => Native}
#-android

#+desktop
import breeze.linalg.DenseMatrix
#-desktop


  val scalar = spire.std.float.FloatAlgebra

  def negate(x: Mat4f): Mat4f = -x

  val zero: Mat4f = Matrix.zero
  val id: Mat4f = Matrix.id

  def plus(x: Mat4f, y: Mat4f): Mat4f = x + y
  def transpose(x: Mat4f): Mat4f = x.transpose
  def timesl(r: Float, v: Mat4f): Mat4f = r *: v
  def trace(x: Mat4f): Float = x.trace
  def det(x: Mat4f): Float = ???
  def symmetric(x: Mat4f): Boolean = x.symmetric

#+desktop

  private def dm4f(v: Vector[Float]): DenseMatrix[Float] =
    DenseMatrix.create(4, 4, v.toArray)

  private def vector(N: Int, M: Int)(dm: DenseMatrix[Float]): Vector[Float] = {
    val builder = Vector.newBuilder[Float]
    builder.sizeHint(N * M)
    (0 until N).foreach { i =>
      (0 until M).foreach { j =>
        builder += dm(j, i)
      }
    }
    val v = builder.result()
    builder.clear()
    v
  }

  def product[N1 <: Nat](x: Mat4f, y: Matrix[N1, nat._4, Float])(implicit toInt: ToInt[N1]): Matrix[N1, nat._4, Float] = {
    val N1 = toInt()
    require(N1 < 5 && N1 > 0, s"not implemented multiplication greater than 4, found $N1")
    val dmy = N1 match {
      case 1 => 
        DenseMatrix.create(4, 1, y.repr.toArray)
        //DenseMatrix((y.repr(0), y.repr(1), y.repr(2), y.repr(3)))
      case 2 => DenseMatrix(
        (y.repr(0), y.repr(1), y.repr(2), y.repr(3)),
        (y.repr(4), y.repr(5), y.repr(6), y.repr(7))
      )
      case 3 => DenseMatrix(
        (y.repr(0), y.repr(1), y.repr(2), y.repr(3)),
        (y.repr(4), y.repr(5), y.repr(6), y.repr(7)),
        (y.repr(8), y.repr(9), y.repr(10), y.repr(11))
      )    
      case 4 => dm4f(y.repr)
    }
    val dmx = dm4f(x.repr)
    val rdm = dmx * dmy
    new Matrix(vector(N1, 4)(rdm))
  }

  def inverse(x: Mat4f): Mat4f = {
    val idm = breeze.linalg.inv[DenseMatrix[Float], DenseMatrix[Float]](dm4f(x.repr))
    new Matrix(vector(4, 4)(idm))
  }
#-desktop

#+android  
  def product[N1 <: Nat](x: Mat4f, y: Matrix[N1, nat._4, Float])(implicit toInt: ToInt[N1]): Matrix[N1, nat._4, Float] = {
    val N1 = toInt()
    require(N1 < 5, s"not implemented multiplication greater than 4, found $N1")
    val r = if(N1 < 1) {
      val m0 = x.toArray
      val m1 = if(N1 != 4) {
        val mtmp = Array.fill(16)(0f)
        val m1 = y.toArray
        (0 until N1).foreach { i =>
          Array.copy(m1, i * N1, mtmp, i * 4, N1)
        }
        //fill with 1 for each of the extra dimensions
        (N1 until 4).foreach { i =>
          mtmp(i * 5) = 1f
        }
        mtmp
      } else y.toArray
      val r0 = new Array[Float](16)
      val t = new Array[Float](32)
      Native.transposeM(t, 0, m0, 0)
      Native.transposeM(t, 16, m1, 0)
      Native.multiplyMM(r0, 0, t, 0, t, 16)
      val r = new Array[Float](N1 * 4)
      (0 until N1).foreach { i =>
        Array.copy(r, i * 4, r, i * N1, N1)
      }
      r
    } else {
      val m0 = x.toArray
      val m1 = y.toArray
      val t = new Array[Float](16)
      val r = new Array[Float](4)
      Native.transposeM(t, 0, m0, 0)
      Native.multiplyMV(r, 0, t, 0, m1, 0)
      r
    }
    new Matrix(r.toVector)
  }

  def inverse(x: Mat4f): Mat4f = {
    val r = new Array[Float](16)
    val t = new Array[Float](16)
    Native.transposeM(t, 0, x.toArray, 0)
    Native.invertM(r, 0, t, 0)
    new Matrix(r.toVector)
  }
#-android

  def times(x: Mat4f, y: Mat4f): Mat4f =
    product(x, y)
}

/** Matrix */
final class Matrix[N <: Nat, M <: Nat, A] private[iliad](val repr: Vector[A]) extends AnyVal {

  import LTEq._

  def apply(n: Nat, m: Nat)(
    implicit evN: n.N <= N, 
    evM: m.N <= M, 
    toIntN: ToInt[N], 
    toIntNN: ToInt[n.N], 
    toIntM: ToInt[m.N]): A = {
    repr(toIntN() * toIntNN() + toIntM())
  }

  def map[B](f: A => B): Matrix[N, M, B] = new Matrix(repr.map(f))
  def ap[B](ff: Matrix[N, M, A => B]): Matrix[N, M, B] =
    new Matrix(ff.repr.zip(repr).map { case (f, a) => f(a) })
  def map2[B, C](that: Matrix[N, M, B])(f: (A, B) => C): Matrix[N, M, C] = 
    that.ap(this.map(f.curried))

  def ortho(implicit G0: SquareMatrixMultiplicativeGroup[Matrix[N, M, A], A], G1: MatrixMultiplicativeGroup[Matrix, N, M, A], toIntN: ToInt[N], toIntM: ToInt[M]): Option[OrthoMatrix[N, M, A]] = {
    if((this * transpose) == G0.id)
      Some(new OrthoMatrix(repr))
    else
      None
  }

  def +(that: OrthoMatrix[N, M, A])(implicit G: AdditiveSemigroup[A]): Matrix[N, M, A] = 
    map2(that.matrix)(_ + _)
  def +(that: Matrix[N, M, A])(implicit G: AdditiveSemigroup[A], ev: DummyImplicit): Matrix[N, M, A] = 
    map2(that)(_ + _)
  def +(a: A)(implicit G: AdditiveSemigroup[A]): Matrix[N, M, A] = map(_ + a)


  def -(that: OrthoMatrix[N, M, A])(implicit G: AdditiveGroup[A]): Matrix[N, M, A] = map2(that.matrix)(_ - _)
  def -(that: Matrix[N, M, A])(implicit G: AdditiveGroup[A], ev: DummyImplicit): Matrix[N, M, A] =
    map2(that)(_ - _)
  def -(a: A)(implicit G: AdditiveGroup[A]): Matrix[N, M, A] = map(_ - a)

  def unary_-(implicit G: AdditiveGroup[A]): Matrix[N, M, A] = map(-_)

  def *:(a: A)(implicit G: MultiplicativeSemigroup[A]): Matrix[N, M, A] = map(_ * a)

  def *(that: OrthoMatrix[N, M, A])(implicit G: MultiplicativeSemigroup[Matrix[N, M, A]]): Matrix[N, M, A] = 
    G.times(this, that.matrix)
  def *(that: Matrix[N, M, A])(implicit G: MultiplicativeSemigroup[Matrix[N, M, A]], ev: DummyImplicit): Matrix[N, M, A] = 
    G.times(this, that)
  def *[N1 <: Nat](that: Matrix[N1, N, A])(implicit G: MatrixMultiplicativeGroup[Matrix, N, M, A], toInt: ToInt[N1]) =
    G.product(this, that)
  def *[N1 <: Nat: ToInt](that: OrthoMatrix[N1, N, A])(implicit G: MatrixMultiplicativeGroup[Matrix, N, M, A], ev: DummyImplicit) =
    G.product(this, that.matrix)
  def /:(a: A)(implicit G: MultiplicativeGroup[A]): Matrix[N, M, A] = map(_ / a)

  //TODO: LU decomp
  def det(implicit G: SquareMatrixMultiplicativeGroup[Matrix[N, M, A], A]): A =
    G.det(this)

  def inverse(implicit G: SquareMatrixMultiplicativeGroup[Matrix[N, M, A], A]): Matrix[N, M, A] = G.inverse(this)

  def transpose(implicit toIntN: ToInt[N]): Matrix[M, N, A] = {
    val builder = Vector.newBuilder[A]
    val N = toIntN()
    val M = repr.size / N
    builder.sizeHint(N * M)
    @annotation.tailrec
    def go(i: Int, j: Int): Vector[A] = {
      if(j < M) {
        builder += repr(i + j * N)
        val ii = i + 1
        if(ii < N)
          go(i + 1, j)
        else
          go(0, j + 1)
     
      } else {
        val v = builder.result()
        builder.clear()
        v
      }
    }
    new Matrix(go(0, 0))
  }

  def pad[N1 <: Nat, M1 <: Nat](implicit G: AdditiveGroup[A]): Matrix[N1, M1, A] = {
    ???
  }

  def trace(implicit G: AdditiveMonoid[A], toIntN: ToInt[N]): A = {
    val N = toIntN()
    val M = repr.size / N
    (0 until N).foldLeft(G.zero) { (b, i) =>
      (0 until M).foldLeft(G.zero) { (bb, j) =>
        repr(i + j * N) + bb
      } + b
    }
  }

  def ===(that: Matrix[N, M, A])(implicit eqA: Eq[A]): Boolean = that.repr === repr

  def symmetric(implicit eqA: Eq[A], ev: N =:= M): Boolean = {
    val N = Math.sqrt(repr.size.toFloat).toInt
    @annotation.tailrec
    def go(i: Int, j: Int): Boolean = {
      if(j < N) {
        if(i < N) {
          if(repr(i + j * N) === repr(j + i * N))
            go(i, j + 1)
          else false
        } else go(i + 1, j)
      } else true
    }
    go(0, 0)
  }

  //TODO: Change this
  override def toString: String = repr.toString

  def toArray(implicit classTag: ClassTag[A]): Array[A] = repr.toArray
}

abstract class MatrixInstances {
  lazy implicit val Matrix4fAlgebra: SquareMatrixMultiplicativeGroup[Mat4f, Float] 
      with MatrixMultiplicativeGroup[Matrix, nat._4, nat._4, Float] = new Matrix4fAlgebra
}

object Matrix extends MatrixInstances {
  def fill[A, N <: Nat, M <: Nat](a: A)(implicit toIntN: ToInt[N], toIntM: ToInt[M]): Matrix[N, M, A] = 
    new Matrix(Vector.fill(toIntN() * toIntM())(a))

  def zero[A, N <: Nat, M <: Nat](implicit R: Ring[A], toIntN: ToInt[N], toIntM: ToInt[M]): Matrix[N, M, A] = 
    fill(R.zero)

  def zero[A](n: Nat, m: Nat)(implicit R: Ring[A], toIntN: ToInt[n.N], toIntM: ToInt[m.N]): Matrix[n.N, m.N, A] =
    zero[A, n.N, m.N]

  def sized[A, N <: Nat, M <: Nat](v: Vector[A])(implicit toIntN: ToInt[N], toIntM: ToInt[M]): Matrix[N, M, A] = {
    val N = toIntN()
    val M = toIntM()
    require((N * M) == v.length, s"matrix $v does not have dimensions $N $M")
    new Matrix(v)
  }

  def sized[A](n: Nat, m: Nat)(v: Vector[A])(implicit toIntN: ToInt[n.N], toIntM: ToInt[m.N]): Matrix[n.N, m.N, A] =
    sized[A, n.N, m.N](v)

  def id[A, N <: Nat](implicit toInt: ToInt[N], R: Ring[A]): Matrix[N, N, A] = {
    val N = toInt()
    val NN = N * N
    val builder = Vector.newBuilder[A]
    builder.sizeHint(NN)
    val z = R.zero
    val o = R.one

    @annotation.tailrec
    def go(i: Int, n: Int): Vector[A] = {
      if(i < NN) {
        if(i == n) {
          builder += o
          go(i + 1, n + N + 1)
        } else {
          builder += z
          go(i + 1, n)
        }       
      } else {
        val v = builder.result()
        builder.clear()
        v
      }
    }
    new Matrix(go(0, 0))
  }

  def id[A](n: Nat)(implicit toInt: ToInt[n.N], R: Ring[A]): Matrix[n.N, n.N, A] = id[A, n.N]
}

object OrthoMatrix {

  private[algebra] def apply[A, N <: Nat, M <: Nat](matrix: Matrix[N, M, A]): OrthoMatrix[N, M, A] = new OrthoMatrix(matrix.repr)  
  def id[A, N <: Nat](implicit toInt: ToInt[N], R: Ring[A]): OrthoMatrix[N, N, A] = apply(Matrix.id[A, N])
  def id[A](n: Nat)(implicit toInt: ToInt[n.N], R: Ring[A]): OrthoMatrix[n.N, n.N, A] = id[A, n.N]

  def sized[A, N <: Nat, M <: Nat](v: Vector[A])(implicit toIntN: ToInt[N], toIntM: ToInt[M]): OrthoMatrix[N, M, A] = apply(Matrix.sized[A, N, M](v))


  def sized[A](n: Nat, m: Nat)(v: Vector[A])(implicit toIntN: ToInt[n.N], toIntM: ToInt[m.N]): OrthoMatrix[n.N, m.N, A] =
    sized[A, n.N, m.N](v)


}

/** Orthogonal matrix */ //TODO: What to do in the case of 
final class OrthoMatrix[N <: Nat, M <: Nat, A] private[iliad](val repr: Vector[A]) extends AnyVal {

  def map[B](f: A => B): OrthoMatrix[N, M, B] = OrthoMatrix(matrix.map(f))
  def ap[B](ff: OrthoMatrix[N, M, A => B]): OrthoMatrix[N, M, B] =
    OrthoMatrix(matrix.ap(ff.matrix))
  def map2[B, C](that: OrthoMatrix[N, M, B])(f: (A, B) => C): OrthoMatrix[N, M, C] = 
    OrthoMatrix(matrix.map2(that.matrix)(f))
  
  def +(that: OrthoMatrix[N, M, A])(implicit G: AdditiveSemigroup[A]): Matrix[N, M, A] = 
    map2(that)(_ + _).matrix
  def +(that: Matrix[N, M, A])(implicit G: AdditiveSemigroup[A], ev: DummyImplicit): Matrix[N, M, A] = 
    matrix.map2(that)(_ + _)
  def +(a: A)(implicit G: AdditiveSemigroup[A]): OrthoMatrix[N, M, A] = map(_ + a)


  def -(that: OrthoMatrix[N, M, A])(implicit G: AdditiveGroup[A]): Matrix[N, M, A] = map2(that)(_ - _).matrix
  def -(that: Matrix[N, M, A])(implicit G: AdditiveGroup[A], ev: DummyImplicit): Matrix[N, M, A] =
    matrix.map2(that)(_ - _)
  def -(a: A)(implicit G: AdditiveGroup[A]): OrthoMatrix[N, M, A] = map(_ - a)

  def unary_-(implicit G: AdditiveGroup[A]): OrthoMatrix[N, M, A] = map(-_)

  def *:(a: A)(implicit G: MultiplicativeSemigroup[A]): OrthoMatrix[N, M, A] = map(_ * a)

  def *(that: OrthoMatrix[N, M, A])(implicit G: MultiplicativeSemigroup[OrthoMatrix[N, M, A]]): OrthoMatrix[N, M, A] = 
    G.times(this, that)
  def *(that: Matrix[N, M, A])(implicit G: MultiplicativeSemigroup[Matrix[N, M, A]], ev: DummyImplicit): Matrix[N, M, A] = 
    matrix * that
  def *[N1 <: Nat: ToInt](that: OrthoMatrix[N1, N, A])(implicit G: MatrixMultiplicativeGroup[OrthoMatrix, N, M, A]) =
    G.product(this, that)
  def *[N1 <: Nat: ToInt](that: Matrix[N1, N, A])(implicit G: MatrixMultiplicativeGroup[Matrix, N, M, A], ev: DummyImplicit) =
    matrix * that

  def /:(a: A)(implicit G: MultiplicativeGroup[A]): OrthoMatrix[N, M, A] = map(_ / a)

  //TODO: LU decomp
  def det(implicit G: SquareMatrixMultiplicativeGroup[OrthoMatrix[N, M, A], A]): A =
    G.det(this)

  def inverse(implicit G: SquareMatrixMultiplicativeGroup[OrthoMatrix[N, M, A], A]): OrthoMatrix[N, M, A] = G.inverse(this)

  def transpose(implicit toIntN: ToInt[N]): OrthoMatrix[M, N, A] = new OrthoMatrix(matrix.transpose.repr)

  def trace(implicit G: AdditiveMonoid[A], toIntN: ToInt[N]): A = matrix.trace

  def matrix: Matrix[N, M, A] = new Matrix(repr)

  def ===(that: OrthoMatrix[N, M, A])(implicit eqA: Eq[A]): Boolean = that.repr === repr

  def toArray(implicit classTag: ClassTag[A]): Array[A] = repr.toArray

  def symmetric(implicit eqA: Eq[A], ev: N =:= M): Boolean = matrix.symmetric

}


object Test {
  def main(args: Array[String]): Unit = {
    val m = Matrix.id[Float, nat._4]
    implicit val G = new Matrix4fAlgebra
    println((5 *: m).repr)
  }
}
