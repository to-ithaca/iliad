package iliad
package algebra

import shapeless._
import shapeless.ops.nat._

import spire.math._
import spire.algebra._
import spire.implicits._

import scala.annotation
import scala.reflect._

import scala.{Vector => SVector}

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

private[algebra] final class Matrix4dAlgebra(floatAlgebra: Matrix4fAlgebra) extends SquareMatrixMultiplicativeGroup[Mat4d, Double] 
    with MatrixMultiplicativeGroup[Matrix, nat._4, nat._4, Double] {

  val scalar = spire.std.double.DoubleAlgebra

  def negate(x: Mat4d): Mat4d = -x

  val zero: Mat4d = Matrix.zero
  val id: Mat4d = Matrix.id
  def plus(x: Mat4d, y: Mat4d): Mat4d = x + y
  def transpose(x: Mat4d): Mat4d = x.transpose
  def timesl(r: Double, v: Mat4d): Mat4d = r *: v
  def trace(x: Mat4d): Double = x.trace
  def det(x: Mat4d): Double = ???
  def symmetric(x: Mat4d): Boolean = x.symmetric
  
  def product[N1 <: Nat](x: Mat4d, y: Matrix[N1, nat._4, Double])(implicit toInt: ToInt[N1]): Matrix[N1, nat._4, Double] =
    floatAlgebra.product(x.cmap[Float], y.cmap[Float]).cmap[Double]

  def times(x: Mat4d, y: Mat4d): Mat4d = product(x, y)
  def inverse(x: Mat4d): Mat4d = floatAlgebra.inverse(x.cmap[Float]).cmap[Double]
}

private[algebra] final class Matrix3fAlgebra(G: Matrix4fAlgebra) 
    extends SquareMatrixMultiplicativeGroup[Mat3f, Float]
    with MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, Float] {
  val scalar = spire.std.float.FloatAlgebra

  def negate(x: Mat3f): Mat3f = -x
  val zero: Mat3f = Matrix.zero
  val id: Mat3f = Matrix.id
  def plus(x: Mat3f, y: Mat3f): Mat3f = x + y
  def transpose(x: Mat3f): Mat3f = x.transpose
  def timesl(r: Float, v: Mat3f): Mat3f = r *: v
  def trace(x: Mat3f): Float = x.trace
  def det(x: Mat3f): Float = ???
  def symmetric(x: Mat3f): Boolean = x.symmetric
  
  def product[N1 <: Nat](x: Mat3f, y: Matrix[N1, nat._3, Float])(implicit toInt: ToInt[N1]): Matrix[N1, nat._3, Float] =
    G.product(x.pad(4, 4), y.pad(4, 4)).pad[N1, nat._3]

  def times(x: Mat3f, y: Mat3f): Mat3f = product(x, y)
  def inverse(x: Mat3f): Mat3f = G.inverse(x.pad(4, 4)).pad(3, 3)

}

private[algebra] final class Matrix3dAlgebra(G: Matrix3fAlgebra) 
    extends SquareMatrixMultiplicativeGroup[Mat3d, Double]
    with MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, Double] {
   
  val scalar = spire.std.double.DoubleAlgebra

  def negate(x: Mat3d): Mat3d = -x

  val zero: Mat3d = Matrix.zero
  val id: Mat3d = Matrix.id
  def plus(x: Mat3d, y: Mat3d): Mat3d = x + y
  def transpose(x: Mat3d): Mat3d = x.transpose
  def timesl(r: Double, v: Mat3d): Mat3d = r *: v
  def trace(x: Mat3d): Double = x.trace
  def det(x: Mat3d): Double = ???
  def symmetric(x: Mat3d): Boolean = x.symmetric
  
  def product[N1 <: Nat](x: Mat3d, y: Matrix[N1, nat._3, Double])(implicit toInt: ToInt[N1]): Matrix[N1, nat._3, Double] =
    G.product(x.cmap[Float], y.cmap[Float]).cmap[Double]

  def times(x: Mat3d, y: Mat3d): Mat3d = product(x, y)
  def inverse(x: Mat3d): Mat3d = G.inverse(x.cmap[Float]).cmap[Double]
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

  private def vector(N: Int, M: Int)(dm: DenseMatrix[Float]): SVector[Float] = {
    val builder = SVector.newBuilder[Float]
    builder.sizeHint(N * M)
    (0 until M).foreach { i =>
      (0 until N).foreach { j =>
        builder += dm(i, j)
      }
    }
    val v = builder.result()
    builder.clear()
    v
  }

  def product[N1 <: Nat](x: Mat4f, y: Matrix[N1, nat._4, Float])(implicit toInt: ToInt[N1]): Matrix[N1, nat._4, Float] = {
    val N1 = toInt()
    require(N1 < 5 && N1 > 0, s"not implemented multiplication greater than 4, found $N1")
    val dmx = DenseMatrix.create(4, 4, x.repr.toArray, 0, 4, true)
    val dmy = DenseMatrix.create(4, N1, y.repr.toArray, 0, N1, true)
    val rdm = dmx * dmy
    new Matrix(vector(N1, 4)(rdm))
  }

  def inverse(x: Mat4f): Mat4f = {
    val idm = breeze.linalg.inv[DenseMatrix[Float], DenseMatrix[Float]](DenseMatrix.create(4, 4, x.repr.toArray, 0, 4, true))
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
    new Matrix(r.toSVector)
  }

  def inverse(x: Mat4f): Mat4f = {
    val r = new Array[Float](16)
    val t = new Array[Float](16)
    Native.transposeM(t, 0, x.toArray, 0)
    Native.invertM(r, 0, t, 0)
    new Matrix(r.toSVector)
  }
#-android

  def times(x: Mat4f, y: Mat4f): Mat4f =
    product(x, y)
}

trait ||[A, B] {
  def a: Option[A]
  def b: Option[B]
}

//TODO: Migrate to shapeless
trait LowPriorityOrImplicits {
    implicit def aOrTpe[A, B](implicit eva: A): A || B = new ||[A, B] {
      val a = Some(eva)
      val b = None
    }
    implicit def bOrTpe[A, B](implicit evb: B): A || B = new ||[A, B] {
      val a = None
      val b = Some(evb)
    }
}

object || extends LowPriorityOrImplicits {
   implicit def abOrTpe[A, B](implicit eva: A, evb: B): A || B = new ||[A, B] {
    val a = Some(eva)
    val b = Some(evb)
  }
}


/** Matrix */
final class Matrix[N <: Nat, M <: Nat, A] private[iliad](val repr: SVector[A]) extends AnyVal {

  import LTEq._
  import LT._

  def apply(n: Nat, m: Nat)(
    implicit evN: n.N <= N, 
    evM: m.N <= M, 
    toIntN: ToInt[N], 
    toIntNN: ToInt[n.N], 
    toIntM: ToInt[m.N]): A = {
    repr(toIntN() * toIntNN() + toIntM())
  }

  def n(implicit toIntN: ToInt[N]): Int = toIntN()
  def m(implicit toIntM: ToInt[M]): Int = toIntM()

  def map[B](f: A => B): Matrix[N, M, B] = new Matrix(repr.map(f))
  def ap[B](ff: Matrix[N, M, A => B]): Matrix[N, M, B] =
    new Matrix(ff.repr.zip(repr).map { case (f, a) => f(a) })
  def map2[B, C](that: Matrix[N, M, B])(f: (A, B) => C): Matrix[N, M, C] = 
    that.ap(this.map(f.curried))

  def ortho(implicit G0: SquareMatrixMultiplicativeGroup[Matrix[N, N, A], A], G1: MatrixMultiplicativeGroup[Matrix, M, N, A], toIntN: ToInt[N], toIntM: ToInt[M], EqA: Eq[A], ev: N =:= M): Option[OrthoMatrix[N, A]] = {
    if((transpose * this) === G0.id)
      Some(new OrthoMatrix(repr))
    else
      None
  }

  def isOrtho(implicit G0: SquareMatrixMultiplicativeGroup[Matrix[N, N, A], A], G1: MatrixMultiplicativeGroup[Matrix, M, N, A], toIntN: ToInt[N], toIntM: ToInt[M], ev: N =:= M, EqA: Eq[A]): Boolean =
    ortho.isDefined

  def square(implicit ev: N =:= M): Matrix[N, N, A] = new Matrix(repr)

  def +(that: OrthoMatrix[N, A])(implicit G: AdditiveSemigroup[A], ev: N =:= M): Matrix[N, N, A] = 
    square.map2(that.matrix)(_ + _)
  def +(that: Matrix[N, M, A])(implicit G: AdditiveSemigroup[A], ev: DummyImplicit): Matrix[N, M, A] = 
    map2(that)(_ + _)
  def +(a: A)(implicit G: AdditiveSemigroup[A]): Matrix[N, M, A] = map(_ + a)


  def -(that: OrthoMatrix[N, A])(implicit G: AdditiveGroup[A], ev: N =:= M): Matrix[N, N, A] = square.map2(that.matrix)(_ - _)
  def -(that: Matrix[N, M, A])(implicit G: AdditiveGroup[A], ev: DummyImplicit): Matrix[N, M, A] =
    map2(that)(_ - _)
  def -(a: A)(implicit G: AdditiveGroup[A]): Matrix[N, M, A] = map(_ - a)

  def unary_-(implicit G: AdditiveGroup[A]): Matrix[N, M, A] = map(-_)

  def *:(a: A)(implicit G: MultiplicativeSemigroup[A]): Matrix[N, M, A] = map(_ * a)

  def *(that: OrthoMatrix[N, A])(implicit G: MultiplicativeSemigroup[Matrix[N, N, A]], ev: N =:= M): Matrix[N, N, A] = 
    G.times(this.square, that.matrix)
  def *(that: Matrix[N, M, A])(implicit G: MultiplicativeSemigroup[Matrix[N, M, A]], ev: DummyImplicit): Matrix[N, M, A] = 
    G.times(this, that)
  def *[N1 <: Nat](that: Matrix[N1, N, A])(implicit G: MatrixMultiplicativeGroup[Matrix, N, M, A], toInt: ToInt[N1]) =
    G.product(this, that)
  def *(that: OrthoMatrix[N, A])(implicit G: MatrixMultiplicativeGroup[Matrix, N, M, A], ev: DummyImplicit, toInt: ToInt[N]) =
    G.product(this, that.matrix)
  def /:(a: A)(implicit G: MultiplicativeGroup[A]): Matrix[N, M, A] = map(_ / a)

  //TODO: LU decomp
  def det(implicit G: SquareMatrixMultiplicativeGroup[Matrix[N, M, A], A]): A =
    G.det(this)

  def inverse(implicit G: SquareMatrixMultiplicativeGroup[Matrix[N, M, A], A]): Matrix[N, M, A] = G.inverse(this)

  def transpose(implicit toIntN: ToInt[N]): Matrix[M, N, A] = {
    val builder = SVector.newBuilder[A]
    val N = toIntN()
    val M = repr.size / N
    builder.sizeHint(N * M)

    (0 until N).foreach { i =>
      (0 until M).foreach { j =>
        builder += repr(i + j * N)
      }
    }
    val v = builder.result()
    builder.clear()
    new Matrix(v)
  }

  //TODO: separate out to those which don't need a Ring
  def pad(n: Nat, m: Nat)(implicit G: Ring[A], 
    toIntN: ToInt[N], 
    toIntNN: ToInt[n.N], 
    toIntM: ToInt[m.N]): Matrix[n.N, m.N, A] =
    pad[n.N, m.N]

  def pad[N1 <: Nat, M1 <: Nat](implicit G: Ring[A], toIntN: ToInt[N], toIntN1: ToInt[N1], toIntM1: ToInt[M1]): Matrix[N1, M1, A] = {
    val N = toIntN()
    val M = repr.size / N
    val N1 = toIntN1()
    val M1 = toIntM1()
    
    val builder = SVector.newBuilder[A]

    val z = G.zero
    val o = G.one

    def pad(i: Int, j: Int): Unit = if(i == j) 
      builder += o else builder += z

    def padN(N1: Int): Unit = {
      (0 until M).foreach { i =>
        val idx = i * N
        builder ++= repr.slice(idx, idx + N)
        (N until N1).foreach(pad(i, _))
      }
    }

    def padM(N1: Int, M1: Int): Unit = {
      (M until M1).foreach { i =>
        (0 until N1).foreach(pad(i, _))
      }
    }

    (N < N1, M < M1) match {
      case (true, true) =>
        builder.sizeHint(N1 * M1)
        padN(N1)
        padM(N1, M1)
      case (true, false) =>
        builder.sizeHint(N1 * M)
        padN(N1)
      case (false, true) =>   
        builder.sizeHint(N * M1)
        builder ++= repr
        padM(N, M1)
      case (false, false) =>
        val M1 = toIntM1()
        builder.sizeHint(N1 * M1)
        (0 until M1).foreach { i =>
          (0 until N1).foreach { j =>
            val idx = i * N
            builder ++= repr.slice(idx, idx + N1)
          }
        }
    }

    val v = builder.result()
    builder.clear()
    new Matrix(v)
  }

  def trace(implicit G: AdditiveMonoid[A], toIntN: ToInt[N], ev: N =:= M): A = {
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

  def *(v: Vector[N, A])(implicit G: MatrixMultiplicativeGroup[Matrix, N, M, A]): Vector[N, A] =
    new Vector((this * new Matrix[nat._1, N, A](v.repr)).repr)



  def show(implicit toIntN: ToInt[N]): String =
    repr.sliding(n, n)
      .map(row => row.mkString(" | "))
      .mkString(System.lineSeparator)
  
  //TODO: Change this
  override def toString: String = repr.toString

  def toArray(implicit classTag: ClassTag[A]): Array[A] = repr.toArray
}

abstract class MatrixInstances {
  implicit lazy val Matrix4fAlgebra: SquareMatrixMultiplicativeGroup[Mat4f, Float] 
      with MatrixMultiplicativeGroup[Matrix, nat._4, nat._4, Float] = new Matrix4fAlgebra

  implicit lazy val Matrix3fAlgebra: SquareMatrixMultiplicativeGroup[Mat3f, Float]
      with MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, Float] = new Matrix3fAlgebra(new Matrix4fAlgebra)

  implicit lazy val Matrix4DAlgebra: SquareMatrixMultiplicativeGroup[Mat4d, Double] 
      with MatrixMultiplicativeGroup[Matrix, nat._4, nat._4, Double] = new Matrix4dAlgebra(new Matrix4fAlgebra)

  implicit lazy val Matrix3DAlgebra: SquareMatrixMultiplicativeGroup[Mat3d, Double] 
      with MatrixMultiplicativeGroup[Matrix, nat._3, nat._3, Double] = 
    new Matrix3dAlgebra(new Matrix3fAlgebra(new Matrix4fAlgebra))

  implicit def matrixEq[A, N <: Nat, M <: Nat](implicit eqA: Eq[A]): Eq[Matrix[N, M, A]] = new Eq[Matrix[N, M, A]] {
    def eqv(x: Matrix[N, M, A], y: Matrix[N, M, A]): Boolean = x === y
  }

  implicit def matrixFunctor[N <: Nat, M <: Nat]: cats.Functor[Matrix[N, M, ?]] = new cats.Functor[Matrix[N, M, ?]] {
    def map[A, B](fa: Matrix[N, M, A])(f: A => B): Matrix[N, M, B] = fa.map(f)
  }
}

object Matrix extends MatrixInstances {
  def fill[A, N <: Nat, M <: Nat](a: A)(implicit toIntN: ToInt[N], toIntM: ToInt[M]): Matrix[N, M, A] = 
    new Matrix(SVector.fill(toIntN() * toIntM())(a))

  def zero[A, N <: Nat, M <: Nat](implicit R: Ring[A], toIntN: ToInt[N], toIntM: ToInt[M]): Matrix[N, M, A] = 
    fill(R.zero)

  def zero[A](n: Nat, m: Nat)(implicit R: Ring[A], toIntN: ToInt[n.N], toIntM: ToInt[m.N]): Matrix[n.N, m.N, A] =
    zero[A, n.N, m.N]

  def sized[A, N <: Nat, M <: Nat](v: SVector[A])(implicit toIntN: ToInt[N], toIntM: ToInt[M]): Matrix[N, M, A] = {
    val N = toIntN()
    val M = toIntM()
    require((N * M) == v.length, s"matrix $v does not have dimensions $N $M")
    new Matrix(v)
  }

  def sized[A](n: Nat, m: Nat)(v: SVector[A])(implicit toIntN: ToInt[n.N], toIntM: ToInt[m.N]): Matrix[n.N, m.N, A] =
    sized[A, n.N, m.N](v)

  def id[A, N <: Nat](implicit toInt: ToInt[N], R: Ring[A]): Matrix[N, N, A] = {
    val N = toInt()
    val NN = N * N
    val builder = SVector.newBuilder[A]
    builder.sizeHint(NN)
    val z = R.zero
    val o = R.one

    @annotation.tailrec
    def go(i: Int, n: Int): SVector[A] = {
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

  private[algebra] def apply[A, N <: Nat](matrix: Matrix[N, N, A]): OrthoMatrix[N, A] = new OrthoMatrix(matrix.repr)  
  def id[A, N <: Nat](implicit toInt: ToInt[N], R: Ring[A]): OrthoMatrix[N, A] = apply(Matrix.id[A, N])
  def id[A](n: Nat)(implicit toInt: ToInt[n.N], R: Ring[A]): OrthoMatrix[n.N, A] = id[A, n.N]

  def sized[A, N <: Nat](v: SVector[A])(implicit toIntN: ToInt[N]): OrthoMatrix[N, A] = apply(Matrix.sized[A, N, N](v))


  def sized[A](n: Nat)(v: SVector[A])(implicit toIntN: ToInt[n.N]): OrthoMatrix[n.N, A] =
    sized[A, n.N](v)


}

/** Orthogonal matrix */ //TODO: What to do in the case of 
final class OrthoMatrix[N <: Nat, A] private[iliad](val repr: SVector[A]) extends AnyVal {

  def pad[N1 <: Nat](implicit G: Ring[A], toIntN1: ToInt[N1], toIntN: ToInt[N]): OrthoMatrix[N, A] = 
    new OrthoMatrix(matrix.pad[N1, N1].repr)

  def pad(n: Nat)(implicit G: Ring[A], toIntN1: ToInt[n.N], toIntN: ToInt[N]): OrthoMatrix[N, A] = pad[n.N]

  def map[B](f: A => B): OrthoMatrix[N, B] = OrthoMatrix(matrix.map(f))
  def ap[B](ff: OrthoMatrix[N, A => B]): OrthoMatrix[N, B] =
    OrthoMatrix(matrix.ap(ff.matrix))
  def map2[B, C](that: OrthoMatrix[N, B])(f: (A, B) => C): OrthoMatrix[N, C] = 
    OrthoMatrix(matrix.map2(that.matrix)(f))
  
  def +(that: OrthoMatrix[N, A])(implicit G: AdditiveSemigroup[A]): Matrix[N, N, A] = 
    map2(that)(_ + _).matrix
  def +(that: Matrix[N, N, A])(implicit G: AdditiveSemigroup[A], ev: DummyImplicit): Matrix[N, N, A] = 
    matrix.map2(that)(_ + _)
  def +(a: A)(implicit G: AdditiveSemigroup[A]): OrthoMatrix[N, A] = map(_ + a)


  def -(that: OrthoMatrix[N, A])(implicit G: AdditiveGroup[A]): Matrix[N, N, A] = map2(that)(_ - _).matrix
  def -(that: Matrix[N, N, A])(implicit G: AdditiveGroup[A], ev: DummyImplicit): Matrix[N, N, A] =
    matrix.map2(that)(_ - _)
  def -(a: A)(implicit G: AdditiveGroup[A]): OrthoMatrix[N, A] = map(_ - a)

  def unary_-(implicit G: AdditiveGroup[A]): OrthoMatrix[N, A] = map(-_)

  def *:(a: A)(implicit G: MultiplicativeSemigroup[A]): OrthoMatrix[N, A] = map(_ * a)

  def *(that: OrthoMatrix[N, A])(implicit G: MultiplicativeSemigroup[Matrix[N, N, A]]): OrthoMatrix[N, A] = 
    new OrthoMatrix((G.times(this.matrix, that.matrix)).repr)
  def *(that: Matrix[N, N, A])(implicit G: MultiplicativeSemigroup[Matrix[N, N, A]], ev: DummyImplicit): Matrix[N, N, A] = 
    matrix * that

  def *(that: Vector[N, A])(implicit G: MatrixMultiplicativeGroup[Matrix, N, N, A], ev1: DummyImplicit, ev2: DummyImplicit): Vector[N, A] =
    matrix * that

  def *[N1 <: Nat: ToInt](that: Matrix[N1, N, A])(implicit G: MatrixMultiplicativeGroup[Matrix, N, N, A], ev: DummyImplicit) =
    matrix * that

  def /:(a: A)(implicit G: MultiplicativeGroup[A]): OrthoMatrix[N, A] = map(_ / a)

  //TODO: LU decomp
  def det(implicit G: SquareMatrixMultiplicativeGroup[OrthoMatrix[N, A], A]): A =
    G.det(this)

  def inverse(implicit G: SquareMatrixMultiplicativeGroup[OrthoMatrix[N, A], A]): OrthoMatrix[N, A] = G.inverse(this)

  def transpose(implicit toIntN: ToInt[N]): OrthoMatrix[N, A] = new OrthoMatrix(matrix.transpose.repr)

  def trace(implicit G: AdditiveMonoid[A], toIntN: ToInt[N]): A = matrix.trace

  def matrix: Matrix[N, N, A] = new Matrix(repr)

  def ===(that: OrthoMatrix[N, A])(implicit eqA: Eq[A]): Boolean = that.repr === repr

  def toArray(implicit classTag: ClassTag[A]): Array[A] = repr.toArray

  def symmetric(implicit eqA: Eq[A]): Boolean = matrix.symmetric
}

