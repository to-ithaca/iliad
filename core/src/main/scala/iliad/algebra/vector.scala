package iliad
package algebra

import scala.{Vector => SVector}

import spire._
import spire.algebra._
import spire.math._

import spire.implicits._

import shapeless._
import shapeless.ops.nat._

import scala.reflect._

final class Vector[N <: Nat, A](val repr: SVector[A]) extends AnyVal {
  
  import LT._
  import LTEq._

  def apply[NN <: Nat](implicit ev: NN < N, toInt: ToInt[NN]): A = 
    repr(toInt())

  def apply(n: Nat)(implicit ev: n.N < N, toInt: ToInt[n.N]): A = apply[n.N]

  def n = repr.size

  def map[B](f: A => B): Vector[N, B] = new Vector(repr.map(f))

  def ap[B](ff: Vector[N, A => B]): Vector[N, B] = 
    new Vector(repr.zip(ff.repr).map { case (a, f) => f(a) })

  def map2[B, C](that: Vector[N, B])(f: (A, B) => C): Vector[N, C] = 
    that.ap(map(f.curried))

  def updated(i: Nat, a: A)(implicit ev: i.N < N,
                           toInt: ToInt[i.N]): Vector[N, A] =
    updated[i.N](a)

  def updated[NN <: Nat](a: A)(implicit ev: NN < N,
                           toInt: ToInt[NN]): Vector[N, A] =
    new Vector(repr.updated(toInt(), a))


  def exists(f: A => Boolean): Boolean = repr.exists(f)

  def forall(f: A => Boolean): Boolean = repr.forall(f)

  def foldLeft[B](b: B)(f: (B, A) => B): B =
    repr.foldLeft(b)(f)

  def x(implicit ev: nat._1 <= N): A = repr(0)
  def y(implicit ev: nat._2 <= N): A = repr(1)
  def z(implicit ev: nat._3 <= N): A = repr(2)
  def w(implicit ev: nat._4 <= N): A = repr(3)

  def r(implicit ev: nat._1 <= N): A = repr(0)
  def θ(implicit ev: nat._2 <= N): A = repr(1)
  def ϕ(implicit ev: nat._3 <= N): A = repr(2)

  def ===(that: Vector[N, A])(implicit EA: Eq[A]): Boolean =
    repr === that.repr

  def +(that: Vector[N, A])(implicit G: AdditiveSemigroup[A]): Vector[N, A] =
    map2(that)(_ + _)

  def +(a: A)(implicit G: AdditiveSemigroup[A]): Vector[N, A] =
    map(_ + a)

  def -(that: Vector[N, A])(implicit G: AdditiveGroup[A]): Vector[N, A] =
    map2(that)(_ - _)

  def *:(a: A)(implicit G: MultiplicativeSemigroup[A]): Vector[N, A] =
    map(a * _)

  def -(a: A)(implicit G: AdditiveGroup[A]): Vector[N, A] =
    map(_ - a)

  def ⋅(that: Vector[N, A])(implicit G: Semiring[A]): A =
    map2(that)(_ * _).repr.foldLeft(G.zero)(_ + _)

  def :+(a: A): Vector[Succ[N], A] = new Vector(repr :+ a)


  def pad[D <: Nat](n: Nat, a: A)(
      implicit DD: Diff.Aux[n.N, N, D],
      toIntD: ToInt[D]): Vector[n.N, A] =
    new Vector[n.N, A](repr ++ SVector.fill(toIntD())(a))

  def padZero[D <: Nat](n: Nat)(implicit G: AdditiveMonoid[A],
                                DD: Diff.Aux[n.N, N, D],
                                toIntD: ToInt[D],
                                toIntN: ToInt[n.N]): Vector[n.N, A] =
    pad(n, G.zero)

  def padOne[D <: Nat](n: Nat)(implicit G: Ring[A],
                               DD: Diff.Aux[n.N, N, D],
                               toIntD: ToInt[D],
                               toIntN: ToInt[n.N]): Vector[n.N, A] = pad(n, G.one)


  def unary_-(implicit G: AdditiveGroup[A]): Vector[N, A] = map(-_)


  def dropUntil[D <: Nat](n: Nat)(implicit ev: n.N <= N,
                                  toInt: ToInt[n.N]): Vector[n.N, A] =
    new Vector(repr.take(toInt()))

  def ×(that: Vector[N, A])(implicit ev: N =:= nat._3, G: Rng[A]): Vector[nat._3, A] = {
    val SVector(u1, u2, u3) = repr
    val SVector(v1, v2, v3) = that.repr
    new Vector(SVector((u2 * v3) - (u3 * v2), (u3 * v1) - (u1 * v3), (u1 * v2) - (u2 * v1)))
  }

  def toArray(implicit classTag: ClassTag[A]): Array[A] = repr.toArray

  override def toString: String = repr.toString
}

object Vector extends VectorInstances {

  import LT._

  def fill[N <: Nat, A](a: A)(implicit toInt: ToInt[N]): Vector[N, A] = new Vector(SVector.fill(toInt())(a))
  def fill[A](n: Nat)(a: A)(implicit toInt: ToInt[n.N]): Vector[n.N, A] = fill[n.N, A](a)
  def zero[N <: Nat, A](implicit G: AdditiveGroup[A], toInt: ToInt[N]): Vector[N, A] = fill[N, A](G.zero)
  def zero[A](n: Nat)(implicit G: AdditiveGroup[A], toInt: ToInt[n.N]): Vector[n.N, A] = zero[n.N, A]
  def sized[N <: Nat, A](repr: SVector[A])(implicit toInt :ToInt[N]): Vector[N, A] = {
    val N = toInt()
    require(N == repr.size, s"expected size $N is not the same as ${repr.length}")
    new Vector(repr)
  }
  def sized[A](n: Nat)(repr: SVector[A])(implicit toInt: ToInt[n.N]): Vector[n.N, A] = sized[n.N, A](repr)

  def basis[B <: Nat, N <: Nat, A](implicit R: Ring[A], ev: B < N, toIntB: ToInt[B], toIntN: ToInt[N]): Vector[N, A] = 
    zero[N, A].updated[B](R.one)

  def basis[A](b: Nat, n: Nat)(implicit R: Ring[A], ev: b.N < n.N, toIntB: ToInt[b.N], toIntN: ToInt[n.N]): Vector[n.N, A] =
    basis[b.N, n.N, A]
}

private[algebra] sealed trait VectorEq[N <: Nat, A] extends Eq[Vector[N, A]] {
  implicit def EqA: Eq[A]
  def eqv(x: Vector[N, A], y: Vector[N, A]): Boolean = x === y
}

abstract class VectorInstances extends VectorInstances0 {
  implicit def vectorEq[N <: Nat, A](implicit E: Eq[A]): Eq[Vector[N, A]] = new VectorEq[N, A] {
    def EqA = E
  }

  implicit lazy val vec2fNormedVectorSpace: NormedVectorSpace[Vec2f, Float] = vec2fInnerProductSpace.normed
  implicit lazy val vec3fNormedVectorSpace: NormedVectorSpace[Vec3f, Float] = vec3fInnerProductSpace.normed
  implicit lazy val vec2dNormedVectorSpace: NormedVectorSpace[Vec2d, Double] = vec2dInnerProductSpace.normed
  implicit lazy val vec3dNormedVectorSpace: NormedVectorSpace[Vec3d, Double] = vec3dInnerProductSpace.normed

  implicit def vectorFunctor[N <: Nat]: cats.Functor[Vector[N, ?]] = new cats.Functor[Vector[N, ?]] {
    def map[A, B](fa: Vector[N, A])(f: A => B): Vector[N, B] = fa.map(f)
  }
}

sealed trait VectorInstances0 {

  implicit lazy val vec2fInnerProductSpace: InnerProductSpace[Vec2f, Float] = 
    new VectorInnerProductSpace[nat._2, Float] {
      def scalar = FloatAlgebra
      def toInt = ToInt[nat._2]
    }

  implicit lazy val vec3fInnerProductSpace: InnerProductSpace[Vec3f, Float] = 
    new VectorInnerProductSpace[nat._3, Float] {
      def scalar = FloatAlgebra
      def toInt = ToInt[nat._3]
    }

  implicit lazy val vec2dInnerProductSpace: InnerProductSpace[Vec2d, Double] = 
    new VectorInnerProductSpace[nat._2, Double] {
      def scalar = DoubleAlgebra
      def toInt = ToInt[nat._2]
    }

  implicit lazy val vec3dInnerProductSpace: InnerProductSpace[Vec3d, Double] = 
    new VectorInnerProductSpace[nat._3, Double] {
      def scalar = DoubleAlgebra
      def toInt = ToInt[nat._3]
    }
}


private[algebra] sealed trait VectorInnerProductSpace[N <: Nat, A] extends InnerProductSpace[Vector[N, A], A] {

  implicit def toInt: ToInt[N]

  def dot(x: Vector[N, A], y: Vector[N, A]): A =  x ⋅ y
  def timesl(l: A, x: Vector[N, A]): Vector[N, A] = l *: x
  def negate(v: Vector[N, A]): Vector[N, A] = -v
  val zero: Vector[N, A] = Vector.zero[N, A]
  def plus(x: Vector[N, A], y: Vector[N, A]): Vector[N, A] = x + y
}


