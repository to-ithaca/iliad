package iliad

import spire._
import spire.math._
import spire.implicits._

import cats._
import cats.implicits._

import shapeless._
import shapeless.ops.nat._

import scala.reflect._

//TODO: Really think we should parameterize this?
final class VectorD[N <: Nat, A] private[iliad] (_unsized: Vector[A]) {

  import LTEq._
  import LT._

  private[iliad] def unsized = _unsized

  private[iliad] def toArray(implicit Ct: ClassTag[A]): Array[A] =
    unsized.toArray

  def apply(i: Nat)(implicit ev: i.N < N, toInt: ToInt[i.N]): A =
    unsized(toInt())

  def map[B](f: A => B): VectorD[N, B] = new VectorD(unsized map f)

  //Define map2, since the applicative requires ToInt[N] for pure
  def map2[B, C](that: VectorD[N, B])(f: (A, B) => C): VectorD[N, C] =
    that ap (this map f.curried)
  def ap[B](ff: VectorD[N, A => B]): VectorD[N, B] =
    new VectorD(this.unsized.zip(ff.unsized).map { case (a, f) => f(a) })
  def combine(that: VectorD[N, A])(implicit s: Semigroup[A]): VectorD[N, A] =
    map2(that)(_ |+| _)

  def n(implicit toInt: ToInt[N]): Int = toInt()

  //TODO: We really want the idea of basis as well?
  def x(implicit ev: nat._1 <= N): A = unsized(0)
  def y(implicit ev: nat._2 <= N): A = unsized(1)
  def z(implicit ev: nat._3 <= N): A = unsized(2)
  def w(implicit ev: nat._4 <= N): A = unsized(3)

  def ===[AA <: A](that: VectorD[N, AA])(implicit EA: Eq[A]): Boolean =
    unsized === that.unsized

  def +(that: VectorD[N, A])(
      implicit G: algebra.AdditiveSemigroup[A]): VectorD[N, A] =
    map2(that)(G.plus)
  def -(that: VectorD[N, A])(
      implicit G: algebra.AdditiveGroup[A]): VectorD[N, A] =
    map2(that)(G.minus)
  def *:(a: A)(implicit G: algebra.MultiplicativeSemigroup[A]): VectorD[N, A] =
    map(a * _)

  def ⋅(that: VectorD[N, A])(implicit G: algebra.Semiring[A]): A =
    map2(that)(_ * _).unsized.foldLeft(G.zero)(_ + _)
  def unary_-(implicit G: algebra.AdditiveGroup[A]): VectorD[N, A] = map(-_)

  def cross(that: VectorD[N, A])(implicit ev: N =:= nat._3,
                                 G: algebra.Rng[A]): VectorD[nat._3, A] =
    (unsized, that.unsized) match {
      case (Vector(u1, u2, u3), Vector(v1, v2, v3)) =>
        VectorD.sized(
            3,
            Vector(u2 * v3 - u3 * v2, u3 * v1 - u1 * v3, u1 * v2 - u2 * v1))
    }

  private def pad[D <: Nat](n: Nat, a: A)(
      implicit DD: Diff.Aux[n.N, N, D],
      toIntD: ToInt[D],
      toIntN: ToInt[n.N]): VectorD[n.N, A] =
    VectorD.sized(n, this.unsized ++ Vector.fill(toIntD())(a))

  def padZero[D <: Nat](n: Nat)(implicit G: algebra.AdditiveMonoid[A],
                                DD: Diff.Aux[n.N, N, D],
                                toIntD: ToInt[D],
                                toIntN: ToInt[n.N]): VectorD[n.N, A] =
    pad(n, G.zero)

  def padOne[D <: Nat](n: Nat)(implicit G: algebra.Ring[A],
                               DD: Diff.Aux[n.N, N, D],
                               toIntD: ToInt[D],
                               toIntN: ToInt[n.N]): VectorD[n.N, A] =
    pad(n, G.one)

  def dropUntil[D <: Nat](n: Nat)(implicit ev: n.N <= N,
                                  toIntN: ToInt[n.N]): VectorD[n.N, A] =
    VectorD.sized(n, this.unsized.take(toIntN()))

  def matrix(implicit toInt: ToInt[N]): MatrixD[nat._1, N, A] =
    MatrixD.sized[nat._1, N, A](unsized)

  override def toString: String = unsized.toString
}

object VectorD extends VectorDInstances {

  def zero[N <: Nat, A](implicit G: algebra.AdditiveMonoid[A],
                        toInt: ToInt[N]): VectorD[N, A] = fill(G.zero)
  def sized[N <: Nat, A](unsized: Vector[A])(
      implicit toInt: ToInt[N]): VectorD[N, A] =
    if (unsized.length < toInt())
      throw new IllegalArgumentException(
          s"vector $unsized is less than ${toInt()}")
    else new VectorD(unsized)

  def sized[A](i: Nat, unsized: Vector[A])(
      implicit toInt: ToInt[i.N]): VectorD[i.N, A] =
    sized[i.N, A](unsized)

  def fill[A](i: Nat, a: A)(implicit toInt: ToInt[i.N]): VectorD[i.N, A] =
    fill[i.N, A](a)
  def fill[N <: Nat, A](a: A)(implicit toInt: ToInt[N]): VectorD[N, A] =
    new VectorD(Vector.fill(toInt())(a))

  def zAxis[A](implicit R: algebra.Ring[A]): VectorD[nat._3, A] =
    VectorD.sized(3, Vector(R.zero, R.zero, R.one))
}

private[iliad] abstract class VectorDInstances extends VectorDInstances1 {

  //The type constraint makes it horrible to work out the `Unapply`
  implicit def vectorDUnapply[N <: Nat, AA, TC[_ [_]]](
      implicit tc: TC[VectorD[N, ?]])
    : Unapply.Aux1[TC, VectorD[N, AA], VectorD[N, ?], AA] =
    new Unapply[TC, VectorD[N, AA]] {
      type M[X] = VectorD[N, X]
      type A = AA
      override def TC: TC[VectorD[N, ?]] = tc
      override def subst: VectorD[N, AA] => M[A] = identity
    }

  implicit def vectorDEq[N <: Nat, A](implicit ea: Eq[A]): Eq[VectorD[N, A]] =
    new VectorDEq[N, A] { val EA = ea }

  implicit def vectorDSpireEq[N <: Nat, A](
      implicit ea: spire.algebra.Eq[A]): spire.algebra.Eq[VectorD[N, A]] =
    new VectorDSpireEq[N, A] { val EA = ea }

  implicit def vectorDSemigroup[N <: Nat, A](
      implicit sa: Semigroup[A]): Semigroup[VectorD[N, A]] =
    new VectorDSemigroup[N, A] { val SA = sa }
}

private[iliad] trait VectorDInstances1 {

  implicit def vectorDIsApplicative[N <: Nat](
      implicit toInt: ToInt[N]): Applicative[VectorD[N, ?]] =
    new VectorDIsApplicative[N] {
      val n: Int = toInt()
    }

  implicit def vectorDIsInnerProductSpace[N <: Nat, A](
      implicit fa: algebra.Field[A],
      toInt: ToInt[N]): algebra.InnerProductSpace[VectorD[N, A], A] =
    new VectorDIsInnerProductSpace[N, A] {
      val n = toInt()
      def scalar: algebra.Field[A] = fa
    }
}

private[iliad] sealed trait VectorDIsApplicative[N <: Nat]
    extends Applicative[VectorD[N, ?]] {
  val n: Int
  def pure[A](a: A): VectorD[N, A] = new VectorD(Vector.fill(n)(a))
  def ap[A, B](ff: VectorD[N, A => B])(fa: VectorD[N, A]): VectorD[N, B] =
    fa ap ff
}

private[iliad] sealed trait VectorDEq[N <: Nat, A] extends Eq[VectorD[N, A]] {
  implicit val EA: Eq[A]
  def eqv(x: VectorD[N, A], y: VectorD[N, A]): Boolean = x === y
}

private[iliad] sealed trait VectorDSpireEq[N <: Nat, A]
    extends spire.algebra.Eq[VectorD[N, A]] {
  implicit val EA: spire.algebra.Eq[A]
  def eqv(x: VectorD[N, A], y: VectorD[N, A]): Boolean =
    x.unsized === y.unsized
}

private[iliad] sealed trait VectorDSemigroup[N <: Nat, A]
    extends Semigroup[VectorD[N, A]] {
  implicit val SA: Semigroup[A]
  def combine(x: VectorD[N, A], y: VectorD[N, A]): VectorD[N, A] = x combine y
}

private[iliad] sealed trait VectorDIsInnerProductSpace[N <: Nat, A]
    extends algebra.InnerProductSpace[VectorD[N, A], A] {

  def n: Int
  def dot(x: VectorD[N, A], y: VectorD[N, A]): A = x ⋅ y
  def timesl(l: A, x: VectorD[N, A]): VectorD[N, A] = l *: x
  def negate(v: VectorD[N, A]): VectorD[N, A] = -v
  def zero: VectorD[N, A] = new VectorD(Vector.fill(n)(scalar.zero))
  def plus(x: VectorD[N, A], y: VectorD[N, A]): VectorD[N, A] = x + y
}
