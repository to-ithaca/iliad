package iliad

import spire.{algebra => spa, math => spm}
import spire.implicits._

import cats._
import cats.implicits._

import shapeless._
import shapeless.ops.nat._

import scala.reflect._

//TODO: Really think we should parameterize this?
case class VectorD[N <: Nat, A] private[iliad] (_unsized: Vector[A]) {

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

  def update(i: Nat, a: A)(implicit ev: i.N < N,
                           toInt: ToInt[i.N]): VectorD[N, A] =
    new VectorD(unsized.updated(toInt(), a))

  def exists(f: A => Boolean): Boolean = unsized.exists(f)

  def forall(f: A => Boolean): Boolean = unsized.forall(f)

  def foldLeft[B](b: B)(f: (B, A) => B): B =
    unsized.foldLeft(b)(f)

  def foldRight[B](lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable[Vector].foldRight(unsized, lb)(f)

  def n(implicit toInt: ToInt[N]): Int = toInt()

  def x(implicit ev: nat._1 <= N): A = unsized(0)
  def y(implicit ev: nat._2 <= N): A = unsized(1)
  def z(implicit ev: nat._3 <= N): A = unsized(2)
  def w(implicit ev: nat._4 <= N): A = unsized(3)

  def ===[AA <: A](that: VectorD[N, AA])(implicit EA: Eq[A]): Boolean =
    unsized === that.unsized

  def +(that: VectorD[N, A])(
      implicit G: spa.AdditiveSemigroup[A]): VectorD[N, A] =
    map2(that)(G.plus)
  def -(that: VectorD[N, A])(implicit G: spa.AdditiveGroup[A]): VectorD[N, A] =
    map2(that)(G.minus)
  def *:(a: A)(implicit G: spa.MultiplicativeSemigroup[A]): VectorD[N, A] =
    map(a * _)
  def -(a: A)(implicit G: spa.AdditiveGroup[A]): VectorD[N, A] =
    map(_ - a)

  def ⋅(that: VectorD[N, A])(implicit G: spa.Semiring[A]): A =
    map2(that)(_ * _).unsized.foldLeft(G.zero)(_ + _)
  def unary_-(implicit G: spa.AdditiveGroup[A]): VectorD[N, A] = map(-_)

  def cross(that: VectorD[N, A])(implicit ev: N =:= nat._3,
                                 G: spa.Rng[A]): VectorD[nat._3, A] =
    (unsized, that.unsized) match {
      case (Vector(u1, u2, u3), Vector(v1, v2, v3)) =>
        VectorD.sized(
            3,
            Vector(u2 * v3 - u3 * v2, u3 * v1 - u1 * v3, u1 * v2 - u2 * v1))
    }

  def scale(that: VectorD[N, A])(
      implicit G: spa.MultiplicativeSemigroup[A]): VectorD[N, A] =
    map2(that)(_ * _)

  private def pad[D <: Nat](n: Nat, a: A)(
      implicit DD: Diff.Aux[n.N, N, D],
      toIntD: ToInt[D],
      toIntN: ToInt[n.N]): VectorD[n.N, A] =
    VectorD.sized(n, this.unsized ++ Vector.fill(toIntD())(a))

  def padZero[D <: Nat](n: Nat)(implicit G: spa.AdditiveMonoid[A],
                                DD: Diff.Aux[n.N, N, D],
                                toIntD: ToInt[D],
                                toIntN: ToInt[n.N]): VectorD[n.N, A] =
    pad(n, G.zero)

  def padOne[D <: Nat](n: Nat)(implicit G: spa.Ring[A],
                               DD: Diff.Aux[n.N, N, D],
                               toIntD: ToInt[D],
                               toIntN: ToInt[n.N]): VectorD[n.N, A] =
    pad(n, G.one)

  def dropUntil[D <: Nat](n: Nat)(implicit ev: n.N <= N,
                                  toIntN: ToInt[n.N]): VectorD[n.N, A] =
    VectorD.sized(n, this.unsized.take(toIntN()))

  def matrix(implicit toInt: ToInt[N]): MatrixD[nat._1, N, A] =
    MatrixD.sized[nat._1, N, A](unsized)

  def as[B: spm.ConvertableTo](
      implicit F: spm.ConvertableFrom[A]): VectorD[N, B] =
    map(F.toType[B])

  def rotate[D <: Nat](to: VectorD[N, A])(implicit ev: N <= nat._3,
                                          D: Diff.Aux[nat._3, N, D],
                                          toIntD: ToInt[D],
                                          T: spa.Trig[A],
                                          F: spa.Field[A],
                                          N: spa.NRoot[A],
                                          E: spa.Eq[A]): VectorD[nat._4, A] = {
    val a = padZero(3) cross to.padZero(3)
    val n = a.norm
    val axis = if (n === F.zero) VectorD.zAxis else a :/ n
    val θ = T.acos(this ⋅ to)
    VectorD.sized(4, Vector(axis.x, axis.y, axis.z, θ))
  }

  override def toString: String =
    s"VectorD(${unsized.toString})"
}

object VectorD extends VectorDInstances {

  import LTEq._
  import LT._

  def zero[N <: Nat: ToInt, A](
      implicit G: spa.AdditiveMonoid[A]): VectorD[N, A] =
    fill(G.zero)

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

  private def axis[N <: Nat: ToInt, A](i: Nat)(implicit ev: i.N < N,
                                               toInt: ToInt[i.N],
                                               R: spa.Ring[A]): VectorD[N, A] =
    fill[N, A](R.zero).update(i, R.one)

  def xAxis[A: spa.Ring]: VectorD[nat._2, A] = axis[nat._2, A](0)
  def yAxis[A: spa.Ring]: VectorD[nat._2, A] = axis[nat._2, A](1)
  def zAxis[A: spa.Ring]: VectorD[nat._3, A] = axis[nat._3, A](2)
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
      implicit ea: spa.Eq[A]): spa.Eq[VectorD[N, A]] =
    new VectorDSpireEq[N, A] { val EA = ea }

  implicit def vectorDSemigroup[N <: Nat, A](
      implicit sa: Semigroup[A]): Semigroup[VectorD[N, A]] =
    new VectorDSemigroup[N, A] { val SA = sa }
}

private[iliad] trait VectorDInstances1 {

  implicit def vectorDCatsInstances[N <: Nat](implicit toInt: ToInt[N])
    : Applicative[VectorD[N, ?]] with Foldable[VectorD[N, ?]] =
    new VectorDCatsInstances[N] {
      val n: Int = toInt()
    }

  implicit def vectorDIsInnerProductSpace[N <: Nat, A](
      implicit fa: spa.Field[A],
      toInt: ToInt[N]): spa.InnerProductSpace[VectorD[N, A], A] =
    new VectorDIsInnerProductSpace[N, A] {
      val n = toInt()
      def scalar: spa.Field[A] = fa
    }
}

private[iliad] sealed trait VectorDCatsInstances[N <: Nat]
    extends Applicative[VectorD[N, ?]]
    with Foldable[VectorD[N, ?]] {
  val n: Int
  def pure[A](a: A): VectorD[N, A] = new VectorD(Vector.fill(n)(a))
  def ap[A, B](ff: VectorD[N, A => B])(fa: VectorD[N, A]): VectorD[N, B] =
    fa ap ff

  def foldLeft[A, B](fa: VectorD[N, A], b: B)(f: (B, A) => B): B =
    fa.foldLeft(b)(f)

  def foldRight[A, B](fa: VectorD[N, A], lb: Eval[B])(
      f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.foldRight(lb)(f)
}

private[iliad] sealed trait VectorDEq[N <: Nat, A] extends Eq[VectorD[N, A]] {
  implicit val EA: Eq[A]
  def eqv(x: VectorD[N, A], y: VectorD[N, A]): Boolean = x === y
}

private[iliad] sealed trait VectorDSpireEq[N <: Nat, A]
    extends spa.Eq[VectorD[N, A]] {
  implicit val EA: spa.Eq[A]
  def eqv(x: VectorD[N, A], y: VectorD[N, A]): Boolean =
    x.unsized === y.unsized
}

private[iliad] sealed trait VectorDSemigroup[N <: Nat, A]
    extends Semigroup[VectorD[N, A]] {
  implicit val SA: Semigroup[A]
  def combine(x: VectorD[N, A], y: VectorD[N, A]): VectorD[N, A] = x combine y
}

private[iliad] sealed trait VectorDIsInnerProductSpace[N <: Nat, A]
    extends spa.InnerProductSpace[VectorD[N, A], A] {

  def n: Int
  def dot(x: VectorD[N, A], y: VectorD[N, A]): A = x ⋅ y
  def timesl(l: A, x: VectorD[N, A]): VectorD[N, A] = l *: x
  def negate(v: VectorD[N, A]): VectorD[N, A] = -v
  def zero: VectorD[N, A] = new VectorD(Vector.fill(n)(scalar.zero))
  def plus(x: VectorD[N, A], y: VectorD[N, A]): VectorD[N, A] = x + y
}
