package iliad

import cats._
import cats.syntax._
import cats.implicits._

import shapeless._
import shapeless.ops.nat._

//TODO: Really think we should parameterize this?
final class VectorD[N <: Nat, A] private[iliad] (_unsized: Vector[A]) {

  import LTEq._

  private def unsized = _unsized

  def apply(i: Nat)(implicit ev: i.N <= N, toInt: ToInt[i.N]): A = unsized(toInt())

  def map[B](f: A => B): VectorD[N, B] = new VectorD(unsized map f)
  def ap[B](ff: VectorD[N, A => B]): VectorD[N, B] = new VectorD(this.unsized.zip(ff.unsized).map { case (a, f) => f(a) } )
  def combine(that: VectorD[N, A])(implicit s: Semigroup[A], n: ToInt[N]): VectorD[N, A] = (this |@| that).map(_ |+| _)

  def n(implicit toInt: ToInt[N]): Int = toInt()
  
  //TODO: We really want the idea of basis as well?
  def x(implicit ev: nat._1 <= N): A = unsized(0)
  def y(implicit ev: nat._2 <= N): A = unsized(1)
  def z(implicit ev: nat._3 <= N): A = unsized(2)
  def w(implicit ev: nat._4 <= N): A = unsized(3)

  def eqv(that: VectorD[N, A])(implicit EqA: Eq[A]): Boolean = (this ap that.map((EqA.eqv _).curried)).unsized.foldLeft(true)(_ && _)

  override def toString: String = unsized.toString
}

object VectorD extends VectorDInstances {
  def sized[A](i: Nat, unsized: Vector[A])(implicit toInt: ToInt[i.N]): VectorD[i.N, A] = if(unsized.length < toInt()) throw new IllegalArgumentException(s"vector $unsized is less than ${toInt()}") else new VectorD(unsized)
  def fill[A](i: Nat, a: A)(implicit toInt: ToInt[i.N]): VectorD[i.N, A] = fill[i.N, A](a)
  def fill[N <: Nat, A](a: A)(implicit toInt: ToInt[N]): VectorD[N, A] = new VectorD(Vector.fill(toInt())(a))
}

private[iliad] abstract class VectorDInstances extends VectorDInstances1 {

  //The type constraint makes it horrible to work out the `Unapply`
  implicit def vectorDUnapply[N <: Nat, AA, TC[_[_]]](implicit tc: TC[VectorD[N, ?]]): Unapply.Aux1[TC, VectorD[N, AA], VectorD[N, ?], AA] = new Unapply[TC, VectorD[N, AA]] {
    type M[X] = VectorD[N, X]
    type A = AA
    override def TC: TC[VectorD[N, ?]] = tc
    override def subst: VectorD[N, AA] => M[A] = identity
  }

  implicit def vectorDEq[N <: Nat, A](implicit eqA: Eq[A]): Eq[VectorD[N, A]] = new VectorDEq[N, A] { implicit val EqA = eqA }
}

private[iliad] trait VectorDInstances1 {
  implicit def vectorDIsApplicative[N <: Nat](implicit toInt: ToInt[N]): Applicative[VectorD[N, ?]]  = new VectorDIsApplicative[N] { val n: Int = toInt() }
}

private[iliad] sealed trait VectorDIsApplicative[N <: Nat] extends Applicative[VectorD[N, ?]] {
  val n: Int
  def pure[A](a: A): VectorD[N, A] = new VectorD(Vector.fill(n)(a))
  def ap[A, B](ff: VectorD[N, A => B])(fa: VectorD[N , A]): VectorD[N, B] = fa ap ff
}

private[iliad] sealed trait VectorDEq[N <: Nat, A] extends Eq[VectorD[N, A]] {
  implicit val EqA: Eq[A]
  def eqv(x: VectorD[N, A], y: VectorD[N, A]): Boolean =  x eqv y
}

