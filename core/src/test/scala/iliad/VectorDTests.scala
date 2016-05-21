package iliad

import org.typelevel.discipline.scalatest._
import org.scalatest._

import spire._
import spire.math._
import spire.implicits._
import spire.laws.VectorSpaceLaws

import cats._
import cats.laws.discipline._
import cats.kernel.laws._
import cats.implicits._

import shapeless._
import shapeless.ops.nat.ToInt

import iliad.implicits._

import org.scalacheck.Arbitrary

class VectorDTests extends FunSuite with Discipline {

  implicit val iso = CartesianTests.Isomorphisms.invariant[VectorD[nat._3, ?]]

  def fuzzyEq[A: algebra.Ring: algebra.Signed: Order](eps: A): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = {
      val delta = Order[A].max(x.abs, y.abs) * eps
      println("d = %f, (x - y).abs = %f" format (delta, (x - y).abs))
      (x - y).abs < delta
    }
  }

  implicit def vectorDArbitrary[N <: Nat, A](implicit arb: Arbitrary[A], toInt: ToInt[N]): Arbitrary[VectorD[N, A]] = Arbitrary {
    for {
      a <- arb.arbitrary
    } yield VectorD.fill[N, A](a)
  }

  {
    Functor[VectorD[nat._3, ?]]
    Applicative[VectorD[nat._3, ?]]
    checkAll("VectorD[nat._3, Float]", ApplicativeTests[VectorD[nat._3, ?]].applicative[Int, Int, Int])
  }

  {
    Semigroup[VectorD[nat._3, Float]]
    checkAll("VectorD[nat._3, Float]", GroupLaws[VectorD[nat._3, Int]].semigroup)
  }

  {
    algebra.InnerProductSpace[VectorD[nat._3, Float], Float]
    checkAll("VectorD[nat._3, BigDecimal], BigDecimal", VectorSpaceLaws[VectorD[nat._3, BigDecimal], BigDecimal].innerProductSpace)
  }

  {
    Eq[VectorD[nat._3, Float]]
  }

  test("vector context syntax as the same as explicit creation") {
    assert( v"1 2 3"=== VectorD.sized(3, Vector(1, 2, 3)))
  }

  test("x accessor gives 0th member") {
    val v = v"1"
    assert(v.x == v(0))
  }

  test("y accessor gives 1st member") {
    val v = v"1 2"
    assert(v.y == v(1))
  }

  test("z accessor gives 2nd member") {
    val v = v"1 2 3"
    assert(v.z == v(2))
  }

  test("w accessor gives 3rd member") {
    val v = v"1 2 3 4"
    assert(v.w == v(3))
  }

  test("n returns the dimensions") {
    assert(v"1 2 3 4 5".n == 5)
  }

}

