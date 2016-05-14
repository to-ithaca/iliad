package iliad

import org.typelevel.discipline.scalatest._
import org.scalatest._
import cats._
import cats.laws.discipline._
import algebra.laws._
import cats.implicits._
import cats.syntax._

import shapeless._
import shapeless.ops.nat.ToInt

import iliad.syntax.vector._

import org.scalacheck.Arbitrary

class VectorDTests extends FunSuite with Discipline {

  implicit val iso = CartesianTests.Isomorphisms.invariant[VectorD[nat._3, ?]]

  implicit def vectorDArbitrary[N <: Nat, A](implicit arb: Arbitrary[A], toInt: ToInt[N]): Arbitrary[VectorD[N, A]] = Arbitrary {
    for {
      a <- arb.arbitrary
    } yield VectorD.fill[N, A](a)
  }

  {
    Functor[VectorD[nat._3, ?]]
    Applicative[VectorD[nat._3, ?]]
    checkAll("VectorD[nat._3, Int]", ApplicativeTests[VectorD[nat._3, ?]].applicative[Int, Int, Int])
  }

  {
    Semigroup[VectorD[nat._3, Int]]
    checkAll("VectorD[nat._3, Int]", GroupLaws[VectorD[nat._3, Int]].semigroup)
  }

  test("vector context syntax as the same as explicit creation") {
    assert(v"1 2 3" === VectorD.sized(3, Vector(1, 2, 3)))
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

