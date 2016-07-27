package iliad

import iliad.syntax.all._

import org.typelevel.discipline.scalatest._
import org.scalatest._

import spire._
import spire.math._
import spire.implicits._
import spire.laws.arb._
import spire.laws.VectorSpaceLaws

import cats._
import cats.laws.discipline._
import cats.kernel.laws._
import cats.implicits._

import shapeless._
import shapeless.ops.nat.ToInt

import org.scalacheck.Arbitrary

import arbitrary._

class VectorDTests extends FunSuite with Discipline {

  implicit val iso = CartesianTests.Isomorphisms.invariant[VectorD[nat._3, ?]]

  def fuzzyEq[A: algebra.Ring: algebra.Signed: Order](eps: A): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = {
      val delta = Order[A].max(x.abs, y.abs) * eps
      println("d = %f, (x - y).abs = %f" format (delta, (x - y).abs))
      (x - y).abs < delta
    }
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
    //TODO: how do spire's tests work
    //checkAll("VectorD[nat._3, Rational], Rational", VectorSpaceLaws[VectorD[nat._3, Rational], Rational].innerProductSpace)
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

  test("padZero pads a matrix with 0 to n dimensions") {
    assert(v"1 2 3".padZero(5) === v"1 2 3 0 0")
  }

  test("padOne pads a matrix with 1 to n dimensions") {
    assert(v"1 2 3".padOne(5) === v"1 2 3 1 1")
  }

  test("dropUntil drops a matrix to n dimensions") {
    assert(v"1 2 3 4 5".dropUntil(3) === v"1 2 3")
  }
}

