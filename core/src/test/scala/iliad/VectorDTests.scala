package iliad

import iliad.syntax.all._

import org.typelevel.discipline.scalatest._
import org.scalatest._
import org.scalatest.prop._

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

class VectorDTests extends FunSuite with Discipline with GeneratorDrivenPropertyChecks with Matchers {

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
    
    checkAll("VectorD[nat._3, Int]", ApplicativeTests[VectorD[nat._3, ?]].applicative[Int, Int, Int])

    Foldable[VectorD[nat._3, ?]]
    checkAll("VectorD[nat._3, Int]", FoldableTests[VectorD[nat._3, ?]].foldable[Int, Int])
  }

  {
    Semigroup[VectorD[nat._3, Int]]
    checkAll("VectorD[nat._3, Int]", GroupLaws[VectorD[nat._3, Int]].semigroup)
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

  test("xAxis gives the 2D x axis") {
    assert(VectorD.xAxis[Int] === v"1 0")
  }

  test("yAxis gives the 2D y axis") {
    assert(VectorD.yAxis[Int] === v"0 1")
  }

  test("zAxis gives the 3D z axis") {
    assert(VectorD.zAxis[Int] === v"0 0 1")
  }

  test("VectorD#exists is consistent with Vector#exists") {
    forAll { (v: VectorD[nat._3, Int], p: Int => Boolean) =>
      val vector = v.unsized
      v.exists(p) should === (vector.exists(p))
    }
  }

  test("VectorD#forall is consistent with Vector#forall") {
    forAll { (v: VectorD[nat._3, Int], p: Int => Boolean) =>
      val vector = v.unsized
      v.forall(p) should === (vector.forall(p))
    }
  }

  test("VectorD#rotateFrom handles angles greater than PI / 2") {
    val from = v"0.0 1.0 0.0"
    val to = v"1.0 -1.0 0.0".normalize

    val aa = to.rotateFrom(from)
    val axis = aa.dropUntil(3)
    assert(axis === v"0.0 0.0 -1.0")
    assert(aa.w == 3.0 * Math.PI/4.0)
  }

  test("VectorD#rotateFrom handles a zero angle") {
    val from = v"0.0 1.0 0.0"
    val to = v"0.0 1.0 0.0"
    val aa = to.rotateFrom(from)
    val axis = aa.dropUntil(3)
    println(from cross to)
    assert(axis === v"0.0 0.0 1.0")
    assert(aa.w == 0.0)
  }

}

