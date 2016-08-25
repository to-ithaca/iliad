package iliad
package algebra

import org.typelevel.discipline.scalatest._
import algebra.syntax.matrix._
import arbitrary._

import org.scalatest._
import org.scalatest.prop._

import org.scalacheck._

import shapeless._

import spire.laws.GroupLaws

import spire.std.int._

class MatrixTests extends FunSuite with Discipline with GeneratorDrivenPropertyChecks with Matchers {

  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 5)

  implicit val fuzzyAlgebraFloat: spire.std.FloatAlgebra = new spire.std.FloatAlgebra {
    override def eqv(x: Float, y: Float): Boolean =
      x === (y +- 0.01f)
  }

  implicit val arbFloat: Arbitrary[Float] = Arbitrary {
    Gen.choose(-100f, 100f)
  }

  checkAll("Matrix[4, 4, Float]", GroupLaws[Mat4f].additiveGroup)


  test("Matrix[3, 3, Int].symmetric") {
    forAll(MatrixGen.symmetric[nat._3, Int](Arbitrary.arbitrary[Int])) { (m: Mat3i) =>
      assert(m.symmetric)
    }
  }

  test("Matrix[4, 4, Float].id * m === m") {
    forAll { (m: Matrix[nat._1, nat._4, Float]) =>
      val id  = Matrix.id[Float](4)
      assert(id * m === m)
    }
  }

  test("Matrix[2, 2, Int].transpose.transpose === m") {
    forAll { (m: Mat2i) =>
      assert(m.transpose.transpose === m)
    }
  }

  test("Matrix[2, 2, Int].context") {
    val m = mat"""1 0
                  0 1"""
    assert(m === Matrix.id[Float, nat._2])
  }

  test("Matrix[4, 4, Float].inverse * m === id") { (a: Mat4f) =>
    val id = Matrix.id[Float](4)
    assert((a.inverse * a) === id)
  }
}

