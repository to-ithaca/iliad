package iliad
package algebra

import algebra.syntax.matrix._
import arbitrary._

import org.scalatest._
import org.scalatest.prop._

import org.scalacheck._

import spire.implicits._

import shapeless._

class MatrixTests extends FunSuite with GeneratorDrivenPropertyChecks {
 
  test("Matrix#id") {
    forAll { (m: Matrix[nat._1, nat._4, Float]) =>
      val id  = Matrix.id[Float](4)
      assert(id * m === m)
    }
  }
  test("Matrix#symmetric") {
    forAll(MatrixGen.symmetric[nat._3, Int](Arbitrary.arbitrary[Int])) { (m: Mat3i) =>
      assert(m.symmetric)
    }
  }

  test("Matrix#transpose") {
    forAll { (m: Mat2i) =>
      assert(m.transpose.transpose === m)
    }
  }

  test("Matrix#zero") {
    forAll { (m: Matrix[nat._2, nat._2, Float]) =>
      val zero = Matrix.zero[Float](2, 2)
      assert((m - m) === zero)
    }
  }
  test("Matrix#context") {
    val m = mat"""1 0
                  0 1"""
    assert(m === Matrix.id[Float, nat._2])
  }

  test("Matrix#plus") { (a: Mat2f, b: Mat2f, c: Mat2f) =>
    assert((a + (b + c)) ===((a + b) + c))
  }

  test("Matrix#minus") { (a: Mat2f, b: Mat2f, c: Mat2f) =>
    assert((a - (b - c)) ===((a - b) - c))
  }

  test("Matrix#inverse") { (a: Mat4f) =>
    val id = Matrix.id[Float](4)
    assert((a.inverse * a) === id)
  }

}
