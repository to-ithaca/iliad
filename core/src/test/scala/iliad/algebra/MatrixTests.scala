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

  implicit val fuzzyAlgebraFloat: spire.std.FloatAlgebra = new spire.std.FloatAlgebra {
    override def eqv(x: Float, y: Float): Boolean = {
      val percent = Math.abs((x / 20f))
      x === (y +- (Math.max(percent, 0.01f)))
    }
  }

  implicit val arbFloat: Arbitrary[Float] = Arbitrary {
    Gen.choose(-100f, 100f)
  }

  checkAll("Matrix[?, ?, Float]", MatrixLaws[Matrix, Float].product[nat._4, nat._1])
  checkAll("Matrix[?, ?, Float]", MatrixLaws[Matrix, Float].product[nat._4, nat._2])
  checkAll("Matrix[?, ?, Float]", MatrixLaws[Matrix, Float].product[nat._4, nat._3])
  checkAll("Matrix[?, ?, Float]", MatrixLaws[Matrix, Float].product[nat._4, nat._4])
  checkAll("Matrix[?, ?, Float]", MatrixLaws[Matrix, Float].square[nat._4](MatrixGen.symmetric(Arbitrary.arbitrary)))

  test("Matrix[2, 2, Int].context is correct") {
    val m = mat"""1 0
                  0 1"""
    assert(m === Matrix.id[Int](2))
  }

  test("Matrix[3, 3, Int].context is correct") {
    val m = mat"""1 0 0
                  0 1 0
                  0 0 1"""
    assert(m === Matrix.id[Int](3))
  }

  test("Matrix[4, 4, Int].context is correct") {
    val m = mat"""1 0 0 0
                  0 1 0 0
                  0 0 1 0
                  0 0 0 1"""
    assert(m === Matrix.id[Int](4))
  }

  test("Matrix[3, 2, Int].pad === id") {
    val m = mat"""1 0 0
                  0 1 0"""
    assert(m.pad(4, 4) === Matrix.id[Int](4))
  }

  test("Matrix[2, 4, Int].pad === id") {
    val m = mat"""1 0
                  0 1
                  0 0
                  0 0"""
    assert(m.pad(4, 4) === Matrix.id[Int](4))
  }

  test("Matrix[4, 2, Int].pad === id") {
    val m = mat"""1 0 0 0
                  0 1 0 0"""
    assert(m.pad(4, 4) === Matrix.id[Int](4))
  }

  test("Matrix[4, 4].id === 4") {
    assert(Matrix.id[Int](4).trace === 4)
  }

  test("Matrix[3, 3].id === 3") {
    assert(Matrix.id[Int](3).trace === 3)
  }

  test("Matrix[2, 2].id === 2") {
    assert(Matrix.id[Int](2).trace === 2)
  }

  test("Matrix[3, 3].transpose is expected") {
    val m = mat"""1 2 3
                  4 5 6
                  7 8 9"""
    val t = mat"""1 4 7
                  2 5 8
                  3 6 9"""
    assert(m.transpose === t)
  }

  test("Matrix[4, 4].rotation.ortho") {
    forAll(MatrixGen.rotation[nat._2, Float](Gen.choose(0f, (2f * Math.PI.toFloat)))) { (m: Mat2f) =>
      assert(m.pad(4, 4).ortho.isDefined)
    }
  }
}
