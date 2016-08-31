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
import spire.implicits.{FloatAlgebra => _, DoubleAlgebra => _, _}

import cats.laws.discipline.FunctorTests

class MatrixTests extends FunSuite with Discipline with GeneratorDrivenPropertyChecks with Matchers {

  implicit val fuzzyAlgebraFloat: spire.std.FloatAlgebra = new spire.std.FloatAlgebra {
    override def eqv(x: Float, y: Float): Boolean = {
      val percent = Math.abs((x / 20f))
      x === (y +- (Math.max(percent, 0.1f)))
    }
  }

  implicit val fuzzyAlgebraDouble: spire.std.DoubleAlgebra = new spire.std.DoubleAlgebra {
    override def eqv(x: Double, y: Double): Boolean = {
      val percent = Math.abs((x / 20.0))
      x === (y +- (Math.max(percent, 0.1)))
    }
  }

  implicit val arbFloat: Arbitrary[Float] = Arbitrary {
    Gen.choose(-100f, 100f)
  }

  implicit val arbDouble: Arbitrary[Double] = Arbitrary {
    Gen.choose(-100.0, 100.0)
  }

  checkAll("Matrix[?, ?, Float]", MatrixLaws[Matrix, Float].product[nat._4, nat._1])
  checkAll("Matrix[?, ?, Float]", MatrixLaws[Matrix, Float].product[nat._4, nat._2])
  checkAll("Matrix[?, ?, Float]", MatrixLaws[Matrix, Float].product[nat._4, nat._3])
  checkAll("Matrix[?, ?, Float]", MatrixLaws[Matrix, Float].product[nat._4, nat._4])
  checkAll("Matrix[?, ?, Float]", MatrixLaws[Matrix, Float].square[nat._4](MatrixGen.symmetric(Arbitrary.arbitrary[Float])))

  checkAll("Matrix[?, ?, Double]", MatrixLaws[Matrix, Double].product[nat._4, nat._1])
  checkAll("Matrix[?, ?, Double]", MatrixLaws[Matrix, Double].product[nat._4, nat._2])
  checkAll("Matrix[?, ?, Double]", MatrixLaws[Matrix, Double].product[nat._4, nat._3])
  checkAll("Matrix[?, ?, Double]", MatrixLaws[Matrix, Double].product[nat._4, nat._4])
  checkAll("Matrix[?, ?, Double]", MatrixLaws[Matrix, Double].square[nat._4](MatrixGen.symmetric(Arbitrary.arbitrary[Double])))
 
  checkAll("Matrix[nat._4, nat._4, Int]", FunctorTests[Matrix[nat._4, nat._4, ?]].functor[Int, Int, Int])

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

  test("Matrix[4, 4].id.times v === v") {
    forAll { (v: Vec4f) =>
      assert((Matrix.id[Float](4) * v) === v)
    }
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

  test("Matrix[4, 4].rotation.isOrtho") {
    forAll(MatrixGen.rotation[nat._2, Float](Gen.choose(0f, (2f * Math.PI.toFloat)))) { (m: Mat2f) =>
      assert(m.pad(4, 4).isOrtho)
    }
  }

  test("Matrix[3, 2].show is expected") {
    mat"""1 2 3
          4 5 6""".show should ===(
s"""1 | 2 | 3
4 | 5 | 6""")
  }

  test("Matrix[4, 4].multiply(Matrix[2, 4]) is expected") {
    val m1 = mat"""1f 0f 0f 0f
                   2f 0f 0f 0f
                   3f 0f 0f 0f
                   4f 0f 0f 0f"""


    val m2 = mat"""1f 2f
                   3f 4f
                   5f 6f
                   7f 8f"""
    (m1 * m2) should === (mat"""1f  2f
                                2f  4f
                                3f  6f
                                4f  8f""")
  }

  test("Matrix[2, 2, Int].cmap[Double] is expected") {
    val m1 = mat"""0 1
                   2 3""".cmap[Double]
    val m2 = mat"""0.0 1.0
                   2.0 3.0"""
    m1 should ===(m2)
  }
}
