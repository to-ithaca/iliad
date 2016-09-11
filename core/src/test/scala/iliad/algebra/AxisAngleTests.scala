package iliad
package algebra

import arbitrary._

import org.scalatest._
import org.scalatest.prop._

import org.scalacheck._

import scala.math._

import spire.implicits.{DoubleAlgebra => _ , _}

import iliad.implicits._

class AxisAngleTests extends FunSuite with Matchers {

  implicit val fuzzyAlgebraDouble: spire.std.DoubleAlgebra = new spire.std.DoubleAlgebra {
    override def eqv(x: Double, y: Double): Boolean = {
      val percent = Math.abs((x / 20.0))
      x === (y +- (Math.max(percent, 0.01)))
    }
  }

  test("AxisAngle.zero[Int] is expected") {
    AxisAngle.zero[Int] should === ((0, v"0 0 1"))
  }

  test("AxisAngle.rotationToBasis[Double] is expected") {
    val x = v"0.0 1.0 0.0"
    val y = v"-1.0 0.0 0.0"
    AxisAngle.rotationToBasis(x, y) should ===((Pi / 2.0, v"0.0 0.0 1.0"))
  }

  test("AxisAngle.rotationToBasis[Double] is the identity if basis is the same") {
    val x = v"1.0 0.0 0.0"
    val y = v"0.0 1.0 0.0"
    AxisAngle.rotationToBasis(x, y) should ===((0.0, v"0.0 0.0 1.0"))
  }
 
  test("AxisAngle.rotationToBasis[Double] swaps the x and y axes") {
    val x = v"0.0 1.0 0.0"
    val y = v"1.0 0.0 0.0"
    val r = AxisAngle.rotationToBasis(x, y)
    r.θ should ===(Pi +- 0.0001)
    assert(r.e === v"1.0 1.0 0.0".normalize)
  }

  test("AxisAngle.rotationToBasis[Double] determines the correct angle") {
    val x = v"0.0 1.0 0.0"
    val y = v"0.0 0.0 1.0"
    val r = AxisAngle.rotationToBasis(x, y)
    r.θ should ===((2 * Pi / 3.0) +- 0.0001)
    assert(r.e === v"1.0 1.0 1.0".normalize)
  }

  test("AxisAngle.rotationToBasis[Double] reverses the x and y axes") {
    val x = v"-1.0 0.0 0.0"
    val y = v"0.0 -1.0 0.0"
    val r = AxisAngle.rotationToBasis(x, y)
    r.θ should ===(Pi +- 0.0001)
    assert(r.e === v"0.0 0.0 1.0")
  }

}
