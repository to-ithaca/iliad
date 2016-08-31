package iliad
package gfx

import iliad.algebra._
import iliad.algebra.syntax.vector._

import spire.implicits._

import cats.implicits._

import org.scalatest._

class AxisAngleTests extends FunSuite {

  test("a rotation of zero should give the identity matrix") {
    val axis = v"1f 0f 0f"
    assert(AxisAngle(axis, 0f).matrix.matrix === Matrix.id[Float](4))
  }
}

class RotationMatrixTests extends FunSuite {

  test("an rotation of identity should leave a vector unchanged") {
    val v = v"0f 1f 0f"
    assert(RotationMatrix(Matrix.id[Float](4)).rotate(v) === v)
  }
}
