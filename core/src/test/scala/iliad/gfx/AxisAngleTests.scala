package iliad
package gfx

import iliad.syntax.vectord._

import spire.implicits._

import cats.implicits._

import org.scalatest._

class AxisAngleTests extends FunSuite {

  implicit def L: iliad.kernel.platform.MatrixLibrary = MatrixLibrary

  test("a rotation of zero should give the identity matrix") {
    val axis = v"1f 0f 0f"
    assert(AxisAngle(axis, 0f).matrix.matrix === MatrixD.id4f)
  }
}

class RotationMatrixTests extends FunSuite {

  implicit def L: iliad.kernel.platform.MatrixLibrary = MatrixLibrary

  test("an rotation of identity should leave a vector unchanged") {
    val v = v"0f 1f 0f"
    assert(RotationMatrix(MatrixD.id4f).rotate(v) === v)
  }
}
