package iliad
package kernel

import iliad.kernel.platform.MatrixLibrary

import breeze.linalg._
import DenseMatrix._
import DenseVector._

object X11MatrixLibrary extends MatrixLibrary {

  private def denseMatrix(a: Array[Float]): DenseMatrix[Float] =
    DenseMatrix((a(0), a(1), a(2), a(3)),
                (a(4), a(5), a(6), a(7)),
                (a(8), a(9), a(10), a(11)),
                (a(12), a(13), a(14), a(15)))

  private def array(m: DenseMatrix[Float]): Array[Float] =
    Array(m(0, 0),
          m(0, 1),
          m(0, 2),
          m(0, 3),
          m(1, 0),
          m(1, 1),
          m(1, 2),
          m(1, 3),
          m(2, 0),
          m(2, 1),
          m(2, 2),
          m(2, 3),
          m(3, 0),
          m(3, 1),
          m(3, 2),
          m(3, 3))

  private def denseVector(v: Array[Float]): DenseVector[Float] =
    DenseVector(v(0), v(1), v(2), v(3))

  private def array(v: DenseVector[Float]): Array[Float] =
    Array(v(0), v(1), v(2), v(3))

  def multiplyMM(m0: Array[Float], m1: Array[Float]): Array[Float] =
    array(denseMatrix(m0) * denseMatrix(m1))
  def multiplyMV(m: Array[Float], v: Array[Float]): Array[Float] =
    array(denseMatrix(m) * denseVector(v))

  def invertM(m: Array[Float]): Array[Float] =
    array(inv[DenseMatrix[Float], DenseMatrix[Float]](denseMatrix(m)))
}
