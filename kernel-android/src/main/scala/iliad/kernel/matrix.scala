package iliad
package kernel

import iliad.kernel.platform.MatrixLibrary

import android.opengl.Matrix

object AndroidMatrixLibrary extends MatrixLibrary {

  private def transpose(m: Array[Float]): Array[Float] = {
    val r = new Array[Float](16)
    Matrix.transposeM(r, 0, m, 0)
    r
  }

  def multiplyMM(m0: Array[Float], m1: Array[Float]): Array[Float] = {
    val r = new Array[Float](16)
    Matrix.multiplyMM(r, 0, transpose(m0), 0, transpose(m1), 0)
    transpose(r)
  }

  def multiplyMV(m: Array[Float], v: Array[Float]): Array[Float] = {
    val r = new Array[Float](4)
    Matrix.multiplyMV(r, 0, transpose(m), 0, v, 0)
    r
  }

  def invertM(m: Array[Float]): Array[Float] = {
    val r = new Array[Float](16)
    Matrix.invertM(r, 0, transpose(m), 0)
    transpose(r)
  }
}
