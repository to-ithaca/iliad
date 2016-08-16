package iliad
package kernel
package platform

private[iliad] trait MatrixLibrary {

  /** Multiplies two 4x4 float matrices together.
    *  The matrices are in row major order
    */
  def multiplyMM(m0: Array[Float], m1: Array[Float]): Array[Float]

  /** Multiplies a 4x4 matrix and a 4 vector together.
    * The matrix is in row major order
    */
  def multiplyMV(m: Array[Float], v: Array[Float]): Array[Float]

  /** Inverts a 4x4 matrix */
  def invertM(m: Array[Float]): Array[Float]
}
