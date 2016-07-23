package iliad
package kernel

import iliad.kernel.platform.MatrixLibrary

object X11MatrixLibrary extends MatrixLibrary {
  //TODO: use breeze to implement these
  def multiplyMM(m0: Array[Float],m1: Array[Float]): Array[Float] = ???
  def multiplyMV(m: Array[Float],v: Array[Float]): Array[Float] = ???
}
