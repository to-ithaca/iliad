package iliad

import shapeless._
import shapeless.ops.nat._

final class MatrixD[W <: Nat, H <: Nat, A] private[iliad] (_unsized: Vector[A]) {
  def toArray: Array[A] = ???
//TODO: add matrix operations
}

object MatrixD {
  def sized[A](w: Nat, h: Nat, unsized: Vector[A])(implicit toIntW: ToInt[w.N], toIntH: ToInt[h.N]): MatrixD[w.N, h.N, A] = if(unsized.length < toIntW() * toIntH()) throw new IllegalArgumentException(s"vector $unsized is less than ${toIntW() * toIntH()}") else new MatrixD(unsized)
}
