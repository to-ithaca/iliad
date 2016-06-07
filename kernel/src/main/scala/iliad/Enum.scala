package iliad

import simulacrum.typeclass

/** Represent discrete operations that can be performed on A */
@typeclass trait Enum[A] {
  def succ(x: A): A
  def pred(x: A): A
  def adj(x: A, y: A): Boolean = succ(x) == y
}
