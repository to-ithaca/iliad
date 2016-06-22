package iliad

import simulacrum.typeclass

@typeclass
trait Bounded[A] {
  def MinValue: A
  def MaxValue: A
}
