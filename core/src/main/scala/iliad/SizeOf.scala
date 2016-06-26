package iliad

import simulacrum.typeclass

@typeclass
trait SizeOf[A] {
  def byteSize: Int
}
