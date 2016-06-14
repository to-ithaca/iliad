package iliad

import simulacrum.typeclass

@typeclass
trait SizeOf[A] {
  def byteSize: Int
}

@typeclass
trait VertexAttribTypeConversion[A] {
  def baseType: iliad.kernel.VertexAttribType
}
