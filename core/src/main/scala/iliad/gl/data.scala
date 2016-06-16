package iliad
package gl

import simulacrum.typeclass

import java.nio.Buffer

object VertexShader {
  case class Source(s: String, attributes: List[(String, AttributeType[_])]) {
    def attributeNames: List[String] = attributes.map(_._1)
  }
  case class Compiled(id: Int, s: Source)
}

object FragmentShader {
  case class Source(s: String)
  case class Compiled(id: Int, s: Source)
}

object Program {
  case class Unlinked(vs: VertexShader.Source, fs: FragmentShader.Source)
  case class Linked(
      id: Int, unlinked: Unlinked, attributes: List[(String, Int)])
}

@typeclass trait AttributeType[A]

object VertexBuffer {
  case class Constructor(attributes: List[(String, AttributeType[_])])
  case class Loaded(id: Int, filled: Int, capacity: Int, c: Constructor) {
    def inc(size: Int): Loaded = copy(filled = filled + size)
    def fits(size: Int): Boolean = capacity - filled > size
  }

  case class Update(l: Loaded, d: VertexData.Loaded)

  def loadNew(id: Int,
              c: VertexBuffer.Constructor,
              d: VertexData.Ref,
              size: Int,
              capacity: Int): Update =
    Update(Loaded(id, size, capacity, c),
           VertexData.Loaded(DataRange(0, size), d))

  def insert(old: Loaded, d: VertexData.Ref, size: Int): Update =
    Update(old.inc(size),
           VertexData.Loaded(DataRange(old.filled, old.filled + size), d))

  def copy(id: Int,
           old: Loaded,
           d: VertexData.Ref,
           size: Int,
           capacity: Int): Update =
    Update(Loaded(id, old.filled + size, capacity, old.c),
           VertexData.Loaded(DataRange(old.filled, old.filled + size), d))
}

object ElementBuffer {
  case class Constructor(name: String)
  case class Loaded(id: Int, filled: Int, capacity: Int, c: Constructor) {
    def inc(size: Int): Loaded = copy(filled = filled + size)
    def fits(size: Int): Boolean = capacity - filled > size
  }

  case class Update(l: Loaded, d: ElementData.Loaded)

  def loadNew(id: Int,
              c: ElementBuffer.Constructor,
              d: ElementData.Ref,
              size: Int,
              capacity: Int): Update =
    Update(Loaded(id, size, capacity, c),
           ElementData.Loaded(DataRange(0, size), d))

  def insert(old: Loaded, d: ElementData.Ref, size: Int): Update =
    Update(old.inc(size),
           ElementData.Loaded(DataRange(old.filled, old.filled + size), d))

  def copy(id: Int,
           old: Loaded,
           d: ElementData.Ref,
           size: Int,
           capacity: Int): Update =
    Update(Loaded(id, old.filled + size, capacity, old.c),
           ElementData.Loaded(DataRange(old.filled, old.filled + size), d))
}

case class DataRange(start: Int, end: Int) {
  def plus(i: Int): DataRange = DataRange(start + i, end + i)
}

object VertexData {
  case class Data(data: Buffer, size: Int)
  case class Ref(name: String, b: VertexBuffer.Constructor)
  case class Loaded(range: DataRange, ref: Ref)

  case class SubRef(v: VertexData.Ref, range: DataRange)
}

object ElementData {
  case class Data(data: Buffer, size: Int)
  case class Ref(name: String, b: ElementBuffer.Constructor)
  case class Loaded(range: DataRange, ref: Ref)

  case class SubRef(v: VertexData.Ref, range: DataRange)
}

case class Model(v: VertexData.SubRef, e: ElementData.SubRef)

sealed trait Framebuffer

object Framebuffer {
  object Default extends Framebuffer
}
