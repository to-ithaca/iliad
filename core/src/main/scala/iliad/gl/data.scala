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
  case class Linked(id: Int, unlinked: Unlinked, attributes: List[(String, Int)])
}

@typeclass trait AttributeType[A]

object VertexBuffer {

  //TODO: should this be in the Model object?
  case class Base(attributes: List[(String, AttributeType[_])])
  case class Loaded(id: Int, filled: Int, capacity: Int, b: Base) {
    def inc(size: Int): Loaded = copy(filled = filled + size)
    def fits(size: Int): Boolean = capacity - filled > size
  }
}

object ElementBuffer {
  //TODO: if we tie this to the vertex buffer, we can't optimise point buffer storage
  case class Loaded(id: Int, filled: Int, capacity: Int, b: VertexBuffer.Base) {
    def inc(size: Int): Loaded = copy(filled = filled + size)
    def fits(size: Int): Boolean = capacity - filled > size
  }
}

object Model {
  case class VertexData(data: Buffer, size: Int)
  case class ElementData(data: Buffer, size: Int)

  //TODO: if we assume that models which have the same VertexBuffer.Base are loaded into the same buffer, then we can derive the vertex and element instances
  //That said, if we continue this, we'll probably end up with too big buffers - perhaps we should have a max size
  case class VertexLoaded(v: VertexBuffer.Loaded, range: (Int, Int))
  case class ElementLoaded(e: ElementBuffer.Loaded, range: (Int, Int))

  case class Base(v: VertexData, e: ElementData, b: VertexBuffer.Base)
  case class Loaded(v: VertexLoaded, e: ElementLoaded, b: Base)
}
