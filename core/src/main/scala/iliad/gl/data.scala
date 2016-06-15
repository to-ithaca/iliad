package iliad
package gl

import java.nio.Buffer

object VertexShader {
  case class Source(s: String)
  case class Compiled(id: Int, s: Source)
}

object FragmentShader {
  case class Source(s: String)
  case class Compiled(id: Int, s: Source)
}

object Program {
  case class Unlinked(vs: VertexShader.Source, fs: FragmentShader.Source)
  case class Linked(id: Int, unlinked: Unlinked)
}

object VertexBuffer {
  case class Loaded(id: Int, filled: Int, capacity: Int) {
    def inc(size: Int): Loaded = copy(filled = filled + size)
  }
}

object ElementBuffer {
  case class Loaded(id: Int, filled: Int, capacity: Int) {
    def inc(size: Int): Loaded = copy(filled = filled + size)
  }
}

object Model {
  case class VertexData(data: Buffer, size: Int)
  case class ElementData(data: Buffer, size: Int)

  case class Data(v: VertexData, e: ElementData)
  case class Loaded(v: VertexBuffer.Loaded,
                    e: ElementBuffer.Loaded,
                    vertexRange: (Int, Int),
                    elementRange: (Int, Int))
}
