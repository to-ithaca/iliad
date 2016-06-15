package iliad
package gl

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
