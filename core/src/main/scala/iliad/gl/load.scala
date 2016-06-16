package iliad
package gl

import iliad.CatsExtra._

import cats._
import cats.free._, Free._

object Load {
  type DSL[A] = Free[Load, A]

  def apply(s: VertexShader.Source): DSL[VertexShader.Compiled] =
    LoadVertexShader(s).free
  def apply(s: FragmentShader.Source): DSL[FragmentShader.Compiled] =
    LoadFragmentShader(s).free
  def apply(vs: VertexShader.Compiled,
            fs: FragmentShader.Compiled): DSL[Program.Linked] =
    LoadProgram(vs, fs).free

  def newBuffer(v: Model.VertexData,
                pageSize: Int,
                b: VertexBuffer.Base): DSL[VertexBuffer.Loaded] =
    LoadNewVertexBuffer(v, pageSize, b).free
  def newBuffer(e: Model.ElementData,
                pageSize: Int,
                b: VertexBuffer.Base): DSL[ElementBuffer.Loaded] =
    LoadNewElementBuffer(e, pageSize, b).free
  def insert(v: Model.VertexData,
             pageSize: Int,
             b: VertexBuffer.Loaded): DSL[VertexBuffer.Loaded] =
    LoadInsertVertexBuffer(v, pageSize, b).free
  def insert(e: Model.ElementData,
             pageSize: Int,
             b: ElementBuffer.Loaded): DSL[ElementBuffer.Loaded] =
    LoadInsertElementBuffer(e, pageSize, b).free
  def copy(v: Model.VertexData,
           pageSize: Int,
           b: VertexBuffer.Loaded): DSL[VertexBuffer.Loaded] =
    LoadCopyVertexBuffer(v, pageSize, b).free
  def copy(e: Model.ElementData,
           pageSize: Int,
           b: ElementBuffer.Loaded): DSL[ElementBuffer.Loaded] =
    LoadCopyElementBuffer(e, pageSize, b).free

  def parse[F[_]: Monad](f: GL ~> F): Load ~> F = new (Load ~> F) {
    def apply[A](load: Load[A]): F[A] = LoadParser(load).foldMap(f)
  }
}

sealed trait Load[A]

case class LoadVertexShader(s: VertexShader.Source)
    extends Load[VertexShader.Compiled]
case class LoadFragmentShader(s: FragmentShader.Source)
    extends Load[FragmentShader.Compiled]
case class LoadProgram(vs: VertexShader.Compiled, fs: FragmentShader.Compiled)
    extends Load[Program.Linked]

case class LoadNewVertexBuffer(
    v: Model.VertexData, pageSize: Int, b: VertexBuffer.Base)
    extends Load[VertexBuffer.Loaded]
case class LoadNewElementBuffer(
    e: Model.ElementData, pageSize: Int, b: VertexBuffer.Base)
    extends Load[ElementBuffer.Loaded]
case class LoadInsertVertexBuffer(
    v: Model.VertexData, pageSize: Int, b: VertexBuffer.Loaded)
    extends Load[VertexBuffer.Loaded]
case class LoadInsertElementBuffer(
    e: Model.ElementData, pageSize: Int, b: ElementBuffer.Loaded)
    extends Load[ElementBuffer.Loaded]
case class LoadCopyVertexBuffer(
    v: Model.VertexData, pageSize: Int, b: VertexBuffer.Loaded)
    extends Load[VertexBuffer.Loaded]
case class LoadCopyElementBuffer(
    e: Model.ElementData, pageSize: Int, b: ElementBuffer.Loaded)
    extends Load[ElementBuffer.Loaded]

private object LoadParser extends (Load ~> GL.DSL) {

  private def roundUp(size: Int, baseCapacity: Int): Int =
    Math.ceil(size.toDouble / baseCapacity.toDouble).toInt * baseCapacity

  def apply[A](load: Load[A]): GL.DSL[A] = load match {
    case LoadVertexShader(s) =>
      GL.makeVertexShader(s.s).map(VertexShader.Compiled(_, s))
    case LoadFragmentShader(s) =>
      GL.makeFragmentShader(s.s).map(FragmentShader.Compiled(_, s))
    case LoadProgram(vs, fs) =>
      for {
        id <- GL.makeProgram(vs.id, fs.id)
        as <- GL.getAttributeLocations(id, vs.s.attributeNames)
      } yield Program.Linked(id, Program.Unlinked(vs.s, fs.s), as)

    case LoadNewVertexBuffer(v, pageSize, b) =>
      val capacity = roundUp(v.size, pageSize)
      GL.makeNewVertexBuffer(v.data, v.size, capacity)
        .map(VertexBuffer.Loaded(_, v.size, capacity, b))
    case LoadNewElementBuffer(e, pageSize, b) =>
      val capacity = roundUp(e.size, pageSize)
      GL.makeNewElementBuffer(e.data, e.size, capacity)
        .map(ElementBuffer.Loaded(_, e.size, capacity, b))

    case LoadInsertVertexBuffer(v, pageSize, b) =>
      GL.insertVertices(b.id, b.filled, v.size, v.data).map(_ => b.inc(v.size))
    case LoadInsertElementBuffer(e, pageSize, b) =>
      GL.insertElements(b.id, b.filled, e.size, e.data).map(_ => b.inc(e.size))

    case LoadCopyVertexBuffer(v, pageSize, b) =>
      val capacity = roundUp(v.size, pageSize)
      GL.copyVertices(b.id, b.filled, v.size, v.data, capacity)
        .map(VertexBuffer.Loaded(_, b.filled + v.size, capacity, b.b))
    case LoadCopyElementBuffer(e, pageSize, b) =>
      val capacity = roundUp(e.size, pageSize)
      GL.copyElements(b.id, b.filled, e.size, e.data, capacity)
        .map(ElementBuffer.Loaded(_, b.filled + e.size, capacity, b.b))
  }
}
