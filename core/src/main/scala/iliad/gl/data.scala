package iliad
package gl

import simulacrum.typeclass

import java.nio.Buffer

import cats._
import cats.data._
import cats.implicits._

object VertexShader {
  case class Source(s: String, attributes: List[Attribute.Constructor]) {
    def attributeNames: List[String] = attributes.map(_.name)
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
      id: Int, unlinked: Unlinked, attributes: List[(String, Int)]) {

    private def loaded(a: Attribute.Constructor): Option[Attribute.Loaded] =
      attributes.find(_._1 == a.name).map {
        case (_, location) => Attribute.Loaded(a, location)
      }

    def loaded(as: List[Attribute.Constructor])
      : String Xor Attribute.LoadedAttributes =
      as.traverse(a =>
              loaded(a).toRightXor(s"Location for attribute is undefined: $a"))
        .map(Attribute.LoadedAttributes)
  }
}

object Attribute {
  case class Constructor(
      name: String, byteSize: Int, elementSize: Int, `type`: VertexAttribType)
  case class Loaded(c: Constructor, location: Int)

  case class Offset(l: Loaded, offset: Int)

  case class LoadedAttributes(ls: List[Loaded]) {
    val stride: Int = ls.map(_.c.byteSize).sum
    def offsets(base: Int): List[Offset] =
      ls.foldLeft(base -> List.empty[Offset])({
          case ((offset, acc), a) =>
            (offset + a.c.byteSize, (Offset(a, offset) :: acc))
        })
        ._2
  }
}

object VertexBuffer {
  case class Constructor(attributes: List[Attribute.Constructor])
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
  //TODO: do we even use this
  def plus(i: Int): DataRange = DataRange(start + i, end + i)
}

object VertexData {
  case class Data(data: Buffer, size: Int)
  case class Ref(name: String, b: VertexBuffer.Constructor)
  case class Loaded(range: DataRange, ref: Ref) {
    def offset(ref: Model.VertexRef): Int = {
      range.start + ref.range.start
    }
  }
}

object ElementData {
  case class Data(data: Buffer, size: Int)
  case class Ref(name: String, b: ElementBuffer.Constructor)
  case class Loaded(range: DataRange, ref: Ref) {
    def offset(ref: Model.ElementRef): DataRange = {
      ref.range plus (range.start)
    }
  }
}

sealed trait Framebuffer

object Framebuffer {
  object Default extends Framebuffer
}

case class Model(vertex: Model.VertexRef, element: Model.ElementRef)

object Model {
  case class VertexRef(ref: VertexData.Ref, range: DataRange)
  case class ElementRef(ref: ElementData.Ref, range: DataRange)
}

case class DrawOp(model: Model, program: Program.Unlinked, framebuffer: Int)
