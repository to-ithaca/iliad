package iliad
package gl

import simulacrum.typeclass

import java.nio.Buffer

import cats._
import cats.data._
import cats.implicits._

import monocle._
import monocle.macros._
import monocle.syntax.all._

object VertexShader {
  case class Source(text: String, attributes: List[Attribute.Constructor]) {
    def attributeNames: List[String] = attributes.map(_.name)
  }
  case class Compiled(id: Int, source: Source)
}

object FragmentShader {
  case class Source(text: String)
  case class Compiled(id: Int, source: Source)
}

object Program {
  case class Unlinked(vertex: VertexShader.Source,
                      fragment: FragmentShader.Source)
  case class Linked(id: Int,
                    unlinked: Unlinked,
                    attributes: List[(String, Int)]) {

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
  case class Constructor(name: String,
                         byteSize: Int,
                         elementSize: Int,
                         `type`: VertexAttribType)
  case class Loaded(constructor: Constructor, location: Int)

  case class Offset(loaded: Loaded, offset: Int)

  case class LoadedAttributes(attributes: List[Loaded]) {
    val stride: Int = attributes.map(_.constructor.byteSize).sum
    def offsets(base: Int): List[Offset] =
      attributes
        .foldLeft(base -> List.empty[Offset])({
          case ((offset, acc), a) =>
            (offset + a.constructor.byteSize, (Offset(a, offset) :: acc))
        })
        ._2
  }
}

object VertexBuffer {
  case class Constructor(attributes: List[Attribute.Constructor])
  case class Loaded(id: Int,
                    filled: Int,
                    capacity: Int,
                    constructor: Constructor) {}

  object Loaded {
    val _filled: Lens[Loaded, Int] = GenLens[Loaded](_.filled)
    val _capacity: Lens[Loaded, Int] = GenLens[Loaded](_.capacity)
  }
  def inc(l: Loaded, size: Int): Loaded =
    l &|-> Loaded._filled modify (_ + size)
  def fits(l: Loaded, size: Int): Boolean =
    (l &|-> Loaded._capacity get) - (l &|-> Loaded._filled get) > size

  case class Update(buffer: Loaded, data: VertexData.Loaded)

  def loadNew(id: Int,
              c: VertexBuffer.Constructor,
              d: VertexData.Ref,
              size: Int,
              capacity: Int): Update =
    Update(Loaded(id, size, capacity, c),
           VertexData.Loaded(DataRange(0, size), d))

  def insert(old: Loaded, d: VertexData.Ref, size: Int): Update =
    Update(inc(old, size),
           VertexData.Loaded(DataRange(old.filled, old.filled + size), d))

  def copy(id: Int,
           old: Loaded,
           d: VertexData.Ref,
           size: Int,
           capacity: Int): Update =
    Update(Loaded(id, old.filled + size, capacity, old.constructor),
           VertexData.Loaded(DataRange(old.filled, old.filled + size), d))
}

object ElementBuffer {
  case class Constructor(name: String)
  case class Loaded(id: Int,
                    filled: Int,
                    capacity: Int,
                    constructor: Constructor) {}

  object Loaded {
    val _filled: Lens[Loaded, Int] = GenLens[Loaded](_.filled)
    val _capacity: Lens[Loaded, Int] = GenLens[Loaded](_.capacity)
  }
  def inc(l: Loaded, size: Int): Loaded =
    l &|-> Loaded._filled modify (_ + size)
  def fits(l: Loaded, size: Int): Boolean =
    (l &|-> Loaded._capacity get) - (l &|-> Loaded._filled get) > size

  case class Update(buffer: Loaded, data: ElementData.Loaded)

  def loadNew(id: Int,
              c: ElementBuffer.Constructor,
              d: ElementData.Ref,
              size: Int,
              capacity: Int): Update =
    Update(Loaded(id, size, capacity, c),
           ElementData.Loaded(DataRange(0, size), d))

  def insert(old: Loaded, d: ElementData.Ref, size: Int): Update =
    Update(inc(old, size),
           ElementData.Loaded(DataRange(old.filled, old.filled + size), d))

  def copy(id: Int,
           old: Loaded,
           d: ElementData.Ref,
           size: Int,
           capacity: Int): Update =
    Update(Loaded(id, old.filled + size, capacity, old.constructor),
           ElementData.Loaded(DataRange(old.filled, old.filled + size), d))
}

case class DataRange(start: Int, end: Int) {
  def plus(i: Int): DataRange = DataRange(start + i, end + i)
}

object VertexData {
  case class Data(data: Buffer, size: Int)
  case class Ref(name: String, buffer: VertexBuffer.Constructor)
  case class Loaded(range: DataRange, ref: Ref) {
    def offset(ref: Model.VertexRef): Int =
      range.start + ref.range.start
  }
}

object ElementData {
  case class Data(data: Buffer, size: Int)
  case class Ref(name: String, buffer: ElementBuffer.Constructor)
  case class Loaded(range: DataRange, ref: Ref) {
    def offset(ref: Model.ElementRef): DataRange =
      ref.range.plus(range.start)
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

case class DrawOp(model: Model, program: Program.Unlinked, framebuffer: Int) {
  val vertexModel: Model.VertexRef = model.vertex
  val vertexData: VertexData.Ref = vertexModel.ref
  val vertexBuffer: VertexBuffer.Constructor = vertexData.buffer
  val attributes = vertexBuffer.attributes
  val elementModel: Model.ElementRef = model.element
  val elementData: ElementData.Ref = elementModel.ref
  val elementBuffer: ElementBuffer.Constructor = elementData.buffer
}
