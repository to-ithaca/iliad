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
  case class Source(text: String, attributes: List[Attribute.Constructor], textures: Map[String, Sampler.Constructor]) {
    def attributeNames: List[String] = attributes.map(_.name)
    def textureNames: List[String] = textures.map(_._1).toList
  }
  case class Compiled(id: Int, source: Source)
}

object FragmentShader {
  case class Source(text: String, textures: Map[String, Sampler.Constructor]) {
    def textureNames: List[String] = textures.map(_._1).toList
  }
  case class Compiled(id: Int, source: Source)
}

object Program {
  case class Unlinked(vertex: VertexShader.Source,
                      fragment: FragmentShader.Source) {
    def textures: Map[String, Sampler.Constructor] = vertex.textures ++ fragment.textures
  }
  case class Linked(id: Int,
                    unlinked: Unlinked,
                    attributes: List[(String, Int)], textures: List[(String, Int)]) {

    private def loaded(a: Attribute.Constructor): Option[Attribute.Loaded] =
      attributes.find(_._1 == a.name).map {
        case (_, location) => Attribute.Loaded(a, location)
      }

    def loaded(as: List[Attribute.Constructor])
      : String Xor Attribute.LoadedAttributes =
      as.traverse(a =>
              loaded(a).toRightXor(s"Location for attribute is undefined: $a"))
        .map(Attribute.LoadedAttributes)

    def textureUniforms(ts: Map[String, Texture.Constructor]): String Xor List[TextureUniform] = 
      textures.zipWithIndex.map {
        case ((name, location), index) => 
          val sampler = unlinked.textures(name)
          ts.get(name).toRightXor(s"Unable to find uniform for texture $name").map { t => 
            val unit: TextureUnit = ???
            TextureUniform(unit, location, t, sampler)
          }
      }.sequence
  }

  case class TextureUniform(unit: TextureUnit, location: Int, texture: Texture.Constructor, sampler: Sampler.Constructor)
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

case class Model(vertex: Model.VertexRef, element: Model.ElementRef)

object Model {
  case class VertexRef(ref: VertexData.Ref, range: DataRange)
  case class ElementRef(ref: ElementData.Ref, range: DataRange)
}

object Texture {

  case class Data(data: Buffer, size: Int)

  case class Format(pixel: TextureFormat,
                    internal: TextureInternalFormat,
                    pixelType: TexturePixelType,
                    bytesPerPixel: Int)

  case class Constructor(name: String,
                         format: Format,
                         viewport: Rect[Int],
                         isBuffered: Boolean)
      extends Framebuffer.AttachmentConstructor

  case class Loaded(constructor: Constructor,
                    frontId: Int,
                    backIdOpt: Option[Int])
      extends Framebuffer.AttachmentLoaded {
    def backId: String Xor Int =
      if (constructor.isBuffered)
        backIdOpt.toRightXor("Unable to find back id")
      else frontId.right
    def flip: Option[Loaded] =
      backIdOpt.map(b => copy(frontId = b, backIdOpt = Some(frontId)))
  }
}

object Renderbuffer {
  case class Constructor(name: String,
                         format: RenderbufferInternalFormat,
                         viewport: Rect[Int])
      extends Framebuffer.AttachmentConstructor
  case class Loaded(constructor: Constructor, id: Int)
      extends Framebuffer.AttachmentLoaded
}

sealed trait Framebuffer

object Framebuffer {
  sealed trait LoadedFramebuffer {
    def frontId: Int
  }

  object Default extends Framebuffer with LoadedFramebuffer {
    val frontId: Int = 0
  }

  sealed trait AttachmentConstructor
  sealed trait AttachmentLoaded
  case class Constructor(
      attachments: List[(FramebufferAttachment, AttachmentConstructor)])
      extends Framebuffer {
    def isBuffered: Boolean = attachments.exists {
      case (_, t: Texture.Constructor) => t.isBuffered
      case _ => false
    }
    def textures = attachments flatMap {
      case (_, t: Texture.Constructor) => Some(t)
      case _ => None
    }
  }
  case class Loaded(constructor: Constructor,
                    frontId: Int,
                    backId: Option[Int])
      extends LoadedFramebuffer {
    def flip: Option[Loaded] =
      backId.map(b => copy(frontId = b, backId = Some(frontId)))
  }
}

object Sampler {
  case class Constructor(wrapS: TextureWrap, wrapT: TextureWrap, minFilter: TextureMinFilter, magFilter: TextureMagFilter)
  case class Loaded(constructor: Constructor, id: Int)
}

case class DrawOp(model: Model,
                  program: Program.Unlinked, 
  textureUniforms: Map[String, Texture.Constructor],
                  framebuffer: Framebuffer) {
  val vertexModel: Model.VertexRef = model.vertex
  val vertexData: VertexData.Ref = vertexModel.ref
  val vertexBuffer: VertexBuffer.Constructor = vertexData.buffer
  val attributes = vertexBuffer.attributes
  val elementModel: Model.ElementRef = model.element
  val elementData: ElementData.Ref = elementModel.ref
  val elementBuffer: ElementBuffer.Constructor = elementData.buffer
}
