package iliad
package gl

import iliad.algebra._
import iliad.algebra.Rect

import iliad.std.list._
import iliad.algebra.syntax.vector._

import cats.data._
import cats.implicits._

import monocle._
import monocle.macros._
import monocle.syntax.all._

import scodec.bits._

object VertexShader {
  case class Source(text: String,
                    attributes: List[Attribute.Constructor],
                    textures: List[(String, Sampler.Constructor)],
                    uniforms: List[Uniform.Constructor]) {
    val attributeNames: List[String] = attributes.map(_.name)
    val uniformNames: List[String] = uniforms.map(_.name)
    val textureNames: List[String] = textures.map(_._1)
  }
  case class Compiled(id: Int, source: Source)
}

object FragmentShader {
  case class Source(text: String,
                    textures: List[(String, Sampler.Constructor)],
                    uniforms: List[Uniform.Constructor]) {
    val uniformNames: List[String] = uniforms.map(_.name)
    val textureNames: List[String] = textures.map(_._1)
  }
  case class Compiled(id: Int, source: Source)
}

object Program {
  case class Unlinked(vertex: VertexShader.Source,
                      fragment: FragmentShader.Source) {
    val samplers: Map[String, Sampler.Constructor] =
      (vertex.textures ++ fragment.textures).toMap
    val textureNames: List[String] = vertex.textureNames ::: fragment.textureNames
    val uniforms: List[Uniform.Constructor] = vertex.uniforms ::: fragment.uniforms
    val uniformNames: List[String] = uniforms.map(_.name)
  }

  case class Linked(id: Int,
                    unlinked: Unlinked,
                    attributes: List[(String, Int)],
                    textureUniforms: List[(String, Int)],
                    uniformNames: Map[String, Int]) {

    private def uniforms: List[Uniform.Loaded] =
      unlinked.uniforms.map(u => Uniform.Loaded(u, uniformNames(u.name)))

    def loaded(as: List[Attribute.Offset]): UndefinedAttributeError Xor Attribute.LoadedAttributes =
      attributes.traverse {
        case (name, location) => 
          as.find(_.constructor.name == name).map(Attribute.Loaded(_, location))
            .toRightXor(UndefinedAttributeError(unlinked, as, name))
      }.map(Attribute.LoadedAttributes)

    def textureUniforms(ts: Map[String, Texture.Constructor])
      : UnsetTextureUniformError Xor List[TextureUniform] =
      textureUniforms.zipWithIndex.traverse {
        case ((name, location), index) =>
          ts.get(name)
            .toRightXor(UnsetTextureUniformError(unlinked, name))
            .map(
                t =>
                  TextureUniform(Bounded.element[TextureUnit](index),
                                 location,
                                 t,
                                 unlinked.samplers(name)))
      }

    def uniforms(
        us: List[Uniform.Value]): UnsetUniformError Xor List[UniformValue] =
      uniforms.traverse { u =>
        us.find(_.constructor == u.constructor)
          .toRightXor(UnsetUniformError(unlinked, u.constructor))
          .map(v => UniformValue(u, v))
      }
  }

  case class TextureUniform(unit: TextureUnit,
                            location: Int,
                            texture: Texture.Constructor,
                            sampler: Sampler.Constructor)

  case class UniformValue(uniform: Uniform.Loaded, value: Uniform.Value)
}

object Attribute {
  case class Constructor(name: String,
                         byteSize: Int,
                         elementSize: Int,
                         `type`: VertexAttribType)

  case class Offset(constructor: Constructor, offset: Int)

  def stride(attributes: List[Constructor]): Int = 
    attributes.map(_.byteSize).sum

  def offsets(base: Int, attributes: List[Constructor]): List[Offset] =
     attributes
        .foldLeft(base -> List.empty[Offset])({
          case ((offset, acc), a) =>
            (offset + a.byteSize, (Offset(a, offset) :: acc))
        })._2

  case class Loaded(offset: Offset, location: Int)
  case class LoadedAttributes(attributes: List[Loaded])
}

object Uniform {
  case class Constructor(name: String)
  case class Loaded(constructor: Constructor, location: Int)
  case class Value(constructor: Constructor, bind: Int => OpenGL.DSL[Unit])

  def apply(name: String, bind: Int => OpenGL.DSL[Unit]): Value =
    Value(Constructor(name), bind)
}

object VertexBuffer {
  case class Constructor(name: String, attributes: List[Attribute.Constructor])
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
                    constructor: Constructor)

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
  case class Ref(name: String, buffer: VertexBuffer.Constructor)
  case class Loaded(range: DataRange, ref: Ref) {
    def offset(ref: Model.VertexRef): Int =
      range.start + ref.range.start
  }
}

object ElementData {
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

  sealed trait Data
  case class Empty(dim: Vec2i) extends Data
  case class SingleData(dim: Vec2i, pixels: ByteVector) extends Data
  case class GroupData(subData: Map[Rect[Int], ByteVector], dim: Vec2i) extends Data

  case class Format(pixel: TextureFormat,
                    internal: TextureInternalFormat,
                    pixelType: TexturePixelType,
                    bytesPerPixel: Int)

  object Format {
    val rgba: Format = Format(GL_RGBA, GL_RGBA, GL_UNSIGNED_BYTE, 4)
    val rgb: Format = Format(GL_RGB, GL_RGB, GL_UNSIGNED_BYTE, 3)
    val redInt: Format = Format(GL_RED_INTEGER, GL_R32I, GL_INT, 4)
    val depth32 = Format(GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT32F, GL_FLOAT, 4)
    val depth16 = Format(GL_DEPTH_COMPONENT, GL_DEPTH_COMPONENT16, GL_UNSIGNED_SHORT, 2)
  }

  sealed trait Constructor extends Framebuffer.AttachmentConstructor {
    def name: String
    def format: Format
  }

  case class SingleConstructor(name: String, format: Format)
      extends Constructor
  case class DoubleConstructor(name: String, format: Format)
      extends Constructor

  sealed trait Loaded extends Framebuffer.AttachmentLoaded {
    def constructor: Constructor
    def frontId: Int
  }

  case class SingleLoaded(constructor: SingleConstructor, frontId: Int)
      extends Loaded
  case class DoubleLoaded(constructor: DoubleConstructor,
                          frontId: Int,
                          backId: Int)
      extends Loaded {
    def flip: DoubleLoaded = copy(frontId = backId, backId = frontId)
  }
}

object Renderbuffer {
  case class Constructor(name: String,
                         format: RenderbufferInternalFormat,
                         viewport: Vec2i)
      extends Framebuffer.AttachmentConstructor
  case class Loaded(constructor: Constructor, id: Int)
      extends Framebuffer.AttachmentLoaded
}

object Framebuffer {
  sealed trait Constructor {
    def attachments: List[(FramebufferAttachment, AttachmentConstructor)]
    def textures: List[Texture.Constructor] =
      attachments.map(_._2).filterClass[Texture.Constructor]
  }

  sealed trait AttachmentConstructor
  sealed trait AttachmentLoaded

  case class SingleConstructor(
      attachments: List[(FramebufferAttachment, AttachmentConstructor)])
      extends Constructor

  case class DoubleConstructor(
      attachments: List[(FramebufferAttachment, AttachmentConstructor)])
      extends Constructor

  sealed trait Loaded {
    def frontId: Int
    def constructor: Constructor
  }

  case class SingleLoaded(constructor: Constructor, frontId: Int)
      extends Loaded

  case class DoubleLoaded(constructor: DoubleConstructor,
                          frontId: Int,
                          backId: Int)
      extends Loaded {
    def flip: DoubleLoaded = copy(frontId = backId, backId = frontId)
  }

  val default = SingleConstructor(Nil)
  val defaultLoaded = SingleLoaded(default, 0)
}

object Sampler {
  case class Constructor(wrapS: TextureWrap,
                         wrapT: TextureWrap,
                         minFilter: TextureMinFilter,
                         magFilter: TextureMagFilter)

  object Constructor {
    val image: Constructor =
      Constructor(GL_CLAMP_TO_EDGE, GL_CLAMP_TO_EDGE, GL_LINEAR, GL_LINEAR)
  }
  case class Loaded(constructor: Constructor, id: Int)
}

case class ColorMask(r: Boolean, g: Boolean, b: Boolean, a: Boolean)

object ColorMask {
  val none: ColorMask = ColorMask(true, true, true, true)
}

object Capabilities {
  val depth: List[Capability] = List(GL_DEPTH_TEST)
}

case class BlendFunction(src: BlendFactor, dest: BlendFactor)
case class Blend(mode: BlendMode, func: BlendFunction)

object Blend {
  val alpha: Blend = Blend(GL_FUNC_ADD, BlendFunction(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA))
}

case class DrawOp(model: Model,
                  program: Program.Unlinked,
                  textureUniforms: Map[String, Texture.Constructor],
                  uniforms: List[Uniform.Value],
                  framebuffer: Framebuffer.Constructor,
                  colorMask: ColorMask,
                  primitive: PrimitiveType,
                  capabilities: Map[Capability, Boolean],
                  blend: Option[Blend],
                  viewport: Rect[Int],
                  numInstances: Int) {
  val vertexModel: Model.VertexRef = model.vertex
  val vertexData: VertexData.Ref = vertexModel.ref
  val vertexBuffer: VertexBuffer.Constructor = vertexData.buffer
  val attributes = vertexBuffer.attributes
  val elementModel: Model.ElementRef = model.element
  val elementData: ElementData.Ref = elementModel.ref
  val elementBuffer: ElementBuffer.Constructor = elementData.buffer
}

case class ClearOp(bitMask: ChannelBitMask,
                   colour: Vec4f,
framebuffer: Framebuffer.Constructor,
viewport: Rect[Int])
