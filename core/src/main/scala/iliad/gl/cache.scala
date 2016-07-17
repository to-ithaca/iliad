package iliad
package gl

import iliad.CatsExtra._

import cats._
import cats.data.{State => CatsState, Xor}
import cats.free._
import cats.implicits._

import monocle._
import monocle.macros._
import monocle.function.all._
import monocle.syntax.all._
import monocle.std.map._

object Cache {
  type DSL[A] = Free[Cache, A]
  type Effect[A] = CatsState[State, A]

  def get(vs: VertexShader.Source): DSL[Option[VertexShader.Compiled]] =
    VertexShaderGet(vs).free
  def put(vs: VertexShader.Compiled): DSL[Unit] = VertexShaderPut(vs).free

  def get(fs: FragmentShader.Source): DSL[Option[FragmentShader.Compiled]] =
    FragmentShaderGet(fs).free
  def put(fs: FragmentShader.Compiled): DSL[Unit] =
    FragmentShaderPut(fs).free

  def get(p: Program.Unlinked): DSL[Option[Program.Linked]] =
    ProgramGet(p).free
  def put(p: Program.Linked): DSL[Unit] = ProgramPut(p).free

  def get(v: VertexData.Ref): DSL[Option[VertexData.Loaded]] =
    VertexDataGet(v).free
  def get(b: VertexBuffer.Constructor): DSL[Option[VertexBuffer.Loaded]] =
    VertexBufferGet(b).free
  def put(b: VertexBuffer.Update): DSL[Unit] =
    VertexBufferPut(b.buffer).free >> VertexDataPut(b.data).free

  def get(e: ElementData.Ref): DSL[Option[ElementData.Loaded]] =
    ElementDataGet(e).free
  def get(b: ElementBuffer.Constructor): DSL[Option[ElementBuffer.Loaded]] =
    ElementBufferGet(b).free
  def put(b: ElementBuffer.Update): DSL[Unit] =
    ElementBufferPut(b.buffer).free >> ElementDataPut(b.data).free

  def get(t: Texture.Constructor): DSL[Option[Texture.Loaded]] =
    TextureGet(t).free
  def put(t: Texture.Loaded): DSL[Unit] = TexturePut(t).free

  def get(r: Renderbuffer.Constructor): DSL[Option[Renderbuffer.Loaded]] =
    RenderbufferGet(r).free
  def put(r: Renderbuffer.Loaded): DSL[Unit] = RenderbufferPut(r).free

  def get(f: Framebuffer.Constructor): DSL[Option[Framebuffer.Loaded]] =
    FramebufferGet(f).free
  def put(f: Framebuffer.Loaded): DSL[Unit] = FramebufferPut(f).free

  def get(s: Sampler.Constructor): DSL[Option[Sampler.Loaded]] =
    SamplerGet(s).free

  def put(s: Sampler.Loaded): DSL[Unit] = SamplerPut(s).free

  def ensure[A, L](dsl: DSL[Option[A]], l: => L): DSL[L Xor A] =
    dsl.map(_.toRightXor(l))

  case class State(
      vertexShaders: Map[VertexShader.Source, VertexShader.Compiled],
      fragmentShaders: Map[FragmentShader.Source, FragmentShader.Compiled],
      programs: Map[Program.Unlinked, Program.Linked],
      vertexData: Map[VertexData.Ref, VertexData.Loaded],
      elementData: Map[ElementData.Ref, ElementData.Loaded],
      vertexBuffers: Map[VertexBuffer.Constructor, VertexBuffer.Loaded],
      elementBuffers: Map[ElementBuffer.Constructor, ElementBuffer.Loaded],
      textures: Map[Texture.Constructor, Texture.Loaded],
      renderbuffers: Map[Renderbuffer.Constructor, Renderbuffer.Loaded],
      framebuffers: Map[Framebuffer.Constructor, Framebuffer.Loaded],
      samplers: Map[Sampler.Constructor, Sampler.Loaded])

  object State {
    val empty: State = State(
        Map.empty,
        Map.empty,
        Map.empty,
        Map.empty,
        Map.empty,
        Map.empty,
        Map.empty,
        Map.empty,
        Map.empty,
        Map(Framebuffer.default -> Framebuffer.defaultLoaded),
        Map.empty)
  }
}

sealed trait Cache[A]

case class VertexShaderGet(vs: VertexShader.Source)
    extends Cache[Option[VertexShader.Compiled]]
case class VertexShaderPut(vs: VertexShader.Compiled) extends Cache[Unit]
case class FragmentShaderGet(fs: FragmentShader.Source)
    extends Cache[Option[FragmentShader.Compiled]]
case class FragmentShaderPut(fs: FragmentShader.Compiled) extends Cache[Unit]
case class ProgramGet(p: Program.Unlinked)
    extends Cache[Option[Program.Linked]]
case class ProgramPut(p: Program.Linked) extends Cache[Unit]
case class VertexDataGet(ref: VertexData.Ref)
    extends Cache[Option[VertexData.Loaded]]
case class VertexDataPut(v: VertexData.Loaded) extends Cache[Unit]
case class VertexBufferGet(c: VertexBuffer.Constructor)
    extends Cache[Option[VertexBuffer.Loaded]]
case class VertexBufferPut(v: VertexBuffer.Loaded) extends Cache[Unit]

case class ElementDataGet(ref: ElementData.Ref)
    extends Cache[Option[ElementData.Loaded]]
case class ElementDataPut(e: ElementData.Loaded) extends Cache[Unit]
case class ElementBufferGet(c: ElementBuffer.Constructor)
    extends Cache[Option[ElementBuffer.Loaded]]
case class ElementBufferPut(e: ElementBuffer.Loaded) extends Cache[Unit]

case class TextureGet(t: Texture.Constructor)
    extends Cache[Option[Texture.Loaded]]
case class TexturePut(t: Texture.Loaded) extends Cache[Unit]

case class RenderbufferGet(r: Renderbuffer.Constructor)
    extends Cache[Option[Renderbuffer.Loaded]]
case class RenderbufferPut(r: Renderbuffer.Loaded) extends Cache[Unit]

case class FramebufferGet(f: Framebuffer.Constructor)
    extends Cache[Option[Framebuffer.Loaded]]
case class FramebufferPut(f: Framebuffer.Loaded) extends Cache[Unit]

case class SamplerGet(s: Sampler.Constructor)
    extends Cache[Option[Sampler.Loaded]]
case class SamplerPut(s: Sampler.Loaded) extends Cache[Unit]

private object CacheParser extends (Cache ~> Cache.Effect) {
  private val _vertexShaders: Lens[
      Cache.State,
      Map[VertexShader.Source, VertexShader.Compiled]] =
    GenLens[Cache.State](_.vertexShaders)
  private val _fragmentShaders: Lens[
      Cache.State,
      Map[FragmentShader.Source, FragmentShader.Compiled]] =
    GenLens[Cache.State](_.fragmentShaders)
  private val _programs: Lens[Cache.State,
                              Map[Program.Unlinked, Program.Linked]] =
    GenLens[Cache.State](_.programs)
  private val _vertexData: Lens[Cache.State,
                                Map[VertexData.Ref, VertexData.Loaded]] =
    GenLens[Cache.State](_.vertexData)
  private val _vertexBuffers: Lens[
      Cache.State,
      Map[VertexBuffer.Constructor, VertexBuffer.Loaded]] =
    GenLens[Cache.State](_.vertexBuffers)
  private val _elementData: Lens[Cache.State,
                                 Map[ElementData.Ref, ElementData.Loaded]] =
    GenLens[Cache.State](_.elementData)
  private val _elementBuffers: Lens[
      Cache.State,
      Map[ElementBuffer.Constructor, ElementBuffer.Loaded]] =
    GenLens[Cache.State](_.elementBuffers)
  private val _textures: Lens[Cache.State,
                              Map[Texture.Constructor, Texture.Loaded]] =
    GenLens[Cache.State](_.textures)
  private val _renderbuffers: Lens[
      Cache.State,
      Map[Renderbuffer.Constructor, Renderbuffer.Loaded]] =
    GenLens[Cache.State](_.renderbuffers)
  private val _framebuffers: Lens[
      Cache.State,
      Map[Framebuffer.Constructor, Framebuffer.Loaded]] =
    GenLens[Cache.State](_.framebuffers)
  private val _samplers: Lens[Cache.State,
                              Map[Sampler.Constructor, Sampler.Loaded]] =
    GenLens[Cache.State](_.samplers)

  def apply[A](cached: Cache[A]): Cache.Effect[A] =
    cached match {
      case VertexShaderGet(vs) =>
        CatsState.inspect(_ &|-> _vertexShaders ^|-> at(vs) get)
      case VertexShaderPut(vs) =>
        CatsState.modify(_ &|-> _vertexShaders ^|-> at(vs.source) set Some(vs))
      case FragmentShaderGet(fs) =>
        CatsState.inspect(_ &|-> _fragmentShaders ^|-> at(fs) get)
      case FragmentShaderPut(fs) =>
        CatsState.modify(
            _ &|-> _fragmentShaders ^|-> at(fs.source) set Some(fs))
      case ProgramGet(p) => CatsState.inspect(_ &|-> _programs ^|-> at(p) get)
      case ProgramPut(p) =>
        CatsState.modify(_ &|-> _programs ^|-> at(p.unlinked) set Some(p))
      case VertexDataGet(d) =>
        CatsState.inspect(_ &|-> _vertexData ^|-> at(d) get)
      case VertexDataPut(d) =>
        CatsState.modify(_ &|-> _vertexData ^|-> at(d.ref) set Some(d))
      case VertexBufferGet(b) =>
        CatsState.inspect(_ &|-> _vertexBuffers ^|-> at(b) get)
      case VertexBufferPut(b) =>
        CatsState.modify(
            _ &|-> _vertexBuffers ^|-> at(b.constructor) set Some(b))
      case ElementDataGet(d) =>
        CatsState.inspect(_ &|-> _elementData ^|-> at(d) get)
      case ElementDataPut(d) =>
        CatsState.modify(_ &|-> _elementData ^|-> at(d.ref) set Some(d))
      case ElementBufferGet(b) =>
        CatsState.inspect(_ &|-> _elementBuffers ^|-> at(b) get)
      case ElementBufferPut(b) =>
        CatsState.modify(
            _ &|-> _elementBuffers ^|-> at(b.constructor) set Some(b))
      case TextureGet(t) =>
        CatsState.inspect(_ &|-> _textures ^|-> at(t) get)
      case TexturePut(t) =>
        CatsState.modify(_ &|-> _textures ^|-> at(t.constructor) set Some(t))
      case RenderbufferGet(r) =>
        CatsState.inspect(_ &|-> _renderbuffers ^|-> at(r) get)
      case RenderbufferPut(r) =>
        CatsState.modify(
            _ &|-> _renderbuffers ^|-> at(r.constructor) set Some(r))
      case FramebufferGet(f) =>
        CatsState.inspect(_ &|-> _framebuffers ^|-> at(f) get)
      case FramebufferPut(f) =>
        CatsState.modify(
            _ &|-> _framebuffers ^|-> at(f.constructor) set Some(f))

      case SamplerGet(s) =>
        CatsState.inspect(_ &|-> _samplers ^|-> at(s) get)
      case SamplerPut(s) =>
        CatsState.modify(_ &|-> _samplers ^|-> at(s.constructor) set Some(s))

    }
}
