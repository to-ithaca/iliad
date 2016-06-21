package iliad
package kernel

import GL._

import cats._
import cats.data._
import cats.implicits._

import monocle._
import monocle.macros._

case class GLLoadState(
    vertexShaders: List[Loaded[VertexShader]],
    fragmentShaders: List[Loaded[FragmentShader]],
    programs: List[LoadedProgram],
    samplers: List[Loaded[Sampler]],
    textures: List[LoadedTexture],
    renderbuffers: List[LoadedRenderbuffer],
    framebuffers: List[LoadedFramebuffer],
    buffers: List[LoadedBuffer],
    models: List[LoadedModel]
)

object GLLoadState {

  def empty: GLLoadState =
    GLLoadState(Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil)

  val _vertexShaders: Lens[GLLoadState, List[Loaded[VertexShader]]] =
    GenLens[GLLoadState](_.vertexShaders)
  val _fragmentShaders: Lens[GLLoadState, List[Loaded[FragmentShader]]] =
    GenLens[GLLoadState](_.fragmentShaders)
  val _programs: Lens[GLLoadState, List[LoadedProgram]] =
    GenLens[GLLoadState](_.programs)
  val _samplers: Lens[GLLoadState, List[Loaded[Sampler]]] =
    GenLens[GLLoadState](_.samplers)
  val _textures: Lens[GLLoadState, List[LoadedTexture]] =
    GenLens[GLLoadState](_.textures)
  val _renderbuffers: Lens[GLLoadState, List[LoadedRenderbuffer]] =
    GenLens[GLLoadState](_.renderbuffers)
  val _framebuffers: Lens[GLLoadState, List[LoadedFramebuffer]] =
    GenLens[GLLoadState](_.framebuffers)
  val _buffers: Lens[GLLoadState, List[LoadedBuffer]] =
    GenLens[GLLoadState](_.buffers)
  val _models: Lens[GLLoadState, List[LoadedModel]] =
    GenLens[GLLoadState](_.models)
}

case class GLDrawState(
    framebuffer: Int,
    capabilities: Map[Capability, Boolean],
    colorMask: ColorMask,
    program: Option[LoadedProgram],
    buffers: Map[BufferTarget, Int],
    textures: Map[Texture, Int],
    activeTexture: Texture
)

object GLDrawState {
  def empty: GLDrawState =
    GLDrawState(0,
                Map(GL_DEPTH_TEST -> false),
                ColorMask(true, true, true, true),
                None,
                Map.empty,
                Map.empty,
                GL_TEXTURE0)

  val _framebuffer: Lens[GLDrawState, Int] =
    GenLens[GLDrawState](_.framebuffer)
  val _capabilities: Lens[GLDrawState, Map[Capability, Boolean]] =
    GenLens[GLDrawState](_.capabilities)
  val _colorMask: Lens[GLDrawState, ColorMask] =
    GenLens[GLDrawState](_.colorMask)
  val _program: Lens[GLDrawState, Option[LoadedProgram]] =
    GenLens[GLDrawState](_.program)
  val _buffers: Lens[GLDrawState, Map[BufferTarget, Int]] =
    GenLens[GLDrawState](_.buffers)
  val _textures: Lens[GLDrawState, Map[Texture, Int]] =
    GenLens[GLDrawState](_.textures)
  val _activeTexture: Lens[GLDrawState, Texture] =
    GenLens[GLDrawState](_.activeTexture)
}

case class GLState(load: GLLoadState, draw: GLDrawState)

object GLState {

  def empty = GLState(GLLoadState.empty, GLDrawState.empty)

  val _load: Lens[GLState, GLLoadState] = GenLens[GLState](_.load)
  val _draw: Lens[GLState, GLDrawState] = GenLens[GLState](_.draw)

  object Zoom {
    val _vertexShaders: Lens[GLState, List[Loaded[VertexShader]]] =
      _load ^|-> GLLoadState._vertexShaders
    val _fragmentShaders: Lens[GLState, List[Loaded[FragmentShader]]] =
      _load ^|-> GLLoadState._fragmentShaders
    val _programs: Lens[GLState, List[LoadedProgram]] =
      _load ^|-> GLLoadState._programs
    val _samplers: Lens[GLState, List[Loaded[Sampler]]] =
      _load ^|-> GLLoadState._samplers
    val _textures: Lens[GLState, List[LoadedTexture]] =
      _load ^|-> GLLoadState._textures
    val _renderbuffers: Lens[GLState, List[LoadedRenderbuffer]] =
      _load ^|-> GLLoadState._renderbuffers
    val _framebuffers: Lens[GLState, List[LoadedFramebuffer]] =
      _load ^|-> GLLoadState._framebuffers
    val _buffers: Lens[GLState, List[LoadedBuffer]] =
      _load ^|-> GLLoadState._buffers
    val _models: Lens[GLState, List[LoadedModel]] =
      _load ^|-> GLLoadState._models

    val _framebuffer: Lens[GLState, Int] = _draw ^|-> GLDrawState._framebuffer
    val _capabilities: Lens[GLState, Map[Capability, Boolean]] =
      _draw ^|-> GLDrawState._capabilities
    val _colorMask: Lens[GLState, ColorMask] =
      _draw ^|-> GLDrawState._colorMask
    val _program: Lens[GLState, Option[LoadedProgram]] =
      _draw ^|-> GLDrawState._program
    val _boundBuffers: Lens[GLState, Map[BufferTarget, Int]] =
      _draw ^|-> GLDrawState._buffers
    val _boundTextures: Lens[GLState, Map[Texture, Int]] =
      _draw ^|-> GLDrawState._textures
    val _activeTexture: Lens[GLState, Texture] =
      _draw ^|-> GLDrawState._activeTexture
  }

  def vertexShader(s: VertexShader)(
      st: GLState): Option[Loaded[VertexShader]] =
    Zoom._vertexShaders.get(st).find(_.glObject == s)

  def addVertexShader(s: Loaded[VertexShader])(st: GLState): GLState =
    Zoom._vertexShaders.modify(s :: _)(st)

  def fragmentShader(s: FragmentShader)(
      st: GLState): Option[Loaded[FragmentShader]] =
    Zoom._fragmentShaders.get(st).find(_.glObject == s)

  def addFragmentShader(s: Loaded[FragmentShader])(st: GLState): GLState =
    Zoom._fragmentShaders.modify(s :: _)(st)

  def program(p: Program)(s: GLState): Option[LoadedProgram] =
    Zoom._programs.get(s).find(_.program == p)

  def addProgram(p: LoadedProgram)(s: GLState): GLState =
    Zoom._programs.modify(p :: _)(s)

  def sampler(s: Sampler)(st: GLState): Option[Loaded[Sampler]] =
    Zoom._samplers.get(st).find(_.glObject == s)

  def addSampler(s: Loaded[Sampler])(st: GLState): GLState =
    Zoom._samplers.modify(s :: _)(st)

  def renderbuffer(r: RenderbufferInstance)(
      s: GLState): Option[LoadedRenderbuffer] =
    Zoom._renderbuffers.get(s).find(_.instance == r)

  def addRenderbuffer(r: LoadedRenderbuffer)(s: GLState): GLState =
    Zoom._renderbuffers.modify(r :: _)(s)

  def texture(t: TextureInstance)(s: GLState): Option[LoadedTexture] =
    Zoom._textures.get(s).find(_.instance == t)

  def addTexture(t: LoadedTexture)(s: GLState): GLState =
    Zoom._textures.modify(t :: _)(s)

  def framebuffer(f: Framebuffer)(s: GLState): Option[LoadedFramebuffer] =
    Zoom._framebuffers.get(s).find(_.framebuffer == f)

  def addFramebuffer(f: LoadedFramebuffer)(s: GLState): GLState =
    Zoom._framebuffers.modify(f :: _)(s)

  def buffer(b: BufferInstance, t: BufferTarget)(
      s: GLState): Option[LoadedBuffer] =
    Zoom._buffers.get(s).find(bb => bb.instance == b && bb.target == t)

  def addBuffer(b: LoadedBuffer)(s: GLState): GLState =
    Zoom._buffers.modify(b :: _)(s)

  def replaceBuffer(prev: LoadedBuffer)(
      next: LoadedBuffer)(s: GLState): GLState =
    Zoom._buffers.modify(_.map {
      case b if b == prev => next
      case x => x
    })(s)

  def addModel(m: LoadedModel)(s: GLState): GLState =
    Zoom._models.modify(m :: _)(s)

  def hasCapability(c: Capability, v: Boolean)(s: GLState): Boolean =
    Zoom._capabilities.get(s).get(c) == Some(v)

  def bind(c: Capability, b: Boolean)(s: GLState): GLState =
    Zoom._capabilities.modify(_ + (c -> b))(s)

  def hasColorMask(m: ColorMask)(s: GLState): Boolean =
    Zoom._colorMask.get(s) == m

  def bind(m: ColorMask)(s: GLState): GLState = Zoom._colorMask.set(m)(s)

  def hasFramebuffer(id: Int)(s: GLState): Boolean =
    Zoom._framebuffer.get(s) == id

  def bindFramebuffer(id: Int)(s: GLState): GLState =
    Zoom._framebuffer.set(id)(s)

  def hasProgram(p: LoadedProgram)(s: GLState): Boolean =
    Zoom._program.get(s) == p

  def bind(p: LoadedProgram)(s: GLState): GLState =
    Zoom._program.set(Some(p))(s)

  def hasBuffer(target: BufferTarget, id: Int)(s: GLState): Boolean =
    Zoom._boundBuffers.get(s).get(target) == Some(id)

  def bindBuffer(target: BufferTarget, id: Int)(s: GLState): GLState =
    Zoom._boundBuffers.modify(_ + (target -> id))(s)

  def hasActiveTexture(t: Texture)(s: GLState): Boolean =
    Zoom._activeTexture.get(s) == t

  def bind(t: Texture)(s: GLState): GLState = Zoom._activeTexture.set(t)(s)

  def hasTexture(id: Int)(s: GLState): Boolean =
    Zoom._boundTextures.get(s).get(Zoom._activeTexture.get(s)) == Some(id)

  def bindTexture(id: Int)(s: GLState): GLState =
    Zoom._boundTextures.modify(_ + (Zoom._activeTexture.get(s) -> id))(s)

  //TODO: fix this
  def hasBoundFramebuffer(f: Framebuffer)(s: GLState): Boolean =
    framebuffer(f)(s).exists(ff => hasFramebuffer(ff.id.front)(s))

  def existingFramebufferId(f: Framebuffer)(s: GLState): String Xor Int =
    framebuffer(f)(s)
      .map(_.id.front)
      .toRightXor(s"Framebuffer has not been loaded $f")

  def hasBoundProgram(p: LoadedProgram)(s: GLState): Boolean =
    Zoom._program.get(s).contains(p)

  def existingProgram(p: Program)(s: GLState): String Xor LoadedProgram =
    program(p)(s).toRightXor(s"Program has not been loaded: $p")

  def hasBoundBuffer(b: BufferInstance, t: BufferTarget)(s: GLState): Boolean =
    Zoom._buffers.get(s).find(_.instance == b).map(b => hasBuffer(t, b.id)(s)) == Some(
        true)

  def existingBuffer(b: BufferInstance, t: BufferTarget)(
      s: GLState): String Xor LoadedBuffer =
    buffer(b, t)(s).toRightXor(s"Buffer has not been loaded: $b $t")

  def existingModel(m: Model)(s: GLState): String Xor LoadedModel =
    Zoom._models
      .get(s)
      .find(_.model == m)
      .toRightXor(s"Model $m has not been loaded")
}
