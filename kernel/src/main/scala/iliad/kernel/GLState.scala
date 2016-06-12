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
  buffers: List[LoadedBuffer]
)

object GLLoadState {

  def empty: GLLoadState = GLLoadState(Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil)

  val _vertexShaders: Lens[GLLoadState, List[Loaded[VertexShader]]] = GenLens[GLLoadState](_.vertexShaders)
  val _fragmentShaders: Lens[GLLoadState, List[Loaded[FragmentShader]]] = GenLens[GLLoadState](_.fragmentShaders)
  val _programs: Lens[GLLoadState, List[LoadedProgram]] = GenLens[GLLoadState](_.programs)
  val _samplers: Lens[GLLoadState, List[Loaded[Sampler]]] = GenLens[GLLoadState](_.samplers)
  val _textures: Lens[GLLoadState, List[LoadedTexture]] = GenLens[GLLoadState](_.textures)
  val _renderbuffers: Lens[GLLoadState, List[LoadedRenderbuffer]] = GenLens[GLLoadState](_.renderbuffers)
  val _framebuffers: Lens[GLLoadState, List[LoadedFramebuffer]] = GenLens[GLLoadState](_.framebuffers)
  val _buffers: Lens[GLLoadState, List[LoadedBuffer]] = GenLens[GLLoadState](_.buffers)
}

case class GLState(load: GLLoadState)

object GLState {

  def empty = GLState(GLLoadState.empty)

  val _load: Lens[GLState, GLLoadState] = GenLens[GLState](_.load)

  object Zoom {
    val _vertexShaders: Lens[GLState, List[Loaded[VertexShader]]] = _load ^|-> GLLoadState._vertexShaders
    val _fragmentShaders: Lens[GLState, List[Loaded[FragmentShader]]] = _load ^|-> GLLoadState._fragmentShaders
    val _programs: Lens[GLState, List[LoadedProgram]] = _load ^|-> GLLoadState._programs
    val _samplers: Lens[GLState, List[Loaded[Sampler]]] = _load ^|-> GLLoadState._samplers
    val _textures: Lens[GLState, List[LoadedTexture]] = _load ^|-> GLLoadState._textures
    val _renderbuffers: Lens[GLState, List[LoadedRenderbuffer]] = _load ^|-> GLLoadState._renderbuffers
    val _framebuffers: Lens[GLState, List[LoadedFramebuffer]] = _load ^|-> GLLoadState._framebuffers
    val _buffers: Lens[GLState, List[LoadedBuffer]] = _load ^|-> GLLoadState._buffers
  }
}
