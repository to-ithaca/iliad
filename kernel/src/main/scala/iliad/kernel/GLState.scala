package iliad
package kernel

import GL._

case class GLState(load: GLLoadState)

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

//private[kernel] final class GLTracker[F[_]](gl: GL[F]) extends GL[StateEffect[F, ?]] {}
