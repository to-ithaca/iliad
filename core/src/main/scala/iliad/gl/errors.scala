package iliad
package gl

sealed trait GLError extends IliadError

case class VertexDataAlreadyLoaded(r: VertexData.Ref) extends GLError {
  def message: String = s"Vertex data has already been loaded: $r"
}

case class ElementDataAlreadyLoaded(r: ElementData.Ref) extends GLError {
  def message: String = s"Element data has already been loaded: $r"
}

case class TextureNotLoadedError(t: Texture.Constructor) extends GLError {
  def message: String = s"Texture has not been loaded: $t"
}
case class SamplerNotLoadedError(s: Sampler.Constructor) extends GLError {
  def message: String = s"Sampler has not been loaded: $s"
}
case class RenderbufferNotLoadedError(r: Renderbuffer.Constructor)
    extends GLError {
  def message: String = s"Renderbuffer has not been loaded: $r"
}
case class ProgramNotLoadedError(p: Program.Unlinked) extends GLError {
  def message: String = s"Program has not been loaded: $p"
}
case class VertexBufferNotLoadedError(v: VertexBuffer.Constructor)
    extends GLError {
  def message: String = s"Vertex buffer has not been loaded: $v"
}
case class VertexDataNotLoadedError(v: VertexData.Ref) extends GLError {
  def message: String = s"Vertex data has not been loaded: $v"
}
case class ElementBufferNotLoadedError(e: ElementBuffer.Constructor)
    extends GLError {
  def message: String = s"Element buffer has not been loaded: $e"
}
case class ElementDataNotLoadedError(e: ElementData.Ref) extends GLError {
  def message: String = s"Element data has not been loaded $e"
}
case class FramebufferNotLoadedError(f: Framebuffer.Constructor)
    extends GLError {
  def message: String = s"Framebuffer has not been loaded $f"
}

case class UnsetTextureUniformError(p: Program.Unlinked, name: String)
    extends GLError {
  def message: String = s"Texture uniform $name is not suppled for program: $p"
}
case class UnsetUniformError(p: Program.Unlinked, unform: Uniform.Constructor)
    extends GLError {
  def message: String =
    s"Uniform ${unform.name} is not suppled for program: $p"
}

case class UndefinedAttributeError(p: Program.Unlinked,
                                   a: Attribute.Constructor)
    extends GLError {
  def message: String = s"Attribute $a is not defined in program $p"
}
case class CallFailedError(method: String, code: ErrorCode) extends GLError {
  def message: String = s"GL method $method failed with error code $code"
}
case class CallFailedUnknownError(method: String, code: Int) extends GLError {
  def message: String =
    s"""GL method $method failed with undefined error $code.
Please look this value up and label it as an ErrorCode.
"""
}
case class ShaderCompileError(log: String) extends GLError {
  def message: String = s"""Shader failed to compile. Log:
$log"""
}
case class ProgramLinkError(log: String) extends GLError {
  def message: String = s"""Program failed to compile. Log:
$log"""
}

sealed trait EGLError
case object EGLGetDisplayError extends EGLError {
  def message: String = "Failed to get EGL display"
}
case object EGLBindAPIError extends EGLError {
  def message: String = "Failed to bind OpenGLES API"
}
case class EGLCallFailedError(method: String, code: EGLErrorCode)
    extends EGLError {
  def message: String = "EGL method $method failed with error code $code"
}
case class EGLCallFailedUnknownError(method: String, code: Int)
    extends EGLError {
  def message: String =
    s"""EGL method $method failed with undefined error $code.
Please look this value up and label it as an EGLErrorCode.
"""
}

case object EGLSwapBuffersError extends EGLError {
  def message: String = "Failed to swap EGL buffers"
}
case object EGLMakeCurrentError extends EGLError {
  def message: String = "Failed to make EGL context current"
}
case class EGLCreateContextError(
    as: Attributes[ContextAttrib, ContextAttribValue])
    extends EGLError {
  def message: String = s"Failed to create context from attributes: $as"
}
case class EGLCreateSurfaceError(
    as: Attributes[WindowAttrib, WindowAttribValue])
    extends EGLError {
  def message: String = s"Failed to create EGL surface from attributes: $as"
}
case class EGLSwapIntervalError(interval: Int) extends EGLError {
  def message: String = s"Failed to set EGL swap interval to $interval"
}
case class EGLConfigError(as: Attributes[ConfigAttrib, ConfigAttribValue])
    extends EGLError {
  def message: String =
    s"Failed to choose EGL config using context attributes $as"
}
