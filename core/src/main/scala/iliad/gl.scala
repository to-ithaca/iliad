package iliad

import iliad.kernel.GLConstants._

case class VertexShader(source: String)
case class FragmentShader(source: String)
case class Program(vertex: VertexShader, fragment: FragmentShader)

case class Sampler(
  magFilter: Option[TextureMagFilter],
  minFilter: Option[TextureMinFilter],
  wrapS: Option[TextureWrap],
  wrapT: Option[TextureWrap]
)

object Sampler {
  val image = Sampler(None, None, Some(GL_CLAMP_TO_EDGE), Some(GL_CLAMP_TO_EDGE))
  val lookupTable = Sampler(Some(GL_NEAREST), Some(GL_NEAREST), Some(GL_CLAMP_TO_EDGE), Some(GL_CLAMP_TO_EDGE))
}

//TODO: we've missed out the unique id, whether it is buffered, and the viewport
case class Texture(format: TextureFormat, internalFormat: TextureInternalFormat, `type`: TexturePixelType)


//TODO: this is not unique!
case class Renderbuffer(internalFormat: RenderbufferInternalFormat)

case class Framebuffer(renderbuffers: Map[FramebufferAttachment, Renderbuffer], textures: Map[FramebufferAttachment, Texture], viewport: Rect[Int])


//TODO: this is confusing - very much like VertexAttribType
sealed trait AttributeType

case class VertexBuffer(attributes: List[(String, AttributeType)])

//Note: do element buffers need to exist?

case object ColorMask
case object Clear
