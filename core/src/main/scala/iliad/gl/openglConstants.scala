package iliad
package gl

abstract class IntConstant(val value: Int)
abstract class BitmaskConstant(value: Int) extends IntConstant(value)
abstract class LongConstant(val value: Long)

sealed trait Texture extends IntConstant
sealed trait ColorAttachment
    extends IntConstant
    with ColorBuffer
    with ColorOutputTarget

sealed trait DrawBuffer extends IntConstant with Parameter
sealed trait Capability extends IntConstant with Parameter

sealed trait ChannelBit extends IntConstant
sealed trait BlitFilter extends IntConstant
sealed trait ShaderType extends IntConstant
sealed trait ShaderParameter extends IntConstant
sealed trait TrueFalse extends IntConstant
sealed trait BufferTarget extends IntConstant
sealed trait BufferUsage extends IntConstant
sealed trait VertexAttribType extends IntConstant
sealed trait VertexAttribIType extends IntConstant
sealed trait FramebufferTarget extends IntConstant
sealed trait FramebufferStatus extends IntConstant
sealed trait FramebufferAttachment extends IntConstant
sealed trait FramebufferTexTarget extends IntConstant
sealed trait RenderbufferInternalFormat extends IntConstant
sealed trait TextureTarget extends IntConstant
sealed trait TextureParameter extends IntConstant
sealed trait TextureCompareMode extends IntConstant
sealed trait TextureMinFilter extends IntConstant
sealed trait TextureMagFilter extends IntConstant
sealed trait TextureSwizzle extends IntConstant
sealed trait TextureWrap extends IntConstant
sealed trait TextureCompareFunc extends IntConstant
sealed trait TextureInternalFormat extends IntConstant
sealed trait TextureSizedInternalFormat extends TextureInternalFormat
sealed trait TextureUnsizedInternalFormat extends TextureInternalFormat
sealed trait TextureFormat extends IntConstant
sealed trait TexturePixelType extends IntConstant
sealed trait PixelStoreParameter extends IntConstant with Parameter
sealed trait PrimitiveType extends IntConstant
sealed trait IndexType extends IntConstant
sealed trait Parameter extends IntConstant
sealed trait SamplerParameter extends IntConstant
sealed trait ColorOutputTarget extends IntConstant
sealed trait Channel extends IntConstant
sealed trait ColorBuffer extends IntConstant
sealed trait BlendMode extends IntConstant
sealed trait BlendFactor extends IntConstant
sealed trait TextureCompressedFormat extends IntConstant
sealed trait CullFaceMode extends IntConstant
sealed trait DepthFunc extends IntConstant
sealed trait BufferParameter extends IntConstant
sealed trait ErrorCode extends IntConstant
