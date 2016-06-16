package iliad
package gl

abstract class IntConstant(val value: Int)
abstract class BitmaskConstant(value: Int) extends IntConstant(value)
abstract class LongConstant(val value: Long)

sealed trait ChannelBitMask { self =>
  def value: Int = self match {
    case ChannelBitMask.BitMask(each) => each.foldLeft(0)(_.toInt | _.value)
    case ChannelBitMask.Empty => 0
  }
}

//TODO: move this somewhere else
object ChannelBitMask {
  implicit def bitToMask(bit: ChannelBit): ChannelBitMask = BitMask(Set(bit))

  case object Empty extends ChannelBitMask
  case class BitMask(each: Set[ChannelBit]) extends ChannelBitMask {
    override def toString: String = s"(${each.mkString("|")})"
  }
}

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

case object GL_FALSE extends IntConstant(0) with TrueFalse
case object GL_TRUE extends IntConstant(1) with TrueFalse
case object GL_POINTS extends IntConstant(0x0000) with PrimitiveType
case object GL_LINES extends IntConstant(0x0001) with PrimitiveType
case object GL_LINE_LOOP extends IntConstant(0x0002) with PrimitiveType
case object GL_LINE_STRIP extends IntConstant(0x0003) with PrimitiveType
case object GL_TRIANGLES extends IntConstant(0x0004) with PrimitiveType
case object GL_TRIANGLE_FAN extends IntConstant(0x0005) with PrimitiveType
case object GL_ZERO extends IntConstant(0) with TextureSwizzle with BlendFactor
case object GL_ONE extends IntConstant(1) with TextureSwizzle with BlendFactor
case object GL_SRC_COLOR extends IntConstant(0x0300) with BlendFactor
case object GL_ONE_MINUS_SRC_COLOR extends IntConstant(0x0301) with BlendFactor
case object GL_SRC_ALPHA extends IntConstant(0x0302) with BlendFactor
case object GL_ONE_MINUS_SRC_ALPHA extends IntConstant(0x0303) with BlendFactor
case object GL_DST_ALPHA extends IntConstant(0x0304) with BlendFactor
case object GL_ONE_MINUS_DST_ALPHA extends IntConstant(0x0305) with BlendFactor
case object GL_DST_COLOR extends IntConstant(0x0306) with BlendFactor
case object GL_ONE_MINUS_DST_COLOR extends IntConstant(0x0307) with BlendFactor
case object GL_SRC_ALPHA_SATURATE extends IntConstant(0x0308) with BlendFactor
case object GL_FUNC_ADD extends IntConstant(0x8006) with BlendMode
case object GL_BLEND_EQUATION extends IntConstant(0x8009)
case object GL_BLEND_EQUATION_RGB extends IntConstant(0x8009) with Parameter
case object GL_BLEND_EQUATION_ALPHA extends IntConstant(0x883D) with Parameter
case object GL_FUNC_SUBTRACT extends IntConstant(0x800A) with BlendMode
case object GL_FUNC_REVERSE_SUBTRACT extends IntConstant(0x800B) with BlendMode
case object GL_BLEND_DST_RGB extends IntConstant(0x80C8)
case object GL_BLEND_SRC_RGB extends IntConstant(0x80C9) with Parameter
case object GL_BLEND_DST_ALPHA extends IntConstant(0x80CA) with Parameter
case object GL_BLEND_SRC_ALPHA extends IntConstant(0x80CB) with Parameter
case object GL_CONSTANT_COLOR extends IntConstant(0x8001)
case object GL_ONE_MINUS_CONSTANT_COLOR extends IntConstant(0x8002)
case object GL_CONSTANT_ALPHA extends IntConstant(0x8003)
case object GL_ONE_MINUS_CONSTANT_ALPHA extends IntConstant(0x8004)
case object GL_BLEND_COLOR extends IntConstant(0x8005)
case object GL_ARRAY_BUFFER extends IntConstant(0x8892) with BufferTarget
case object GL_ELEMENT_ARRAY_BUFFER
    extends IntConstant(0x8893)
    with BufferTarget
case object GL_ARRAY_BUFFER_BINDING extends IntConstant(0x8894) with Parameter
case object GL_ELEMENT_ARRAY_BUFFER_BINDING
    extends IntConstant(0x8895)
    with Parameter
case object GL_STREAM_DRAW extends IntConstant(0x88E0) with BufferUsage
case object GL_STATIC_DRAW extends IntConstant(0x88E4) with BufferUsage
case object GL_DYNAMIC_DRAW extends IntConstant(0x88E8) with BufferUsage
case object GL_BUFFER_SIZE extends IntConstant(0x8764) with BufferParameter
case object GL_BUFFER_USAGE extends IntConstant(0x8765) with BufferParameter
case object GL_CURRENT_VERTEX_ATTRIB extends IntConstant(0x8626)
case object GL_FRONT extends IntConstant(0x0404) with CullFaceMode
case object GL_BACK
    extends IntConstant(0x0405)
    with ColorOutputTarget
    with ColorBuffer
    with CullFaceMode
case object GL_FRONT_AND_BACK extends IntConstant(0x0408) with CullFaceMode
case object GL_TEXTURE_2D
    extends IntConstant(0x0DE1)
    with FramebufferTexTarget
    with TextureTarget
case object GL_CULL_FACE extends IntConstant(0x0B44) with Capability
case object GL_BLEND extends IntConstant(0x0BE2) with Capability
case object GL_DITHER extends IntConstant(0x0BD0) with Capability
case object GL_STENCIL_TEST extends IntConstant(0x0B90) with Capability
case object GL_DEPTH_TEST extends IntConstant(0x0B71) with Capability
case object GL_SCISSOR_TEST extends IntConstant(0x0C11) with Capability
case object GL_POLYGON_OFFSET_FILL extends IntConstant(0x8037) with Capability
case object GL_SAMPLE_ALPHA_TO_COVERAGE
    extends IntConstant(0x809E)
    with Capability
case object GL_SAMPLE_COVERAGE extends IntConstant(0x80A0) with Capability
case object GL_NO_ERROR extends IntConstant(0) with ErrorCode
case object GL_INVALID_ENUM extends IntConstant(0x0500)
case object GL_INVALID_VALUE extends IntConstant(0x0501)
case object GL_INVALID_OPERATION extends IntConstant(0x0502)
case object GL_OUT_OF_MEMORY extends IntConstant(0x0505)
case object GL_CW extends IntConstant(0x0900)
case object GL_CCW extends IntConstant(0x0901)
case object GL_LINE_WIDTH extends IntConstant(0x0B21) with Parameter
case object GL_ALIASED_POINT_SIZE_RANGE
    extends IntConstant(0x846D)
    with Parameter
case object GL_ALIASED_LINE_WIDTH_RANGE
    extends IntConstant(0x846E)
    with Parameter
case object GL_CULL_FACE_MODE extends IntConstant(0x0B45) with Parameter
case object GL_FRONT_FACE extends IntConstant(0x0B46) with Parameter
case object GL_DEPTH_RANGE extends IntConstant(0x0B70) with Parameter
case object GL_DEPTH_WRITEMASK extends IntConstant(0x0B72) with Parameter
case object GL_DEPTH_CLEAR_VALUE extends IntConstant(0x0B73) with Parameter
case object GL_DEPTH_FUNC extends IntConstant(0x0B74) with Parameter
case object GL_STENCIL_CLEAR_VALUE extends IntConstant(0x0B91)
case object GL_STENCIL_FUNC extends IntConstant(0x0B92) with Parameter
case object GL_STENCIL_FAIL extends IntConstant(0x0B94)
case object GL_STENCIL_PASS_DEPTH_FAIL
    extends IntConstant(0x0B95)
    with Parameter
case object GL_STENCIL_PASS_DEPTH_PASS
    extends IntConstant(0x0B96)
    with Parameter
case object GL_STENCIL_REF extends IntConstant(0x0B97) with Parameter
case object GL_STENCIL_VALUE_MASK extends IntConstant(0x0B93) with Parameter
case object GL_STENCIL_WRITEMASK extends IntConstant(0x0B98) with Parameter
case object GL_STENCIL_BACK_FUNC extends IntConstant(0x8800) with Parameter
case object GL_STENCIL_BACK_FAIL extends IntConstant(0x8801) with Parameter
case object GL_STENCIL_BACK_PASS_DEPTH_FAIL
    extends IntConstant(0x8802)
    with Parameter
case object GL_STENCIL_BACK_PASS_DEPTH_PASS
    extends IntConstant(0x8803)
    with Parameter
case object GL_STENCIL_BACK_REF extends IntConstant(0x8CA3) with Parameter
case object GL_STENCIL_BACK_VALUE_MASK
    extends IntConstant(0x8CA4)
    with Parameter
case object GL_STENCIL_BACK_WRITEMASK
    extends IntConstant(0x8CA5)
    with Parameter
case object GL_VIEWPORT extends IntConstant(0x0BA2) with Parameter
case object GL_SCISSOR_BOX extends IntConstant(0x0C10) with Parameter
case object GL_COLOR_CLEAR_VALUE extends IntConstant(0x0C22) with Parameter
case object GL_COLOR_WRITEMASK extends IntConstant(0x0C23) with Parameter
case object GL_UNPACK_ALIGNMENT
    extends IntConstant(0x0CF5)
    with PixelStoreParameter
case object GL_PACK_ALIGNMENT
    extends IntConstant(0x0D05)
    with PixelStoreParameter
case object GL_MAX_TEXTURE_SIZE extends IntConstant(0x0D33) with Parameter
case object GL_MAX_VIEWPORT_DIMS extends IntConstant(0x0D3A) with Parameter
case object GL_SUBPIXEL_BITS extends IntConstant(0x0D50) with Parameter
case object GL_RED_BITS extends IntConstant(0x0D52) with Parameter
case object GL_GREEN_BITS extends IntConstant(0x0D53) with Parameter
case object GL_BLUE_BITS extends IntConstant(0x0D54)
case object GL_ALPHA_BITS extends IntConstant(0x0D55) with Parameter
case object GL_DEPTH_BITS extends IntConstant(0x0D56) with Parameter
case object GL_STENCIL_BITS extends IntConstant(0x0D57) with Parameter
case object GL_POLYGON_OFFSET_UNITS extends IntConstant(0x2A00) with Parameter
case object GL_POLYGON_OFFSET_FACTOR extends IntConstant(0x8038) with Parameter
case object GL_TEXTURE_BINDING_2D extends IntConstant(0x8069) with Parameter
case object GL_SAMPLE_BUFFERS extends IntConstant(0x80A8) with Parameter
case object GL_SAMPLES extends IntConstant(0x80A9) with Parameter
case object GL_SAMPLE_COVERAGE_VALUE extends IntConstant(0x80AA) with Parameter
case object GL_SAMPLE_COVERAGE_INVERT
    extends IntConstant(0x80AB)
    with Parameter
case object GL_NUM_COMPRESSED_TEXTURE_FORMATS
    extends IntConstant(0x86A2)
    with Parameter
case object GL_COMPRESSED_TEXTURE_FORMATS
    extends IntConstant(0x86A3)
    with Parameter
case object GL_DONT_CARE extends IntConstant(0x1100)
case object GL_FASTEST extends IntConstant(0x1101)
case object GL_NICEST extends IntConstant(0x1102)
case object GL_GENERATE_MIPMAP_HINT extends IntConstant(0x8192) with Parameter
case object GL_BYTE
    extends IntConstant(0x1400)
    with VertexAttribType
    with VertexAttribIType
    with TexturePixelType
    with IndexType
case object GL_UNSIGNED_BYTE
    extends IntConstant(0x1401)
    with VertexAttribType
    with VertexAttribIType
    with TexturePixelType
case object GL_SHORT
    extends IntConstant(0x1402)
    with VertexAttribType
    with VertexAttribIType
    with TexturePixelType
case object GL_UNSIGNED_SHORT
    extends IntConstant(0x1403)
    with VertexAttribType
    with VertexAttribIType
    with TexturePixelType
    with IndexType
case object GL_INT
    extends IntConstant(0x1404)
    with VertexAttribType
    with VertexAttribIType
    with TexturePixelType
case object GL_UNSIGNED_INT
    extends IntConstant(0x1405)
    with VertexAttribType
    with VertexAttribIType
    with TexturePixelType
    with IndexType
case object GL_FLOAT
    extends IntConstant(0x1406)
    with VertexAttribType
    with TexturePixelType
case object GL_FIXED extends IntConstant(0x140C) with VertexAttribType
case object GL_DEPTH_COMPONENT extends IntConstant(0x1902) with TextureFormat
case object GL_ALPHA
    extends IntConstant(0x1906)
    with TextureSwizzle
    with TextureFormat
case object GL_RGB
    extends IntConstant(0x1907)
    with TextureFormat
    with TextureUnsizedInternalFormat
case object GL_RGBA
    extends IntConstant(0x1908)
    with TextureFormat
    with TextureUnsizedInternalFormat
case object GL_LUMINANCE
    extends IntConstant(0x1909)
    with TextureFormat
    with TextureUnsizedInternalFormat
case object GL_LUMINANCE_ALPHA
    extends IntConstant(0x190A)
    with TextureFormat
    with TextureUnsizedInternalFormat
case object GL_UNSIGNED_SHORT_4_4_4_4
    extends IntConstant(0x8033)
    with TexturePixelType
case object GL_UNSIGNED_SHORT_5_5_5_1
    extends IntConstant(0x8034)
    with TexturePixelType
case object GL_UNSIGNED_SHORT_5_6_5
    extends IntConstant(0x8363)
    with TexturePixelType
case object GL_FRAGMENT_SHADER extends IntConstant(0x8B30) with ShaderType
case object GL_VERTEX_SHADER extends IntConstant(0x8B31) with ShaderType
case object GL_MAX_VERTEX_ATTRIBS extends IntConstant(0x8869) with Parameter
case object GL_MAX_VERTEX_UNIFORM_VECTORS
    extends IntConstant(0x8DFB)
    with Parameter
case object GL_MAX_VARYING_VECTORS extends IntConstant(0x8DFC) with Parameter
case object GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
    extends IntConstant(0x8B4D)
    with Parameter
case object GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
    extends IntConstant(0x8B4C)
    with Parameter
case object GL_MAX_TEXTURE_IMAGE_UNITS extends IntConstant(0x8872)
case object GL_MAX_FRAGMENT_UNIFORM_VECTORS
    extends IntConstant(0x8DFD)
    with Parameter
case object GL_SHADER_TYPE extends IntConstant(0x8B4F) with ShaderParameter
case object GL_DELETE_STATUS extends IntConstant(0x8B80) with ShaderParameter
case object GL_LINK_STATUS extends IntConstant(0x8B82)
case object GL_VALIDATE_STATUS extends IntConstant(0x8B83)
case object GL_ATTACHED_SHADERS extends IntConstant(0x8B85)
case object GL_ACTIVE_UNIFORMS extends IntConstant(0x8B86)
case object GL_ACTIVE_UNIFORM_MAX_LENGTH extends IntConstant(0x8B87)
case object GL_ACTIVE_ATTRIBUTES extends IntConstant(0x8B89)
case object GL_ACTIVE_ATTRIBUTE_MAX_LENGTH extends IntConstant(0x8B8A)
case object GL_SHADING_LANGUAGE_VERSION extends IntConstant(0x8B8C)
case object GL_CURRENT_PROGRAM extends IntConstant(0x8B8D) with Parameter
case object GL_NEVER
    extends IntConstant(0x0200)
    with TextureCompareFunc
    with DepthFunc
case object GL_LESS
    extends IntConstant(0x0201)
    with TextureCompareFunc
    with DepthFunc
case object GL_EQUAL
    extends IntConstant(0x0202)
    with TextureCompareFunc
    with DepthFunc
case object GL_LEQUAL
    extends IntConstant(0x0203)
    with TextureCompareFunc
    with DepthFunc
case object GL_GREATER
    extends IntConstant(0x0204)
    with TextureCompareFunc
    with DepthFunc
case object GL_NOTEQUAL
    extends IntConstant(0x0205)
    with TextureCompareFunc
    with DepthFunc
case object GL_GEQUAL
    extends IntConstant(0x0206)
    with TextureCompareFunc
    with DepthFunc
case object GL_ALWAYS
    extends IntConstant(0x0207)
    with TextureCompareFunc
    with DepthFunc
case object GL_KEEP extends IntConstant(0x1E00)
case object GL_REPLACE extends IntConstant(0x1E01)
case object GL_INCR extends IntConstant(0x1E02)
case object GL_DECR extends IntConstant(0x1E03)
case object GL_INVERT extends IntConstant(0x150A)
case object GL_INCR_WRAP extends IntConstant(0x8507)
case object GL_DECR_WRAP extends IntConstant(0x8508)
case object GL_VENDOR extends IntConstant(0x1F00)
case object GL_RENDERER extends IntConstant(0x1F01)
case object GL_VERSION extends IntConstant(0x1F02)
case object GL_EXTENSIONS extends IntConstant(0x1F03)
case object GL_NEAREST
    extends IntConstant(0x2600)
    with BlitFilter
    with TextureMinFilter
    with TextureMagFilter
case object GL_LINEAR
    extends IntConstant(0x2601)
    with BlitFilter
    with TextureMinFilter
    with TextureMagFilter
case object GL_NEAREST_MIPMAP_NEAREST
    extends IntConstant(0x2700)
    with TextureMinFilter
case object GL_LINEAR_MIPMAP_NEAREST
    extends IntConstant(0x2701)
    with TextureMinFilter
case object GL_NEAREST_MIPMAP_LINEAR
    extends IntConstant(0x2702)
    with TextureMinFilter
case object GL_LINEAR_MIPMAP_LINEAR
    extends IntConstant(0x2703)
    with TextureMinFilter
case object GL_TEXTURE_MAG_FILTER
    extends IntConstant(0x2800)
    with TextureParameter
    with SamplerParameter
case object GL_TEXTURE_MIN_FILTER
    extends IntConstant(0x2801)
    with TextureParameter
    with SamplerParameter
case object GL_TEXTURE_WRAP_S
    extends IntConstant(0x2802)
    with TextureParameter
    with SamplerParameter
case object GL_TEXTURE_WRAP_T
    extends IntConstant(0x2803)
    with TextureParameter
    with SamplerParameter
case object GL_TEXTURE extends IntConstant(0x1702)
case object GL_TEXTURE_CUBE_MAP extends IntConstant(0x8513) with TextureTarget
case object GL_TEXTURE_BINDING_CUBE_MAP
    extends IntConstant(0x8514)
    with Parameter
case object GL_TEXTURE_CUBE_MAP_POSITIVE_X
    extends IntConstant(0x8515)
    with FramebufferTexTarget
case object GL_TEXTURE_CUBE_MAP_NEGATIVE_X
    extends IntConstant(0x8516)
    with FramebufferTexTarget
case object GL_TEXTURE_CUBE_MAP_POSITIVE_Y
    extends IntConstant(0x8517)
    with FramebufferTexTarget
case object GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
    extends IntConstant(0x8518)
    with FramebufferTexTarget
case object GL_TEXTURE_CUBE_MAP_POSITIVE_Z
    extends IntConstant(0x8519)
    with FramebufferTexTarget
case object GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
    extends IntConstant(0x851A)
    with FramebufferTarget
case object GL_MAX_CUBE_MAP_TEXTURE_SIZE
    extends IntConstant(0x851C)
    with Parameter
case object GL_TEXTURE0 extends IntConstant(0x84C0) with Texture
case object GL_TEXTURE1 extends IntConstant(0x84C1) with Texture
case object GL_TEXTURE2 extends IntConstant(0x84C2) with Texture
case object GL_TEXTURE3 extends IntConstant(0x84C3) with Texture
case object GL_TEXTURE4 extends IntConstant(0x84C4) with Texture
case object GL_TEXTURE5 extends IntConstant(0x84C5) with Texture
case object GL_TEXTURE6 extends IntConstant(0x84C6) with Texture
case object GL_TEXTURE7 extends IntConstant(0x84C7) with Texture
case object GL_TEXTURE8 extends IntConstant(0x84C8) with Texture
case object GL_TEXTURE9 extends IntConstant(0x84C9) with Texture
case object GL_TEXTURE10 extends IntConstant(0x84CA) with Texture
case object GL_TEXTURE11 extends IntConstant(0x84CB) with Texture
case object GL_TEXTURE12 extends IntConstant(0x84CC) with Texture
case object GL_TEXTURE13 extends IntConstant(0x84CD) with Texture
case object GL_TEXTURE14 extends IntConstant(0x84CE) with Texture
case object GL_TEXTURE15 extends IntConstant(0x84CF) with Texture
case object GL_TEXTURE16 extends IntConstant(0x84D0) with Texture
case object GL_TEXTURE17 extends IntConstant(0x84D1) with Texture
case object GL_TEXTURE18 extends IntConstant(0x84D2) with Texture
case object GL_TEXTURE19 extends IntConstant(0x84D3) with Texture
case object GL_TEXTURE20 extends IntConstant(0x84D4) with Texture
case object GL_TEXTURE21 extends IntConstant(0x84D5) with Texture
case object GL_TEXTURE22 extends IntConstant(0x84D6) with Texture
case object GL_TEXTURE23 extends IntConstant(0x84D7) with Texture
case object GL_TEXTURE24 extends IntConstant(0x84D8) with Texture
case object GL_TEXTURE25 extends IntConstant(0x84D9) with Texture
case object GL_TEXTURE26 extends IntConstant(0x84DA) with Texture
case object GL_TEXTURE27 extends IntConstant(0x84DB) with Texture
case object GL_TEXTURE28 extends IntConstant(0x84DC) with Texture
case object GL_TEXTURE29 extends IntConstant(0x84DD) with Texture
case object GL_TEXTURE30 extends IntConstant(0x84DE) with Texture
case object GL_TEXTURE31 extends IntConstant(0x84DF) with Texture
case object GL_ACTIVE_TEXTURE extends IntConstant(0x84E0) with Parameter
case object GL_REPEAT extends IntConstant(0x2901) with TextureWrap
case object GL_CLAMP_TO_EDGE extends IntConstant(0x812F) with TextureWrap
case object GL_MIRRORED_REPEAT extends IntConstant(0x8370) with TextureWrap
case object GL_FLOAT_VEC2 extends IntConstant(0x8B50)
case object GL_FLOAT_VEC3 extends IntConstant(0x8B51)
case object GL_FLOAT_VEC4 extends IntConstant(0x8B52)
case object GL_INT_VEC2 extends IntConstant(0x8B53)
case object GL_INT_VEC3 extends IntConstant(0x8B54)
case object GL_INT_VEC4 extends IntConstant(0x8B55)
case object GL_BOOL extends IntConstant(0x8B56)
case object GL_BOOL_VEC2 extends IntConstant(0x8B57)
case object GL_BOOL_VEC3 extends IntConstant(0x8B58)
case object GL_BOOL_VEC4 extends IntConstant(0x8B59)
case object GL_FLOAT_MAT2 extends IntConstant(0x8B5A)
case object GL_FLOAT_MAT3 extends IntConstant(0x8B5B)
case object GL_FLOAT_MAT4 extends IntConstant(0x8B5C)
case object GL_SAMPLER_2D extends IntConstant(0x8B5E)
case object GL_SAMPLER_CUBE extends IntConstant(0x8B60)
case object GL_VERTEX_ATTRIB_ARRAY_ENABLED extends IntConstant(0x8622)
case object GL_VERTEX_ATTRIB_ARRAY_SIZE extends IntConstant(0x8623)
case object GL_VERTEX_ATTRIB_ARRAY_STRIDE extends IntConstant(0x8624)
case object GL_VERTEX_ATTRIB_ARRAY_TYPE extends IntConstant(0x8625)
case object GL_VERTEX_ATTRIB_ARRAY_NORMALIZED extends IntConstant(0x886A)
case object GL_VERTEX_ATTRIB_ARRAY_POINTER extends IntConstant(0x8645)
case object GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING extends IntConstant(0x889F)
case object GL_IMPLEMENTATION_COLOR_READ_TYPE
    extends IntConstant(0x8B9A)
    with Parameter
case object GL_IMPLEMENTATION_COLOR_READ_FORMAT
    extends IntConstant(0x8B9B)
    with Parameter
case object GL_COMPILE_STATUS extends IntConstant(0x8B81) with ShaderParameter
case object GL_INFO_LOG_LENGTH extends IntConstant(0x8B84) with ShaderParameter
case object GL_SHADER_SOURCE_LENGTH
    extends IntConstant(0x8B88)
    with ShaderParameter
case object GL_SHADER_COMPILER extends IntConstant(0x8DFA) with Parameter
case object GL_SHADER_BINARY_FORMATS extends IntConstant(0x8DF8) with Parameter
case object GL_NUM_SHADER_BINARY_FORMATS
    extends IntConstant(0x8DF9)
    with Parameter
case object GL_LOW_FLOAT extends IntConstant(0x8DF0)
case object GL_MEDIUM_FLOAT extends IntConstant(0x8DF1)
case object GL_HIGH_FLOAT extends IntConstant(0x8DF2)
case object GL_LOW_INT extends IntConstant(0x8DF3)
case object GL_MEDIUM_INT extends IntConstant(0x8DF4)
case object GL_HIGH_INT extends IntConstant(0x8DF5)
case object GL_FRAMEBUFFER extends IntConstant(0x8D40) with FramebufferTarget
case object GL_RENDERBUFFER extends IntConstant(0x8D41)
case object GL_RGBA4
    extends IntConstant(0x8056)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RGB5_A1
    extends IntConstant(0x8057)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RGB565
    extends IntConstant(0x8D62)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_DEPTH_COMPONENT16
    extends IntConstant(0x81A5)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_STENCIL_INDEX8
    extends IntConstant(0x8D48)
    with RenderbufferInternalFormat
case object GL_RENDERBUFFER_WIDTH extends IntConstant(0x8D42)
case object GL_RENDERBUFFER_HEIGHT extends IntConstant(0x8D43)
case object GL_RENDERBUFFER_INTERNAL_FORMAT extends IntConstant(0x8D44)
case object GL_RENDERBUFFER_RED_SIZE extends IntConstant(0x8D50)
case object GL_RENDERBUFFER_GREEN_SIZE extends IntConstant(0x8D51)
case object GL_RENDERBUFFER_BLUE_SIZE extends IntConstant(0x8D52)
case object GL_RENDERBUFFER_ALPHA_SIZE extends IntConstant(0x8D53)
case object GL_RENDERBUFFER_DEPTH_SIZE extends IntConstant(0x8D54)
case object GL_RENDERBUFFER_STENCIL_SIZE extends IntConstant(0x8D55)
case object GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE extends IntConstant(0x8CD0)
case object GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME extends IntConstant(0x8CD1)
case object GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL extends IntConstant(0x8CD2)
case object GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
    extends IntConstant(0x8CD3)
case object GL_COLOR_ATTACHMENT0
    extends IntConstant(0x8CE0)
    with ColorAttachment
    with FramebufferAttachment
case object GL_DEPTH_ATTACHMENT
    extends IntConstant(0x8D00)
    with FramebufferAttachment
case object GL_STENCIL_ATTACHMENT
    extends IntConstant(0x8D20)
    with FramebufferAttachment
case object GL_NONE
    extends IntConstant(0)
    with TextureCompareMode
    with ColorOutputTarget
    with ColorBuffer
case object GL_FRAMEBUFFER_COMPLETE
    extends IntConstant(0x8CD5)
    with FramebufferStatus
case object GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
    extends IntConstant(0x8CD6)
    with FramebufferStatus
case object GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
    extends IntConstant(0x8CD7)
    with FramebufferStatus
case object GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS
    extends IntConstant(0x8CD9)
    with FramebufferStatus
case object GL_FRAMEBUFFER_UNSUPPORTED
    extends IntConstant(0x8CDD)
    with FramebufferStatus
case object GL_FRAMEBUFFER_BINDING extends IntConstant(0x8CA6)
case object GL_RENDERBUFFER_BINDING extends IntConstant(0x8CA7) with Parameter
case object GL_MAX_RENDERBUFFER_SIZE extends IntConstant(0x84E8) with Parameter
case object GL_INVALID_FRAMEBUFFER_OPERATION extends IntConstant(0x0506)
case object GL_READ_BUFFER extends IntConstant(0x0C02) with Parameter
case object GL_UNPACK_ROW_LENGTH
    extends IntConstant(0x0CF2)
    with PixelStoreParameter
case object GL_UNPACK_SKIP_ROWS
    extends IntConstant(0x0CF3)
    with PixelStoreParameter
case object GL_UNPACK_SKIP_PIXELS
    extends IntConstant(0x0CF4)
    with PixelStoreParameter
case object GL_PACK_ROW_LENGTH
    extends IntConstant(0x0D02)
    with PixelStoreParameter
case object GL_PACK_SKIP_ROWS
    extends IntConstant(0x0D03)
    with PixelStoreParameter
case object GL_PACK_SKIP_PIXELS
    extends IntConstant(0x0D04)
    with PixelStoreParameter
case object GL_COLOR extends IntConstant(0x1800) with Channel
case object GL_DEPTH extends IntConstant(0x1801) with Channel
case object GL_STENCIL extends IntConstant(0x1802) with Channel
case object GL_RED
    extends IntConstant(0x1903)
    with TextureSwizzle
    with TextureFormat
case object GL_RGB8
    extends IntConstant(0x8051)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RGBA8
    extends IntConstant(0x8058)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RGB10_A2
    extends IntConstant(0x8059)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_TEXTURE_BINDING_3D extends IntConstant(0x806A) with Parameter
case object GL_UNPACK_SKIP_IMAGES
    extends IntConstant(0x806D)
    with PixelStoreParameter
case object GL_UNPACK_IMAGE_HEIGHT
    extends IntConstant(0x806E)
    with PixelStoreParameter
case object GL_TEXTURE_3D extends IntConstant(0x806F) with TextureTarget
case object GL_TEXTURE_WRAP_R
    extends IntConstant(0x8072)
    with TextureParameter
    with SamplerParameter
case object GL_MAX_3D_TEXTURE_SIZE extends IntConstant(0x8073) with Parameter
case object GL_UNSIGNED_INT_2_10_10_10_REV
    extends IntConstant(0x8368)
    with VertexAttribType
    with TexturePixelType
case object GL_MAX_ELEMENTS_VERTICES extends IntConstant(0x80E8) with Parameter
case object GL_MAX_ELEMENTS_INDICES extends IntConstant(0x80E9) with Parameter
case object GL_TEXTURE_MIN_LOD
    extends IntConstant(0x813A)
    with TextureParameter
    with SamplerParameter
case object GL_TEXTURE_MAX_LOD
    extends IntConstant(0x813B)
    with TextureParameter
    with SamplerParameter
case object GL_TEXTURE_BASE_LEVEL
    extends IntConstant(0x813C)
    with TextureParameter
case object GL_TEXTURE_MAX_LEVEL
    extends IntConstant(0x813D)
    with TextureParameter
case object GL_MIN extends IntConstant(0x8007) with BlendMode
case object GL_MAX extends IntConstant(0x8008) with BlendMode
case object GL_DEPTH_COMPONENT24
    extends IntConstant(0x81A6)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_MAX_TEXTURE_LOD_BIAS extends IntConstant(0x84FD) with Parameter
case object GL_TEXTURE_COMPARE_MODE
    extends IntConstant(0x884C)
    with TextureParameter
    with SamplerParameter
case object GL_TEXTURE_COMPARE_FUNC
    extends IntConstant(0x884D)
    with TextureParameter
    with SamplerParameter
case object GL_CURRENT_QUERY extends IntConstant(0x8865)
case object GL_QUERY_RESULT extends IntConstant(0x8866)
case object GL_QUERY_RESULT_AVAILABLE extends IntConstant(0x8867)
case object GL_BUFFER_MAPPED extends IntConstant(0x88BC)
case object GL_BUFFER_MAP_POINTER extends IntConstant(0x88BD)
case object GL_STREAM_READ extends IntConstant(0x88E1)
case object GL_STREAM_COPY extends IntConstant(0x88E2)
case object GL_STATIC_READ extends IntConstant(0x88E5)
case object GL_STATIC_COPY extends IntConstant(0x88E6)
case object GL_DYNAMIC_READ extends IntConstant(0x88E9) with BufferUsage
case object GL_DYNAMIC_COPY extends IntConstant(0x88EA) with BufferUsage
case object GL_MAX_DRAW_BUFFERS extends IntConstant(0x8824) with Parameter
case object GL_DRAW_BUFFER0 extends IntConstant(0x8825) with DrawBuffer
case object GL_DRAW_BUFFER1 extends IntConstant(0x8826) with DrawBuffer
case object GL_DRAW_BUFFER2 extends IntConstant(0x8827) with DrawBuffer
case object GL_DRAW_BUFFER3 extends IntConstant(0x8828) with DrawBuffer
case object GL_DRAW_BUFFER4 extends IntConstant(0x8829) with DrawBuffer
case object GL_DRAW_BUFFER5 extends IntConstant(0x882A) with DrawBuffer
case object GL_DRAW_BUFFER6 extends IntConstant(0x882B) with DrawBuffer
case object GL_DRAW_BUFFER7 extends IntConstant(0x882C) with DrawBuffer
case object GL_DRAW_BUFFER8 extends IntConstant(0x882D) with DrawBuffer
case object GL_DRAW_BUFFER9 extends IntConstant(0x882E) with DrawBuffer
case object GL_DRAW_BUFFER10 extends IntConstant(0x882F) with DrawBuffer
case object GL_DRAW_BUFFER11 extends IntConstant(0x8830) with DrawBuffer
case object GL_DRAW_BUFFER12 extends IntConstant(0x8831) with DrawBuffer
case object GL_DRAW_BUFFER13 extends IntConstant(0x8832) with DrawBuffer
case object GL_DRAW_BUFFER14 extends IntConstant(0x8833) with DrawBuffer
case object GL_DRAW_BUFFER15 extends IntConstant(0x8834) with DrawBuffer
case object GL_MAX_FRAGMENT_UNIFORM_COMPONENTS
    extends IntConstant(0x8B49)
    with Parameter
case object GL_MAX_VERTEX_UNIFORM_COMPONENTS
    extends IntConstant(0x8B4A)
    with Parameter
case object GL_SAMPLER_3D extends IntConstant(0x8B5F)
case object GL_SAMPLER_2D_SHADOW extends IntConstant(0x8B62)
case object GL_FRAGMENT_SHADER_DERIVATIVE_HINT
    extends IntConstant(0x8B8B)
    with Parameter
case object GL_PIXEL_PACK_BUFFER extends IntConstant(0x88EB) with BufferTarget
case object GL_PIXEL_UNPACK_BUFFER
    extends IntConstant(0x88EC)
    with BufferTarget
case object GL_PIXEL_PACK_BUFFER_BINDING
    extends IntConstant(0x88ED)
    with Parameter
case object GL_PIXEL_UNPACK_BUFFER_BINDING
    extends IntConstant(0x88EF)
    with Parameter
case object GL_FLOAT_MAT2x3 extends IntConstant(0x8B65)
case object GL_FLOAT_MAT2x4 extends IntConstant(0x8B66)
case object GL_FLOAT_MAT3x2 extends IntConstant(0x8B67)
case object GL_FLOAT_MAT3x4 extends IntConstant(0x8B68)
case object GL_FLOAT_MAT4x2 extends IntConstant(0x8B69)
case object GL_FLOAT_MAT4x3 extends IntConstant(0x8B6A)
case object GL_SRGB extends IntConstant(0x8C40) with TextureSizedInternalFormat
case object GL_SRGB8 extends IntConstant(0x8C41)
case object GL_SRGB8_ALPHA8
    extends IntConstant(0x8C43)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_COMPARE_REF_TO_TEXTURE
    extends IntConstant(0x884E)
    with TextureCompareMode
case object GL_MAJOR_VERSION extends IntConstant(0x821B) with Parameter
case object GL_MINOR_VERSION extends IntConstant(0x821C) with Parameter
case object GL_NUM_EXTENSIONS extends IntConstant(0x821D) with Parameter
case object GL_RGBA32F
    extends IntConstant(0x8814)
    with TextureSizedInternalFormat
case object GL_RGB32F
    extends IntConstant(0x8815)
    with TextureSizedInternalFormat
case object GL_RGBA16F
    extends IntConstant(0x881A)
    with TextureSizedInternalFormat
case object GL_RGB16F
    extends IntConstant(0x881B)
    with TextureSizedInternalFormat
case object GL_VERTEX_ATTRIB_ARRAY_INTEGER extends IntConstant(0x88FD)
case object GL_MAX_ARRAY_TEXTURE_LAYERS
    extends IntConstant(0x88FF)
    with Parameter
case object GL_MIN_PROGRAM_TEXEL_OFFSET
    extends IntConstant(0x8904)
    with Parameter
case object GL_MAX_PROGRAM_TEXEL_OFFSET
    extends IntConstant(0x8905)
    with Parameter
case object GL_MAX_VARYING_COMPONENTS
    extends IntConstant(0x8B4B)
    with Parameter
case object GL_TEXTURE_2D_ARRAY extends IntConstant(0x8C1A) with TextureTarget
case object GL_TEXTURE_BINDING_2D_ARRAY
    extends IntConstant(0x8C1D)
    with Parameter
case object GL_R11F_G11F_B10F
    extends IntConstant(0x8C3A)
    with TextureSizedInternalFormat
case object GL_UNSIGNED_INT_10F_11F_11F_REV
    extends IntConstant(0x8C3B)
    with TexturePixelType
case object GL_RGB9_E5
    extends IntConstant(0x8C3D)
    with TextureSizedInternalFormat
case object GL_UNSIGNED_INT_5_9_9_9_REV
    extends IntConstant(0x8C3E)
    with TexturePixelType
case object GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH
    extends IntConstant(0x8C76)
case object GL_TRANSFORM_FEEDBACK_BUFFER_MODE extends IntConstant(0x8C7F)
case object GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS
    extends IntConstant(0x8C80)
    with Parameter
case object GL_TRANSFORM_FEEDBACK_VARYINGS extends IntConstant(0x8C83)
case object GL_TRANSFORM_FEEDBACK_BUFFER_START
    extends IntConstant(0x8C84)
    with Parameter
case object GL_TRANSFORM_FEEDBACK_BUFFER_SIZE
    extends IntConstant(0x8C85)
    with Parameter
case object GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN
    extends IntConstant(0x8C88)
case object GL_RASTERIZER_DISCARD extends IntConstant(0x8C89) with Capability
case object GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS
    extends IntConstant(0x8C8A)
    with Parameter
case object GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS
    extends IntConstant(0x8C8B)
    with Parameter
case object GL_INTERLEAVED_ATTRIBS extends IntConstant(0x8C8C)
case object GL_SEPARATE_ATTRIBS extends IntConstant(0x8C8D)
case object GL_TRANSFORM_FEEDBACK_BUFFER
    extends IntConstant(0x8C8E)
    with BufferTarget
case object GL_TRANSFORM_FEEDBACK_BUFFER_BINDING
    extends IntConstant(0x8C8F)
    with Parameter
case object GL_RGBA32UI
    extends IntConstant(0x8D70)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RGB32UI
    extends IntConstant(0x8D71)
    with TextureSizedInternalFormat
case object GL_RGBA16UI
    extends IntConstant(0x8D76)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RGB16UI
    extends IntConstant(0x8D77)
    with TextureSizedInternalFormat
case object GL_RGBA8UI
    extends IntConstant(0x8D7C)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RGB8UI
    extends IntConstant(0x8D7D)
    with TextureSizedInternalFormat
case object GL_RGBA32I
    extends IntConstant(0x8D82)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RGB32I
    extends IntConstant(0x8D83)
    with TextureSizedInternalFormat
case object GL_RGBA16I
    extends IntConstant(0x8D88)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RGB16I
    extends IntConstant(0x8D89)
    with TextureSizedInternalFormat
case object GL_RGBA8I
    extends IntConstant(0x8D8E)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RGB8I
    extends IntConstant(0x8D8F)
    with TextureSizedInternalFormat
case object GL_RED_INTEGER extends IntConstant(0x8D94) with TextureFormat
case object GL_RGB_INTEGER extends IntConstant(0x8D98) with TextureFormat
case object GL_RGBA_INTEGER extends IntConstant(0x8D99) with TextureFormat
case object GL_SAMPLER_2D_ARRAY extends IntConstant(0x8DC1)
case object GL_SAMPLER_2D_ARRAY_SHADOW extends IntConstant(0x8DC4)
case object GL_SAMPLER_CUBE_SHADOW extends IntConstant(0x8DC5)
case object GL_UNSIGNED_INT_VEC2 extends IntConstant(0x8DC6)
case object GL_UNSIGNED_INT_VEC3 extends IntConstant(0x8DC7)
case object GL_UNSIGNED_INT_VEC4 extends IntConstant(0x8DC8)
case object GL_INT_SAMPLER_2D extends IntConstant(0x8DCA)
case object GL_INT_SAMPLER_3D extends IntConstant(0x8DCB)
case object GL_INT_SAMPLER_CUBE extends IntConstant(0x8DCC)
case object GL_INT_SAMPLER_2D_ARRAY extends IntConstant(0x8DCF)
case object GL_UNSIGNED_INT_SAMPLER_2D extends IntConstant(0x8DD2)
case object GL_UNSIGNED_INT_SAMPLER_3D extends IntConstant(0x8DD3)
case object GL_UNSIGNED_INT_SAMPLER_CUBE extends IntConstant(0x8DD4)
case object GL_UNSIGNED_INT_SAMPLER_2D_ARRAY extends IntConstant(0x8DD7)
case object GL_BUFFER_ACCESS_FLAGS
    extends IntConstant(0x911F)
    with BufferParameter
case object GL_BUFFER_MAP_LENGTH
    extends IntConstant(0x9120)
    with BufferParameter
case object GL_BUFFER_MAP_OFFSET
    extends IntConstant(0x9121)
    with BufferParameter
case object GL_DEPTH_COMPONENT32F
    extends IntConstant(0x8CAC)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_DEPTH32F_STENCIL8
    extends IntConstant(0x8CAD)
    with TextureSizedInternalFormat
case object GL_FLOAT_32_UNSIGNED_INT_24_8_REV
    extends IntConstant(0x8DAD)
    with TextureInternalFormat
case object GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING
    extends IntConstant(0x8210)
case object GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE
    extends IntConstant(0x8211)
case object GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE extends IntConstant(0x8212)
case object GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE extends IntConstant(0x8213)
case object GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE extends IntConstant(0x8214)
case object GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE extends IntConstant(0x8215)
case object GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE extends IntConstant(0x8216)
case object GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE extends IntConstant(0x8217)
case object GL_FRAMEBUFFER_DEFAULT extends IntConstant(0x8218)
case object GL_FRAMEBUFFER_UNDEFINED
    extends IntConstant(0x8219)
    with FramebufferStatus
case object GL_DEPTH_STENCIL_ATTACHMENT
    extends IntConstant(0x821A)
    with FramebufferAttachment
case object GL_DEPTH_STENCIL
    extends IntConstant(0x84F9)
    with TextureFormat
    with Channel
case object GL_UNSIGNED_INT_24_8
    extends IntConstant(0x84FA)
    with TextureInternalFormat
case object GL_DEPTH24_STENCIL8
    extends IntConstant(0x88F0)
    with RenderbufferInternalFormat
    with TextureInternalFormat
case object GL_UNSIGNED_NORMALIZED extends IntConstant(0x8C17) with Parameter
case object GL_DRAW_FRAMEBUFFER_BINDING extends IntConstant(0x8CA6)
case object GL_READ_FRAMEBUFFER
    extends IntConstant(0x8CA8)
    with FramebufferTarget
case object GL_DRAW_FRAMEBUFFER
    extends IntConstant(0x8CA9)
    with FramebufferTarget
case object GL_READ_FRAMEBUFFER_BINDING
    extends IntConstant(0x8CAA)
    with Parameter
case object GL_RENDERBUFFER_SAMPLES extends IntConstant(0x8CAB)
case object GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER extends IntConstant(0x8CD4)
case object GL_MAX_COLOR_ATTACHMENTS extends IntConstant(0x8CDF) with Parameter
case object GL_COLOR_ATTACHMENT1
    extends IntConstant(0x8CE1)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT2
    extends IntConstant(0x8CE2)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT3
    extends IntConstant(0x8CE3)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT4
    extends IntConstant(0x8CE4)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT5
    extends IntConstant(0x8CE5)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT6
    extends IntConstant(0x8CE6)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT7
    extends IntConstant(0x8CE7)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT8
    extends IntConstant(0x8CE8)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT9
    extends IntConstant(0x8CE9)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT10
    extends IntConstant(0x8CEA)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT11
    extends IntConstant(0x8CEB)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT12
    extends IntConstant(0x8CEC)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT13
    extends IntConstant(0x8CED)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT14
    extends IntConstant(0x8CEE)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT15
    extends IntConstant(0x8CEF)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT16
    extends IntConstant(0x8CF0)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT17
    extends IntConstant(0x8CF1)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT18
    extends IntConstant(0x8CF2)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT19
    extends IntConstant(0x8CF3)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT20
    extends IntConstant(0x8CF4)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT21
    extends IntConstant(0x8CF5)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT22
    extends IntConstant(0x8CF6)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT23
    extends IntConstant(0x8CF7)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT24
    extends IntConstant(0x8CF8)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT25
    extends IntConstant(0x8CF9)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT26
    extends IntConstant(0x8CFA)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT27
    extends IntConstant(0x8CFB)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT28
    extends IntConstant(0x8CFC)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT29
    extends IntConstant(0x8CFD)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT30
    extends IntConstant(0x8CFE)
    with ColorAttachment
    with FramebufferAttachment
case object GL_COLOR_ATTACHMENT31
    extends IntConstant(0x8CFF)
    with ColorAttachment
    with FramebufferAttachment
case object GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE
    extends IntConstant(0x8D56)
    with FramebufferStatus
case object GL_MAX_SAMPLES extends IntConstant(0x8D57) with Parameter
case object GL_HALF_FLOAT
    extends IntConstant(0x140B)
    with VertexAttribType
    with TexturePixelType
case object GL_MAP_READ_BIT extends IntConstant(0x0001)
case object GL_MAP_WRITE_BIT extends IntConstant(0x0002)
case object GL_MAP_INVALIDATE_RANGE_BIT extends IntConstant(0x0004)
case object GL_MAP_INVALIDATE_BUFFER_BIT extends IntConstant(0x0008)
case object GL_MAP_FLUSH_EXPLICIT_BIT extends IntConstant(0x0010)
case object GL_MAP_UNSYNCHRONIZED_BIT extends IntConstant(0x0020)
case object GL_RG extends IntConstant(0x8227) with TextureFormat
case object GL_RG_INTEGER extends IntConstant(0x8228) with TextureFormat
case object GL_R8
    extends IntConstant(0x8229)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RG8
    extends IntConstant(0x822B)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_R16F extends IntConstant(0x822D) with TextureSizedInternalFormat
case object GL_R32F extends IntConstant(0x822E) with TextureSizedInternalFormat
case object GL_RG16F
    extends IntConstant(0x822F)
    with TextureSizedInternalFormat
case object GL_RG32F
    extends IntConstant(0x8230)
    with TextureSizedInternalFormat
case object GL_R8I
    extends IntConstant(0x8231)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_R8UI
    extends IntConstant(0x8232)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_R16I
    extends IntConstant(0x8233)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_R16UI
    extends IntConstant(0x8234)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_R32I extends IntConstant(0x8235) with RenderbufferInternalFormat
case object GL_R32UI
    extends IntConstant(0x8236)
    with RenderbufferInternalFormat
case object GL_RG8I
    extends IntConstant(0x8237)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RG8UI
    extends IntConstant(0x8238)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RG16I
    extends IntConstant(0x8239)
    with RenderbufferInternalFormat
case object GL_RG16UI
    extends IntConstant(0x823A)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RG32I
    extends IntConstant(0x823B)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_RG32UI
    extends IntConstant(0x823C)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_VERTEX_ARRAY_BINDING extends IntConstant(0x85B5) with Parameter
case object GL_R8_SNORM
    extends IntConstant(0x8F94)
    with TextureSizedInternalFormat
case object GL_RG8_SNORM
    extends IntConstant(0x8F95)
    with TextureSizedInternalFormat
case object GL_RGB8_SNORM
    extends IntConstant(0x8F96)
    with TextureSizedInternalFormat
case object GL_RGBA8_SNORM
    extends IntConstant(0x8F97)
    with TextureSizedInternalFormat
case object GL_SIGNED_NORMALIZED extends IntConstant(0x8F9C)
case object GL_PRIMITIVE_RESTART_FIXED_INDEX
    extends IntConstant(0x8D69)
    with Capability
case object GL_COPY_READ_BUFFER extends IntConstant(0x8F36) with BufferTarget
case object GL_COPY_WRITE_BUFFER extends IntConstant(0x8F37) with BufferTarget
case object GL_COPY_READ_BUFFER_BINDING
    extends IntConstant(0x8F36)
    with Parameter
case object GL_COPY_WRITE_BUFFER_BINDING
    extends IntConstant(0x8F37)
    with Parameter
case object GL_UNIFORM_BUFFER extends IntConstant(0x8A11) with BufferTarget
case object GL_UNIFORM_BUFFER_BINDING
    extends IntConstant(0x8A28)
    with Parameter
case object GL_UNIFORM_BUFFER_START extends IntConstant(0x8A29) with Parameter
case object GL_UNIFORM_BUFFER_SIZE extends IntConstant(0x8A2A) with Parameter
case object GL_MAX_VERTEX_UNIFORM_BLOCKS
    extends IntConstant(0x8A2B)
    with Parameter
case object GL_MAX_FRAGMENT_UNIFORM_BLOCKS
    extends IntConstant(0x8A2D)
    with Parameter
case object GL_MAX_COMBINED_UNIFORM_BLOCKS
    extends IntConstant(0x8A2E)
    with Parameter
case object GL_MAX_UNIFORM_BUFFER_BINDINGS
    extends IntConstant(0x8A2F)
    with Parameter
case object GL_MAX_UNIFORM_BLOCK_SIZE
    extends IntConstant(0x8A30)
    with Parameter
case object GL_MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS
    extends IntConstant(0x8A31)
    with Parameter
case object GL_MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS
    extends IntConstant(0x8A33)
    with Parameter
case object GL_UNIFORM_BUFFER_OFFSET_ALIGNMENT
    extends IntConstant(0x8A34)
    with Parameter
case object GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH extends IntConstant(0x8A35)
case object GL_ACTIVE_UNIFORM_BLOCKS extends IntConstant(0x8A36)
case object GL_UNIFORM_TYPE extends IntConstant(0x8A37)
case object GL_UNIFORM_SIZE extends IntConstant(0x8A38) with Parameter
case object GL_UNIFORM_NAME_LENGTH extends IntConstant(0x8A39)
case object GL_UNIFORM_BLOCK_INDEX extends IntConstant(0x8A3A)
case object GL_UNIFORM_OFFSET extends IntConstant(0x8A3B)
case object GL_UNIFORM_ARRAY_STRIDE extends IntConstant(0x8A3C)
case object GL_UNIFORM_MATRIX_STRIDE extends IntConstant(0x8A3D)
case object GL_UNIFORM_IS_ROW_MAJOR extends IntConstant(0x8A3E)
case object GL_UNIFORM_BLOCK_BINDING extends IntConstant(0x8A3F)
case object GL_UNIFORM_BLOCK_DATA_SIZE extends IntConstant(0x8A40)
case object GL_UNIFORM_BLOCK_NAME_LENGTH extends IntConstant(0x8A41)
case object GL_UNIFORM_BLOCK_ACTIVE_UNIFORMS extends IntConstant(0x8A42)
case object GL_UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES extends IntConstant(0x8A43)
case object GL_UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER
    extends IntConstant(0x8A44)
case object GL_UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER
    extends IntConstant(0x8A46)
case object GL_INVALID_INDEX extends IntConstant(0xFFFFFFFF)
case object GL_MAX_VERTEX_OUTPUT_COMPONENTS
    extends IntConstant(0x9122)
    with Parameter
case object GL_MAX_FRAGMENT_INPUT_COMPONENTS
    extends IntConstant(0x9125)
    with Parameter
case object GL_MAX_SERVER_WAIT_TIMEOUT
    extends IntConstant(0x9111)
    with Parameter
case object GL_OBJECT_TYPE extends IntConstant(0x9112)
case object GL_SYNC_CONDITION extends IntConstant(0x9113)
case object GL_SYNC_STATUS extends IntConstant(0x9114)
case object GL_SYNC_FLAGS extends IntConstant(0x9115)
case object GL_SYNC_FENCE extends IntConstant(0x9116)
case object GL_SYNC_GPU_COMMANDS_COMPLETE extends IntConstant(0x9117)
case object GL_UNSIGNALED extends IntConstant(0x9118)
case object GL_SIGNALED extends IntConstant(0x9119)
case object GL_ALREADY_SIGNALED extends IntConstant(0x911A)
case object GL_TIMEOUT_EXPIRED extends IntConstant(0x911B)
case object GL_CONDITION_SATISFIED extends IntConstant(0x911C)
case object GL_WAIT_FAILED extends IntConstant(0x911D)
case object GL_SYNC_FLUSH_COMMANDS_BIT extends IntConstant(0x00000001)
case object GL_TIMEOUT_IGNORED extends LongConstant(0xFFFFFFFFFFFFFFFFL)
case object GL_VERTEX_ATTRIB_ARRAY_DIVISOR extends IntConstant(0x88FE)
case object GL_ANY_SAMPLES_PASSED extends IntConstant(0x8C2F)
case object GL_ANY_SAMPLES_PASSED_CONSERVATIVE extends IntConstant(0x8D6A)
case object GL_SAMPLER_BINDING extends IntConstant(0x8919) with Parameter
case object GL_RGB10_A2UI
    extends IntConstant(0x906F)
    with RenderbufferInternalFormat
    with TextureSizedInternalFormat
case object GL_TEXTURE_SWIZZLE_R
    extends IntConstant(0x8E42)
    with TextureParameter
case object GL_TEXTURE_SWIZZLE_G
    extends IntConstant(0x8E43)
    with TextureParameter
case object GL_TEXTURE_SWIZZLE_B
    extends IntConstant(0x8E44)
    with TextureParameter
case object GL_TEXTURE_SWIZZLE_A
    extends IntConstant(0x8E45)
    with TextureParameter
case object GL_GREEN extends IntConstant(0x1904) with TextureSwizzle
case object GL_BLUE extends IntConstant(0x1905) with TextureSwizzle
case object GL_INT_2_10_10_10_REV
    extends IntConstant(0x8D9F)
    with VertexAttribType
case object GL_TRANSFORM_FEEDBACK extends IntConstant(0x8E22)
case object GL_TRANSFORM_FEEDBACK_PAUSED
    extends IntConstant(0x8E23)
    with Parameter
case object GL_TRANSFORM_FEEDBACK_ACTIVE
    extends IntConstant(0x8E24)
    with Parameter
case object GL_TRANSFORM_FEEDBACK_BINDING
    extends IntConstant(0x8E25)
    with Parameter
case object GL_PROGRAM_BINARY_RETRIEVABLE_HINT extends IntConstant(0x8257)
case object GL_PROGRAM_BINARY_LENGTH extends IntConstant(0x8741)
case object GL_NUM_PROGRAM_BINARY_FORMATS
    extends IntConstant(0x87FE)
    with Parameter
case object GL_PROGRAM_BINARY_FORMATS
    extends IntConstant(0x87FF)
    with Parameter
case object GL_COMPRESSED_R11_EAC
    extends IntConstant(0x9270)
    with TextureCompressedFormat
case object GL_COMPRESSED_SIGNED_R11_EAC
    extends IntConstant(0x9271)
    with TextureCompressedFormat
case object GL_COMPRESSED_RG11_EAC
    extends IntConstant(0x9272)
    with TextureCompressedFormat
case object GL_COMPRESSED_SIGNED_RG11_EAC
    extends IntConstant(0x9273)
    with TextureCompressedFormat
case object GL_COMPRESSED_RGB8_ETC2
    extends IntConstant(0x9274)
    with TextureCompressedFormat
case object GL_COMPRESSED_SRGB8_ETC2
    extends IntConstant(0x9275)
    with TextureCompressedFormat
case object GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2
    extends IntConstant(0x9276)
    with TextureCompressedFormat
case object GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2
    extends IntConstant(0x9277)
    with TextureCompressedFormat
case object GL_COMPRESSED_RGBA8_ETC2_EAC
    extends IntConstant(0x9278)
    with TextureCompressedFormat
case object GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC
    extends IntConstant(0x9279)
    with TextureCompressedFormat
case object GL_TEXTURE_IMMUTABLE_FORMAT extends IntConstant(0x912F)
case object GL_MAX_ELEMENT_INDEX extends IntConstant(0x8D6B) with Parameter
case object GL_NUM_SAMPLE_COUNTS extends IntConstant(0x9380)
case object GL_TEXTURE_IMMUTABLE_LEVELS extends IntConstant(0x82DF)
