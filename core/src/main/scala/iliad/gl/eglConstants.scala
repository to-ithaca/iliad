package iliad
package gl

import cats.data._
import cats.implicits._

sealed trait ConfigAttrib extends IntConstant
sealed trait ConfigAttribValue extends IntConstant

sealed trait EnumConfigAttrib extends ConfigAttrib
sealed trait IntConfigAttrib extends ConfigAttrib

sealed trait ContextAttrib extends IntConstant
sealed trait ContextAttribValue extends IntConstant

sealed trait WindowAttrib extends IntConstant
sealed trait WindowAttribValue extends IntConstant

sealed trait PBufferAttrib extends IntConstant
sealed trait PBufferAttribValue extends IntConstant

sealed trait EGLAPI extends IntConstant

sealed trait DisplayProperty extends IntConstant

sealed trait EGLError extends IntConstant

object ConfigAttrib {
  def apply(
      k: ConfigAttrib, i: Int): (ConfigAttrib, Int Xor ConfigAttribValue) =
    k -> i.left
  def apply(k: ConfigAttrib,
            v: ConfigAttribValue): (ConfigAttrib, Int Xor ConfigAttribValue) =
    k -> v.right
}

object ContextAttrib {
  def apply(
      k: ContextAttrib, i: Int): (ContextAttrib, Int Xor ContextAttribValue) =
    k -> i.left
  def apply(
      k: ContextAttrib,
      v: ContextAttribValue): (ContextAttrib, Int Xor ContextAttribValue) =
    k -> v.right
}

case object EGL_ALPHA_SIZE extends IntConstant(0x3021) with ConfigAttrib
case object EGL_BAD_ACCESS extends IntConstant(0x3002) with EGLError
case object EGL_BAD_ALLOC extends IntConstant(0x3003) with EGLError
case object EGL_BAD_ATTRIBUTE extends IntConstant(0x3004) with EGLError
case object EGL_BAD_CONFIG extends IntConstant(0x3005) with EGLError
case object EGL_BAD_CONTEXT extends IntConstant(0x3006) with EGLError
case object EGL_BAD_CURRENT_SURFACE extends IntConstant(0x3007)
case object EGL_BAD_DISPLAY extends IntConstant(0x3008) with EGLError
case object EGL_BAD_MATCH extends IntConstant(0x3009) with EGLError
case object EGL_BAD_NATIVE_PIXMAP extends IntConstant(0x300A)
case object EGL_BAD_NATIVE_WINDOW extends IntConstant(0x300B) with EGLError
case object EGL_BAD_PARAMETER extends IntConstant(0x300C) with EGLError
case object EGL_BAD_SURFACE extends IntConstant(0x300D) with EGLError
case object EGL_BLUE_SIZE extends IntConstant(0x3022) with IntConfigAttrib
case object EGL_BUFFER_SIZE extends IntConstant(0x3020) with IntConfigAttrib
case object EGL_CONFIG_CAVEAT extends IntConstant(0x3027) with EnumConfigAttrib
case object EGL_CONFIG_ID extends IntConstant(0x3028) with IntConfigAttrib
case object EGL_CORE_NATIVE_ENGINE extends IntConstant(0x305B)
case object EGL_DEPTH_SIZE extends IntConstant(0x3025) with IntConfigAttrib
case object EGL_DONT_CARE extends IntConstant(-1) with ConfigAttribValue
case object EGL_DRAW extends IntConstant(0x3059)
case object EGL_EXTENSIONS extends IntConstant(0x3055)
case object EGL_FALSE extends IntConstant(0) with ConfigAttribValue
case object EGL_GREEN_SIZE extends IntConstant(0x3023) with IntConfigAttrib
case object EGL_HEIGHT extends IntConstant(0x3056) with PBufferAttrib
case object EGL_LARGEST_PBUFFER extends IntConstant(0x3058)
case object EGL_LEVEL extends IntConstant(0x3029) with IntConfigAttrib
case object EGL_MAX_PBUFFER_HEIGHT
    extends IntConstant(0x302A)
    with IntConfigAttrib
case object EGL_MAX_PBUFFER_PIXELS
    extends IntConstant(0x302B)
    with IntConfigAttrib
case object EGL_MAX_PBUFFER_WIDTH
    extends IntConstant(0x302C)
    with IntConfigAttrib
case object EGL_NATIVE_RENDERABLE
    extends IntConstant(0x302D)
    with EnumConfigAttrib
case object EGL_NATIVE_VISUAL_ID extends IntConstant(0x302E)
case object EGL_NATIVE_VISUAL_TYPE extends IntConstant(0x302F)
case object EGL_NON_CONFORMANT_CONFIG
    extends IntConstant(0x3051)
    with ConfigAttribValue
case object EGL_NOT_INITIALIZED extends IntConstant(0x3001) with EGLError
case object EGL_PBUFFER_BIT extends IntConstant(0x0001) with ConfigAttribValue
case object EGL_PIXMAP_BIT extends IntConstant(0x0002)
case object EGL_READ extends IntConstant(0x305A)
case object EGL_RED_SIZE extends IntConstant(0x3024) with IntConfigAttrib
case object EGL_SAMPLES extends IntConstant(0x3031) with IntConfigAttrib
case object EGL_SAMPLE_BUFFERS extends IntConstant(0x3032) with IntConfigAttrib
case object EGL_SLOW_CONFIG extends IntConstant(0x3050) with ConfigAttribValue
case object EGL_STENCIL_SIZE extends IntConstant(0x3026) with IntConfigAttrib
case object EGL_SUCCESS extends IntConstant(0x3000)
case object EGL_SURFACE_TYPE extends IntConstant(0x3033) with ConfigAttrib
case object EGL_TRANSPARENT_BLUE_VALUE
    extends IntConstant(0x3035)
    with IntConfigAttrib
case object EGL_TRANSPARENT_GREEN_VALUE
    extends IntConstant(0x3036)
    with IntConfigAttrib
case object EGL_TRANSPARENT_RED_VALUE
    extends IntConstant(0x3037)
    with IntConfigAttrib
case object EGL_TRANSPARENT_RGB
    extends IntConstant(0x3052)
    with ConfigAttribValue
case object EGL_TRANSPARENT_TYPE
    extends IntConstant(0x3034)
    with EnumConfigAttrib
case object EGL_TRUE extends IntConstant(1) with ConfigAttribValue
case object EGL_VENDOR extends IntConstant(0x3053) with DisplayProperty
case object EGL_VERSION extends IntConstant(0x3054) with DisplayProperty
case object EGL_WIDTH extends IntConstant(0x3057) with PBufferAttrib
case object EGL_WINDOW_BIT extends IntConstant(0x0004) with ConfigAttribValue

/** EGL 11 */
case object EGL_BACK_BUFFER extends IntConstant(0x3084)
case object EGL_BIND_TO_TEXTURE_RGB
    extends IntConstant(0x3039)
    with EnumConfigAttrib
case object EGL_BIND_TO_TEXTURE_RGBA
    extends IntConstant(0x303A)
    with EnumConfigAttrib
case object EGL_CONTEXT_LOST extends IntConstant(0x300E)
case object EGL_MIN_SWAP_INTERVAL
    extends IntConstant(0x303B)
    with IntConfigAttrib
case object EGL_MAX_SWAP_INTERVAL
    extends IntConstant(0x303C)
    with IntConfigAttrib
case object EGL_MIPMAP_TEXTURE extends IntConstant(0x3082)
case object EGL_MIPMAP_LEVEL extends IntConstant(0x3083)
case object EGL_NO_TEXTURE extends IntConstant(0x305C)
case object EGL_TEXTURE_2D extends IntConstant(0x305F) with PBufferAttribValue
case object EGL_TEXTURE_FORMAT extends IntConstant(0x3080) with PBufferAttrib
case object EGL_TEXTURE_RGB extends IntConstant(0x305D)
case object EGL_TEXTURE_RGBA
    extends IntConstant(0x305E)
    with PBufferAttribValue
case object EGL_TEXTURE_TARGET extends IntConstant(0x3081) with PBufferAttrib

/** EGL 12 */
case object EGL_ALPHA_FORMAT extends IntConstant(0x3088)
case object EGL_ALPHA_FORMAT_NONPRE extends IntConstant(0x308B)
case object EGL_ALPHA_FORMAT_PRE extends IntConstant(0x308C)
case object EGL_ALPHA_MASK_SIZE
    extends IntConstant(0x303E)
    with IntConfigAttrib
case object EGL_BUFFER_PRESERVED extends IntConstant(0x3094)
case object EGL_BUFFER_DESTROYED extends IntConstant(0x3095)
case object EGL_CLIENT_APIS extends IntConstant(0x308D) with DisplayProperty
case object EGL_COLORSPACE extends IntConstant(0x3087)
case object EGL_COLORSPACE_sRGB extends IntConstant(0x3089)
case object EGL_COLORSPACE_LINEAR extends IntConstant(0x308A)
case object EGL_COLOR_BUFFER_TYPE
    extends IntConstant(0x303F)
    with ConfigAttrib
    with EnumConfigAttrib
case object EGL_CONTEXT_CLIENT_TYPE extends IntConstant(0x3097)
case object EGL_DISPLAY_SCALING extends IntConstant(10000)
case object EGL_HORIZONTAL_RESOLUTION extends IntConstant(0x3090)
case object EGL_LUMINANCE_BUFFER
    extends IntConstant(0x308F)
    with ConfigAttribValue
case object EGL_LUMINANCE_SIZE extends IntConstant(0x303D) with IntConfigAttrib
case object EGL_OPENGL_ES_BIT extends IntConstant(0x0001)
case object EGL_OPENVG_BIT extends IntConstant(0x0002)
case object EGL_OPENGL_ES_API extends IntConstant(0x30A0) with EGLAPI
case object EGL_OPENVG_API extends IntConstant(0x30A1) with EGLAPI
case object EGL_OPENVG_IMAGE extends IntConstant(0x3096)
case object EGL_PIXEL_ASPECT_RATIO extends IntConstant(0x3092)
case object EGL_RENDERABLE_TYPE extends IntConstant(0x3040) with IntConfigAttrib //TODO: Add bitmask support
case object EGL_RENDER_BUFFER extends IntConstant(0x3086)
case object EGL_RGB_BUFFER extends IntConstant(0x308E) with ConfigAttribValue
case object EGL_SINGLE_BUFFER extends IntConstant(0x3085)
case object EGL_SWAP_BEHAVIOR extends IntConstant(0x3093)
case object EGL_UNKNOWN extends IntConstant(-1)
case object EGL_VERTICAL_RESOLUTION extends IntConstant(0x3091)

/** EGL 13 */
case object EGL_CONFORMANT extends IntConstant(0x3042) with ConfigAttrib //CHECK
case object EGL_CONTEXT_CLIENT_VERSION
    extends IntConstant(0x3098)
    with ContextAttrib //CHECK
case object EGL_MATCH_NATIVE_PIXMAP
    extends IntConstant(0x3041)
    with ConfigAttrib //CHECK
case object EGL_OPENGL_ES2_BIT extends IntConstant(0x0004)
case object EGL_VG_ALPHA_FORMAT extends IntConstant(0x3088)
case object EGL_VG_ALPHA_FORMAT_NONPRE extends IntConstant(0x308B)
case object EGL_VG_ALPHA_FORMAT_PRE extends IntConstant(0x308C)
case object EGL_VG_ALPHA_FORMAT_PRE_BIT extends IntConstant(0x0040)
case object EGL_VG_COLORSPACE extends IntConstant(0x3087)
case object EGL_VG_COLORSPACE_sRGB extends IntConstant(0x3089)
case object EGL_VG_COLORSPACE_LINEAR extends IntConstant(0x308A)
case object EGL_VG_COLORSPACE_LINEAR_BIT extends IntConstant(0x0020)

/** EGL 14 */
case object EGL_MULTISAMPLE_RESOLVE_BOX_BIT extends IntConstant(0x0200)
case object EGL_MULTISAMPLE_RESOLVE extends IntConstant(0x3099)
case object EGL_MULTISAMPLE_RESOLVE_DEFAULT extends IntConstant(0x309A)
case object EGL_MULTISAMPLE_RESOLVE_BOX extends IntConstant(0x309B)
case object EGL_OPENGL_API extends IntConstant(0x30A2) with EGLAPI
case object EGL_OPENGL_BIT extends IntConstant(0x0008)
case object EGL_SWAP_BEHAVIOR_PRESERVED_BIT extends IntConstant(0x0400)

case object EGL_CONTEXT_MAJOR_VERSION extends IntConstant(0x3098)
case object EGL_CONTEXT_MINOR_VERSION extends IntConstant(0x30FB)
case object EGL_CONTEXT_OPENGL_PROFILE_MASK extends IntConstant(0x30FD)
case object EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY
    extends IntConstant(0x31BD)
case object EGL_NO_RESET_NOTIFICATION extends IntConstant(0x31BE)
case object EGL_LOSE_CONTEXT_ON_RESET extends IntConstant(0x31BF)
case object EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT extends IntConstant(0x00000001)
case object EGL_CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT
    extends IntConstant(0x00000002)
case object EGL_CONTEXT_OPENGL_DEBUG extends IntConstant(0x31B0)
case object EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE extends IntConstant(0x31B1)
case object EGL_CONTEXT_OPENGL_ROBUST_ACCESS extends IntConstant(0x31B2)
case object EGL_OPENGL_ES3_BIT
    extends IntConstant(0x00000040)
    with ConfigAttribValue
case object EGL_CL_EVENT_HANDLE extends IntConstant(0x309C)
case object EGL_SYNC_CL_EVENT extends IntConstant(0x30FE)
case object EGL_SYNC_CL_EVENT_COMPLETE extends IntConstant(0x30FF)
case object EGL_SYNC_PRIOR_COMMANDS_COMPLETE extends IntConstant(0x30F0)
case object EGL_SYNC_TYPE extends IntConstant(0x30F7)
case object EGL_SYNC_STATUS extends IntConstant(0x30F1)
case object EGL_SYNC_CONDITION extends IntConstant(0x30F8)
case object EGL_SIGNALED extends IntConstant(0x30F2)
case object EGL_UNSIGNALED extends IntConstant(0x30F3)
case object EGL_SYNC_FLUSH_COMMANDS_BIT extends IntConstant(0x0001)
case object EGL_FOREVER extends IntConstant(-1)
case object EGL_TIMEOUT_EXPIRED extends IntConstant(0x30F5)
case object EGL_CONDITION_SATISFIED extends IntConstant(0x30F6)
case object EGL_NO_SYNC extends IntConstant(0)
case object EGL_SYNC_FENCE extends IntConstant(0x30F9)
case object EGL_GL_COLORSPACE extends IntConstant(0x309D)
case object EGL_GL_COLORSPACE_SRGB extends IntConstant(0x3089)
case object EGL_GL_COLORSPACE_LINEAR extends IntConstant(0x308A)
case object EGL_GL_RENDERBUFFER extends IntConstant(0x30B9)
case object EGL_GL_TEXTURE_2D extends IntConstant(0x30B1)
case object EGL_GL_TEXTURE_LEVEL extends IntConstant(0x30BC)
case object EGL_GL_TEXTURE_3D extends IntConstant(0x30B2)
case object EGL_GL_TEXTURE_ZOFFSET extends IntConstant(0x30BD)
case object EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X extends IntConstant(0x30B3)
case object EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X extends IntConstant(0x30B4)
case object EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y extends IntConstant(0x30B5)
case object EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y extends IntConstant(0x30B6)
case object EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z extends IntConstant(0x30B7)
case object EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z extends IntConstant(0x30B8)
case object EGL_IMAGE_PRESERVED extends IntConstant(0x30D2)
case object EGL_NO_IMAGE extends IntConstant(0)
case object EGL_NONE extends IntConstant(0x3038) with ConfigAttribValue
