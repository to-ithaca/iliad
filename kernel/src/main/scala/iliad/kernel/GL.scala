package iliad
package kernel

import iliad.kernel.vectord._
import iliad.kernel.matrixd._
import iliad.kernel.platform._

import java.nio._

import cats._
import cats.data._
import cats.implicits._
 
import GL._
import GLConstants._

/**
  Typesafe GL. The type parameter `F` has type constraints to compose operations.
 */
abstract class GL[F[_]: Monad] {

  //Semigroup and Monad are defined for |+| and >>= operations
  val S: Semigroup[F[Unit]]

  def blitFramebuffer(src: Rect[Int], dest: Rect[Int], bitMask: ChannelBitMask, filter: BlitFilter): IO[F, Unit]
  def viewport(rect: Rect[Int]): IO[F, Unit]
  def flush: IO[F, Unit]
  def clear(bitMask: ChannelBitMask): IO[F, Unit]
  def clearColor(red: Float, green: Float, blue: Float, alpha: Float): IO[F, Unit]
  def enable(cap: Capability): IO[F, Unit]
  def disable(cap: Capability): IO[F, Unit]
  def getError: IO[F, Option[Int Xor ErrorCode]]
  def createShader(`type`: ShaderType): IO[F, Int]
  def shaderSource(shader: Int, count: Int, sources: Seq[String]): IO[F, Unit]
  def shaderSource(shader: Int, source: String): IO[F, Unit] = shaderSource(shader, 0, Seq(source))
  def deleteShader(shader: Int): IO[F, Unit]
  def compileShader(shader: Int): IO[F, Unit]
  def attachShader(program: Int, shader: Int): IO[F, Unit]
  private[kernel] def getShaderiv(shader: Int, pname: ShaderParameter): IO[F, Int]
  
  private def getShaderiv[A](shader: Int, pname: ShaderParameter, expected: Set[A])(eq: A => Int): IO[F, A] = getShaderiv(shader, pname).map(code => expected.find(eq(_) == code).get)

  def getShaderiv(shader: Int, pname: GL_SHADER_TYPE.type): IO[F, ShaderType] = getShaderiv(shader, pname, SealedEnum.values[ShaderType])(_.value)
  def getShaderiv(shader: Int, pname: GL_DELETE_STATUS.type): IO[F, Boolean] = getShaderiv(shader, pname, SealedEnum.values[TrueFalse])(_.value).map(_ == GL_TRUE)
  def getShaderiv(shader: Int, pname: GL_COMPILE_STATUS.type): IO[F, Boolean] = getShaderiv(shader, pname, SealedEnum.values[TrueFalse])(_.value).map(_ == GL_TRUE)
  def getShaderiv(shader: Int, pname: GL_INFO_LOG_LENGTH.type): IO[F, Int] = getShaderiv(shader, pname: ShaderParameter)
  def getShaderiv(shader: Int, pname: GL_SHADER_SOURCE_LENGTH.type): IO[F, Int] = getShaderiv(shader, pname: ShaderParameter)

  private[kernel] def getShaderInfoLog(shader: Int, maxLength: Int): IO[F, String]
  def getShaderInfoLog(shader: Int): IO[F, String] = getShaderiv(shader, GL_SHADER_SOURCE_LENGTH) >>= (getShaderInfoLog(shader, _))

  def createProgram: IO[F, Int]
  def useProgram(program: Int): IO[F, Unit]
  def linkProgram(program: Int): IO[F, Unit]


  private def _tup1[A](arr: Array[A]): A = arr(0)
  private def _tup2[A](arr: Array[A]): (A, A) = (arr(0), arr(1))
  private def _tup3[A](arr: Array[A]): (A, A, A) = (arr(0), arr(1), arr(2))

  //TODO: Work out how best to expose N buffers safely
  private[kernel] def genBuffers(num: Int): IO[F, Array[Int]]
  def genBuffer: IO[F, Int] = genBuffers(1) map _tup1
  def genBuffer2: IO[F, (Int, Int)] = genBuffers(2) map _tup2
  def genBuffer3: IO[F, (Int, Int, Int)] = genBuffers(3) map _tup3 
  def bindBuffer(target: BufferTarget, buffer: Int): IO[F , Unit]
  def bufferData(target: BufferTarget, size: Int, data: Buffer, usage: BufferUsage): IO[F, Unit]
  def bufferSubData(target: BufferTarget, offset: Int, size: Int, data: Buffer): IO[F, Unit]
  def enableVertexAttribArray(location: Int): IO[F, Unit]
  def vertexAttribPointer(location: Int, size: Int,`type`: VertexAttribType, normalized: Boolean, stride: Int, offset: Int): IO[F, Unit]
  private[kernel] def genFramebuffers(num: Int): IO[F,Array[Int]]
  def genFramebuffer: IO[F, Int] = genFramebuffers(1) map _tup1
  def genFramebuffer2(num: Int): IO[F, (Int, Int)] = genFramebuffers(2) map _tup2
  def genFramebuffer3(num: Int): IO[F, (Int, Int, Int)] = genFramebuffers(3) map _tup3
  def bindFramebuffer(target: FramebufferTarget, framebuffer: Int): IO[F, Unit]
  def framebufferRenderbuffer(target: FramebufferTarget, attachment: FramebufferAttachment, renderbuffer: Int): IO[F, Unit]
  def checkFramebufferStatus(target: FramebufferTarget): IO[F, FramebufferStatus]
  def framebufferTexture2D(target: FramebufferTarget, attachment: FramebufferAttachment, texTarget: FramebufferTexTarget, texture: Int, level: Int): IO[F, Unit]

  private[kernel] def genRenderbuffers(num: Int): IO[F, Array[Int]]
  def genRenderbuffer: IO[F, Int] = genRenderbuffers(1) map _tup1
  def genRenderbuffer2: IO[F, (Int, Int)] = genRenderbuffers(2) map _tup2
  def genRenderbuffer3: IO[F, (Int, Int, Int)] = genRenderbuffers(3) map _tup3
  def bindRenderbuffer(renderbuffer: Int): IO[F, Unit]
  def renderbufferStorage(format: RenderbufferInternalFormat, width: Int, height: Int): IO[F, Unit]
  def bindTexture(target: TextureTarget, texture: Int): IO[F, Unit]

  private[kernel] def genTextures(num: Int): IO[F, Array[Int]]
  def genTexture: IO[F, Int] = genTextures(1) map _tup1
  def genTexture2: IO[F, (Int, Int)] = genTextures(2) map _tup2
  def genTexture3: IO[F, (Int, Int, Int)] = genTextures(3) map _tup3

  private[kernel] def texParameteri(target: TextureTarget, name: TextureParameter, value: Int): IO[F, Unit]
  private[kernel] def texParameteri(target: TextureTarget, name: TextureParameter, value: IntConstant): IO[F, Unit]
  def texParameter(target: TextureTarget, name: GL_TEXTURE_BASE_LEVEL.type, value: Int): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_COMPARE_FUNC.type, value: TextureCompareFunc): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_COMPARE_MODE.type, value: TextureCompareMode): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_MIN_FILTER.type, value: TextureMinFilter): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_MAG_FILTER.type, value: TextureMagFilter): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_MIN_LOD.type, value: Int): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_MAX_LOD.type, value: Int): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_MAX_LEVEL.type, value: Int): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_SWIZZLE_R.type, value: TextureSwizzle): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_SWIZZLE_G.type, value: TextureSwizzle): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_SWIZZLE_B.type, value: TextureSwizzle): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_SWIZZLE_A.type, value: TextureSwizzle): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_WRAP_S.type, value: TextureWrap): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_WRAP_T.type, value: TextureWrap): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_WRAP_R.type, value: TextureWrap): IO[F, Unit] = texParameteri(target, name, value)

  def texImage2D(target: TextureTarget, level: Int, internalFormat: TextureInteralFormat, width: Int, height: Int, format: TextureFormat, `type`: TexturePixelType, data: Buffer): IO[F, Unit]

  def pixelStorei(name: PixelStoreParameter, value: Int): IO[F, Unit]

  def activeTexture(texture: Int): IO[F, Unit]
  def drawArrays(mode: PrimitiveType, first: Int, count: Int): IO[F, Unit]
  def drawElements(mode: PrimitiveType, count: Int, `type`: IndexType, offset: Int): IO[F, Unit]
  def drawElements(mode: PrimitiveType, count: Int, `type`: IndexType, indices: Buffer): IO[F, Unit]

  private[kernel] def uniform1i(location: Int, arg0: Int): IO[F, Unit]
  private[kernel] def uniform1f(location: Int, arg0: Float): IO[F, Unit]
  private[kernel] def uniform1fv(location: Int, count: Int, arr: Array[Float]): IO[F, Unit]
  private[kernel] def uniform1iv(location: Int, count: Int, arr: Array[Int]): IO[F, Unit]
  private[kernel] def uniform2i(location: Int, arg0: Int, arg1: Int): IO[F, Unit]
  private[kernel] def uniform2f(location: Int, arg0: Float, arg1: Float): IO[F, Unit]
  private[kernel] def uniform2fv(location: Int, count: Int, arr: Array[Float]): IO[F, Unit]
  private[kernel] def uniform2iv(location: Int, count: Int, arr: Array[Int]): IO[F, Unit]
  private[kernel] def uniform3i(location: Int, arg0: Int, arg1: Int, arg2: Int): IO[F, Unit]
  private[kernel] def uniform3f(location: Int, arg0: Float, arg1: Float, arg2: Float): IO[F, Unit]
  private[kernel] def uniform3fv(location: Int, count: Int, arr: Array[Float]): IO[F, Unit]
  private[kernel] def uniform3iv(location: Int, count: Int, arr: Array[Int]): IO[F, Unit]
  private[kernel] def uniform4i(location: Int, arg0: Int, arg1: Int, arg2: Int, arg3: Int): IO[F, Unit]
  private[kernel] def uniform4f(location: Int, arg0: Float, arg1: Float, arg2: Float, arg3: Float): IO[F, Unit]
  private[kernel] def uniform4fv(location: Int, count: Int, arr: Array[Float]): IO[F, Unit]
  private[kernel] def uniform4iv(location: Int, count: Int, arr: Array[Int]): IO[F, Unit]
  private[kernel] def uniformMatrix2fv(location: Int, count: Int, transpose: Boolean, arg0: Array[Float]): IO[F, Unit]
  private[kernel] def uniformMatrix3fv(location: Int, count: Int, transpose: Boolean, arg0: Array[Float]): IO[F, Unit]
  private[kernel] def uniformMatrix4fv(location: Int, count: Int, transpose: Boolean, arg0: Array[Float]): IO[F, Unit]

  def uniform[A](location: Int, value: A)(implicit update: CanUniformUpdate[F, A]): IO[F, Unit] = update(this)(location, value)

  private def getLocationTraverse[A, B, G[_] : Traverse](keys: G[A])(f: A => IO[F, B]): IO[F, G[(A, B)]] =  keys.traverse[IO[F, ?], (A, B)](s => f(s).map(s -> _))

  def getAttribLocation(program: Int, name: String): IO[F, Int]
  def getAttribLocation[G[_] : Traverse](program: Int, names: G[String]): IO[F,  G[(String, Int)]] = getLocationTraverse(names)(getAttribLocation(program, _))
  def getUniformLocation(program: Int, name: String): IO[F, Int]
  def getUniformLocations[G[_] : Traverse](program: Int, names: G[String]): IO[F, G[(String, Int)]] = getLocationTraverse(names)(getUniformLocation(program, _))

  private[kernel] def genSamplers(num: Int): IO[F, Array[Int]]
  def genSampler: IO[F, Int] = genSamplers(1) map _tup1
  def genSampler2: IO[F, (Int, Int)] = genSamplers(2) map _tup2
  def genSampler3: IO[F, (Int, Int, Int)] = genSamplers(3) map _tup3

  private[kernel] def samplerParameteri(sampler: Int, name: SamplerParameter, value: IntConstant): IO[F, Unit]
  private[kernel] def samplerParameteri(sampler: Int, name: SamplerParameter, value: Int): IO[F, Unit]
  

  def samplerParameter(sampler: Int, name: GL_TEXTURE_MIN_FILTER.type, value: TextureMinFilter): IO[F, Unit] = samplerParameteri(sampler, name, value)
  def samplerParameter(sampler: Int, name: GL_TEXTURE_WRAP_S.type, value: TextureWrap): IO[F, Unit] = samplerParameteri(sampler, name, value)
  def samplerParameter(sampler: Int, name: GL_TEXTURE_WRAP_T.type, value: TextureWrap): IO[F, Unit] = samplerParameteri(sampler, name, value)
  def samplerParameter(sampler: Int, name: GL_TEXTURE_WRAP_R.type, value: TextureWrap): IO[F, Unit] = samplerParameteri(sampler, name, value)
  def samplerParameter(sampler: Int, name: GL_TEXTURE_MAG_FILTER.type, value: TextureMagFilter): IO[F, Unit] = samplerParameteri(sampler, name, value)
  def samplerParameter(sampler: Int, name: GL_TEXTURE_MIN_LOD.type, value: Int): IO[F, Unit] = samplerParameteri(sampler, name, value)
  def samplerParameter(sampler: Int, name: GL_TEXTURE_MAX_LOD.type, value: Int): IO[F, Unit] = samplerParameteri(sampler, name, value)
  def samplerParameter(sampler: Int, name: GL_TEXTURE_COMPARE_MODE.type, value: TextureCompareMode): IO[F, Unit] = samplerParameteri(sampler, name, value)
  def samplerParameter(sampler: Int, name: GL_TEXTURE_COMPARE_FUNC.type, value: TextureCompareFunc): IO[F, Unit] = samplerParameteri(sampler, name, value)

  def copyBufferSubData(read: BufferTarget, write: BufferTarget, readOffset: Int, writeOffset: Int, size: Int): IO[F, Unit]
  def bindVertexArray(vertexArray: Int): IO[F, Unit]
  def drawBuffers(num: Int, buffers: Seq[ColorOutputTarget]): IO[F, Unit]

  private[kernel] def clearBufferfi(target: Channel, drawBuffer: Int, depth: Float, stencil: Int): IO[F, Unit]
  private[kernel] def clearBufferiv(target: Channel, drawBuffer: Int, value: Array[Int]): IO[F, Unit]
  private[kernel] def clearBufferuiv(target: Channel, drawBuffer: Int, value: Array[Int]): IO[F, Unit]
  private[kernel] def clearBufferfv(target: Channel, drawBuffer: Int, value: Array[Float]): IO[F, Unit]

  def clearBuffer(target: GL_COLOR.type, drawBuffer: DrawBuffer, red: Float, green: Float, blue: Float, alpha: Float): IO[F, Unit] = clearBufferfv(target, drawBuffer.n, Array(red, green, blue, alpha))
  def clearBuffer(target: GL_COLOR.type, drawBuffer: DrawBuffer, red: Int, green: Int, blue: Int, alpha: Int): IO[F, Unit] = clearBufferiv(target, drawBuffer.n, Array(red, green, blue, alpha))

  def clearBuffer(target: GL_DEPTH.type, value: Float): IO[F, Unit] = clearBufferfv(target, 0, Array(value))
  def clearBuffer(target: GL_STENCIL.type, value: Int): IO[F, Unit] = clearBufferiv(target, 0, Array(value))
  def clearBuffer(target: GL_DEPTH_STENCIL.type, depth: Float, stencil: Int): IO[F, Unit] = clearBufferfi(target, 0, depth, stencil)
  
  // TODO: Need to tag unsigned Ints since they are so common!  Perhaps use spire.math.Natural
  def clearBufferu(target: GL_COLOR.type, drawBuffer: DrawBuffer, red: Int, green: Int, blue: Int, alpha: Int): IO[F, Unit] = clearBufferuiv(target, drawBuffer.n, Array(red, green, blue, alpha))

  def readBuffer(src: DrawBuffer): IO[F, Unit]
  def drawElementsInstanced(mode: PrimitiveType, count: Int, `type`: IndexType, offset: Int, primCount: Int): IO[F, Unit]
  def drawElementsInstanced(mode: PrimitiveType, count: Int, `type`: IndexType, ptr: Buffer, primCount: Int): IO[F, Unit]
  def bindAttribLocation(program: Int, index: Int, name: String): IO[F, Unit]
  def blendColor(red: Float, green: Float, blue: Float, alpha: Float): IO[F, Unit]
}

@scala.annotation.implicitNotFound("Cannot find update of type $A")
trait CanUniformUpdate[F[_], A] {
  def apply(gl: GL[F])(at: Int, value: A): IO[F, Unit]
}

object CanUniformUpdate {

  implicit def intCanUniformUpdate[F[_]]: CanUniformUpdate[F, Int] = new CanUniformUpdate[F, Int] {
    def apply(gl: GL[F])(at: Int, value: Int): IO[F, Unit] = gl.uniform1i(at, value)
  }

  implicit def floatCanUniformUpdate[F[_]]: CanUniformUpdate[F, Float] = new CanUniformUpdate[F, Float] {
    def apply(gl: GL[F])(at: Int, value: Float): IO[F, Unit] = gl.uniform1f(at, value)
  }

  implicit def vec2iCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec2i] = new CanUniformUpdate[F, Vec2i] {
    def apply(gl: GL[F])(at: Int, value: Vec2i): IO[F, Unit] = gl.uniform2i(at, value.x, value.y)
  }

  implicit def vec3iCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec3i] = new CanUniformUpdate[F, Vec3i] {
    def apply(gl: GL[F])(at: Int, value: Vec3i): IO[F, Unit] = gl.uniform3i(at, value.x, value.y, value.z)
  }

  implicit def vec4iCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec4i] = new CanUniformUpdate[F, Vec4i] {
    def apply(gl: GL[F])(at: Int, value: Vec4i): IO[F, Unit] = gl.uniform4i(at, value.x, value.y, value.z, value.w)
  }
  implicit def vec2fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec2f] = new CanUniformUpdate[F, Vec2f] {
    def apply(gl: GL[F])(at: Int, value: Vec2f): IO[F, Unit] = gl.uniform2f(at, value.x, value.y)
  }

  implicit def vec3fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec3f] = new CanUniformUpdate[F, Vec3f] {
    def apply(gl: GL[F])(at: Int, value: Vec3f): IO[F, Unit] = gl.uniform3f(at, value.x, value.y, value.z)
  }

  implicit def vec4fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec4f] = new CanUniformUpdate[F, Vec4f] {
    def apply(gl: GL[F])(at: Int, value: Vec4f): IO[F, Unit] = gl.uniform4f(at, value.x, value.y, value.z, value.w)
  }
  implicit def mat2fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Mat2f] = new CanUniformUpdate[F, Mat2f] {
    def apply(gl: GL[F])(at: Int, value: Mat2f): IO[F, Unit] = gl.uniformMatrix2fv(at, 1, false, value.toArray)
  }
  implicit def mat3fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Mat3f] = new CanUniformUpdate[F, Mat3f] {
    def apply(gl: GL[F])(at: Int, value: Mat3f): IO[F, Unit] = gl.uniformMatrix3fv(at, 1, false, value.toArray)
  }
  implicit def mat4fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Mat4f] = new CanUniformUpdate[F, Mat4f] {
    def apply(gl: GL[F])(at: Int, value: Mat4f): IO[F, Unit] = gl.uniformMatrix4fv(at, 1, false, value.toArray)
  }
}


object MissingCatsImplicits {
  implicit def xorTApplicative[F[_], L](implicit F: Applicative[F]): Applicative[XorT[F, L, ?]] = new Applicative[XorT[F, L, ?]] {
    def pure[A](a: A): XorT[F, L, A] = XorT(F.pure(Xor.right(a)))
    def ap[A, B](ff: XorT[F, L, A => B])(fa: XorT[F, L, A]): XorT[F, L, B] = {
      XorT(F.ap(ff.value.map {
        case l @ Xor.Left(_) => (_: L Xor A) => l
        case Xor.Right(f) => (xa: L Xor A) => xa.map(f)   
      })(fa.value))
    }
  }

  implicit def xorTMonoid[F[_], L, A](implicit F: Applicative[F], M: Monoid[A]): Monoid[XorT[F, L, A]] = new Monoid[XorT[F, L ,A]] {
    def empty = XorT(F.pure(M.empty.right[L]))
    def combine(xx: XorT[F, L ,A], xy: XorT[F, L , A]): XorT[F, L , A] = {
      XorT(F.ap(xx.value.map {
        case l @ Xor.Left(_) => (_: L Xor A) => l
        case Xor.Right(x) => (xy: L Xor A) => xy.map(x |+| _)   
      })(xy.value))
    }}
}

import MissingCatsImplicits._

object GL {
  val run: GL[Id] = GLRunner
  
  def log(logCfg: LoggerConfig): GL[LogEffect[Id, ?]] = new GLLogger(logCfg, run)
  def debugAndLog(debugCfg: DebuggerConfig, logCfg: LoggerConfig): GL[DebugEffect[LogEffect[Id, ?], ?]] = new GLDebugger[LogEffect[Id, ?]](log(logCfg))
 
  /** Logger config for specifying which methods to log and verbosity */
  case class LoggerConfig(s: Set[String])


  /** Debugging config for specifying which methods to debug */
  case class DebuggerConfig(filtered: Set[String])

  type IO[F[_], A] = ReaderT[F, GLES30Library, A]
  type LogEffect[F[_], A] = WriterT[F, List[String], A]
  type DebugEffect[F[_], A] = XorT[F, String, A]
}
