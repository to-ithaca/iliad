package iliad
package kernel
package gl

import iliad.kernel.platform.gl._

import java.nio.{IntBuffer, ByteBuffer, Buffer => NBuffer}

import imp._
import cats._
import cats.implicits._
import cats.data._



import Constant._
import GL._

private[gl] object NoEffectRunning extends GL[Id] {

  val S: Semigroup[Unit] = imp[Semigroup[Unit]]

  def blitFramebuffer(src: Rectangle, dest: Rectangle, bitMask: ChannelBitMask, filter: BlitFilter): IO[Id, Unit] = {
    val sbr = src.bottomRight
    val dbr = dest.bottomRight
    Reader(_.glBlitFramebuffer(src.topLeft.x, src.topLeft.y, sbr.x, sbr.y, dest.topLeft.x, dest.topLeft.y, dbr.x, dbr.y, bitMask.toInt, filter.toInt))
  }
  def viewport(rect: Rectangle): IO[Id, Unit] = Reader(_.glViewport(rect.topLeft.x, rect.topLeft.y, rect.width, rect.height))
  def enable(cap: Capability): IO[Id, Unit] = Reader(_.glEnable(cap.toInt))
  def disable(cap: Capability): IO[Id, Unit] = Reader(_.glDisable(cap.toInt))
  def flush: IO[Id, Unit] = Reader(_.glFlush())
  def clear(bitMask: ChannelBitMask): IO[Id, Unit] = Reader(_.glClear(bitMask.toInt))
  def clearColor(red: Float, green: Float, blue: Float, alpha: Float): IO[Id, Unit] = Reader(_.glClearColor(red, green, blue, alpha))
  def getError: IO[Id, Option[Int Xor ErrorCode]] = Reader(_.glGetError() match {
    case v if v == GL_NO_ERROR.toInt => None
    case v => Some(SealedEnum.values[ErrorCode].find(_.toInt == v).toRightXor(v))
  })
  def createShader(`type`: ShaderType): IO[Id, Int] = Reader(_.glCreateShader(`type`.toInt))
  def shaderSource(shader: Int, count: Int, sources: Seq[String]): IO[Id, Unit] = {
    val ls = sources.map(_.size).toArray
    Reader(_.glShaderSource(shader, count, sources.toArray, ls))
  }
  def deleteShader(shader: Int): IO[Id, Unit] = Reader(_.glDeleteShader(shader))
  def compileShader(shader: Int): IO[Id, Unit] = Reader(_.glCompileShader(shader))
  def attachShader(program: Int, shader: Int): IO[Id, Unit] = Reader(_.glAttachShader(program, shader))

  def getShaderiv(shader: Int, pname: ShaderParameter): IO[Id, Int] = Reader { gl =>
    val ptr = Buffer.capacity[Int](1)
    gl.glGetShaderiv(shader, pname.toInt, ptr)
    ptr.get()
  }
  def getShaderInfoLog(shader: Int, maxLength: Int): IO[Id, String] = Reader { gl =>
    val lenPtr = Buffer.capacity[Int](1)
    val logPtr = Buffer.capacity[Byte](maxLength)
    gl.glGetShaderInfoLog(shader, maxLength, lenPtr, logPtr)
    val len = lenPtr.get()
    val arr = new Array[Byte](len)
    logPtr.get(arr, 0, len)
    new String(arr)
  }

  def createProgram: IO[Id, Int] = Reader(_.glCreateProgram())
  def useProgram(program: Int): IO[Id, Unit] = Reader(_.glUseProgram(program))
  def linkProgram(program: Int): IO[Id, Unit] = Reader(_.glLinkProgram(program))

  //nasty but the signature of gen means the alternatives are worse!
  private def genObject(f: Lib => (Int, IntBuffer) => Unit, num: Int): IO[Id, Array[Int]] = Reader { gl =>
    val ptr = Buffer.capacity[Int](num)
    f(gl)(num, ptr)
    val arr = new Array[Int](num)
    ptr.get(arr, 0, num)
    arr
  }

  def genBuffers(num: Int): IO[Id, Array[Int]] = genObject(_.glGenBuffers, num)
  def bindBuffer(target: BufferTarget, buffer: Int): IO[Id, Unit] = Reader(_.glBindBuffer(target.toInt, buffer))
  def bufferData(target: BufferTarget, size: Int, data: NBuffer, usage: BufferUsage): IO[Id, Unit] = Reader(_.glBufferData(target.toInt, size, data, usage.toInt))
  def bufferSubData(target: BufferTarget, offset: Int, size: Int, data: NBuffer): IO[Id, Unit] = Reader(_.glBufferSubData(target.toInt, offset, size, data))
  def enableVertexAttribArray(location: Int): IO[Id, Unit] = Reader(_.glEnableVertexAttribArray(location))
  def vertexAttribPointer(location: Int, size: Int, `type`: VertexAttribType, normalized: Boolean, stride: Int, offset: Int): IO[Id, Unit] = Reader(_.glVertexAttribPointer(location, size, `type`.toInt, normalized, stride, offset))
  def genFramebuffers(num: Int): IO[Id, Array[Int]] = genObject(_.glGenFramebuffers, num)
  def bindFramebuffer(target: FramebufferTarget, framebuffer: Int): IO[Id, Unit] = Reader(_.glBindFramebuffer(target.toInt, framebuffer))
  def framebufferRenderbuffer(target: FramebufferTarget, attachment: FramebufferAttachment, renderbuffer: Int): IO[Id, Unit] = Reader(_.glFramebufferRenderbuffer(target.toInt, attachment.toInt, GL_RENDERBUFFER.toInt, renderbuffer))
  def checkFramebufferStatus(target: FramebufferTarget): IO[Id, FramebufferStatus] = Reader[Lib, Unit](_.glCheckFramebufferStatus(target.toInt)).map(c => SealedEnum.values[FramebufferStatus].find(_.toInt == c).get)
  def framebufferTexture2D(target: FramebufferTarget, attachment: FramebufferAttachment, texTarget: FramebufferTexTarget, texture: Int, level: Int): IO[Id, Unit] = Reader(_.glFramebufferTexture2D(target.toInt, attachment.toInt, texTarget.toInt, texture, level))
  def genRenderbuffers(num: Int): IO[Id, Array[Int]] = genObject(_.glGenRenderbuffers, num)
  def bindRenderbuffer(renderbuffer: Int): IO[Id, Unit] = Reader(_.glBindBuffer(GL_RENDERBUFFER.toInt, renderbuffer))
  def renderbufferStorage(format: RenderbufferInternalFormat, width: Int, height: Int): IO[Id, Unit] = Reader(_.glRenderbufferStorage(GL_RENDERBUFFER.toInt, format.toInt, width, height))
  def bindTexture(target: TextureTarget, texture: Int): IO[Id, Unit] = Reader(_.glBindTexture(target.toInt, texture))
  def genTextures(num: Int): IO[Id, Array[Int]] = genObject(_.glGenTextures, num)
  def texParameteri(target: TextureTarget, name: TextureParameter, value: Int): IO[Id, Unit] = Reader(_.glTexParameteri(target.toInt, name.toInt, value))
  def texParameteri(target: TextureTarget, name: TextureParameter, value: IntValue): IO[Id, Unit] = Reader(_.glTexParameteri(target.toInt, name.toInt, value.toInt))

  def texImage2D(target: TextureTarget, level: Int, internalFormat: TextureInteralFormat, width: Int, height: Int, format: TextureFormat, `type`: TexturePixelType, data: NBuffer): IO[Id, Unit] = Reader(_.glTexImage2D(target.toInt, level, internalFormat.toInt, width, height, 0, internalFormat.toInt, `type`.toInt, data))

  def pixelStorei(name: PixelStoreParameter, value: Int): IO[Id, Unit] = Reader(_.glPixelStorei(name.toInt, value))
  def activeTexture(texture: Int): IO[Id, Unit] = Reader(_.glActiveTexture(texture))

  def drawArrays(mode: PrimitiveType, first: Int, count: Int): IO[Id, Unit] = Reader(_.glDrawArrays(mode.toInt, first, count))
  def drawElements(mode: PrimitiveType, count: Int, `type`: IndexType, offset: Int): IO[Id, Unit] = Reader(_.glDrawElements(mode.toInt, count, `type`.toInt, offset))
  def drawElements(mode: PrimitiveType, count: Int, `type`: IndexType, indices: NBuffer): IO[Id, Unit] = Reader(_.glDrawElements(mode.toInt, count, `type`.toInt, indices))

  def uniform1i(location: Int, arg0: Int): IO[Id, Unit] = Reader(_.glUniform1i(location, arg0))
  def uniform1f(location: Int, arg0: Float): IO[Id, Unit] = Reader(_.glUniform1f(location, arg0))
  def uniform1fv(location: Int, count: Int, arr: Array[Float]): IO[Id, Unit] = Reader(_.glUniform1fv(location, count, arr))
  def uniform1iv(location: Int, count: Int, arr: Array[Int]): IO[Id, Unit] = Reader(_.glUniform1iv(location, count, arr))
  def uniform2i(location: Int, arg0: Int, arg1: Int): IO[Id, Unit] = Reader(_.glUniform2i(location, arg0, arg0))
  def uniform2f(location: Int, arg0: Float, arg1: Float): IO[Id, Unit] = Reader(_.glUniform2f(location, arg0, arg1))
  def uniform2fv(location: Int, count: Int, arr: Array[Float]): IO[Id, Unit] = Reader(_.glUniform2fv(location, count, arr))
  def uniform2iv(location: Int, count: Int, arr: Array[Int]): IO[Id, Unit] = Reader(_.glUniform2iv(location, count, arr))
  def uniform3i(location: Int, arg0: Int, arg1: Int, arg2: Int): IO[Id, Unit] = Reader(_.glUniform3i(location, arg0, arg1, arg2))
  def uniform3f(location: Int, arg0: Float, arg1: Float, arg2: Float): IO[Id, Unit] = Reader(_.glUniform3f(location, arg0, arg1, arg2))
  def uniform3fv(location: Int, count: Int, arr: Array[Float]): IO[Id, Unit] = Reader(_.glUniform3fv(location, count, arr))
  def uniform3iv(location: Int, count: Int, arr: Array[Int]): IO[Id, Unit] = Reader(_.glUniform3iv(location, count, arr))
  def uniform4i(location: Int, arg0: Int, arg1: Int, arg2: Int, arg3: Int): IO[Id, Unit] = Reader(_.glUniform4i(location, arg0, arg1, arg2, arg3))
  def uniform4f(location: Int, arg0: Float, arg1: Float, arg2: Float, arg3: Float): IO[Id, Unit] = Reader(_.glUniform4f(location, arg0, arg1, arg2, arg3))
  def uniform4fv(location: Int, count: Int, arr: Array[Float]): IO[Id, Unit] = Reader(_.glUniform4fv(location, count, arr))
  def uniform4iv(location: Int, count: Int, arr: Array[Int]): IO[Id, Unit] = Reader(_.glUniform4iv(location, count, arr))

  def uniformMatrix2fv(location: Int, count: Int, transpose: Boolean, arr: Array[Float]): IO[Id, Unit] = Reader(_.glUniformMatrix2fv(location, count, transpose, arr))
  def uniformMatrix3fv(location: Int, count: Int, transpose: Boolean, arr: Array[Float]): IO[Id, Unit] = Reader(_.glUniformMatrix3fv(location, count, transpose, arr))
  def uniformMatrix4fv(location: Int, count: Int, transpose: Boolean, arr: Array[Float]): IO[Id, Unit] = Reader(_.glUniformMatrix4fv(location, count, transpose, arr))

  def getAttribLocation(program: Int, name: String): IO[Id, Int] = Reader(_.glGetAttribLocation(program, name))
  def getUniformLocation(program: Int, name: String): IO[Id, Int] = Reader(_.glGetUniformLocation(program, name))

  def genSamplers(num: Int): IO[Id, Array[Int]] = genObject(_.glGenSamplers, num)
  
  def samplerParameteri(sampler: Int, name: SamplerParameter, value: Int): IO[Id, Unit] = Reader(_.glSamplerParameteri(sampler, name.toInt, value))
  def samplerParameteri(sampler: Int, name: SamplerParameter, value: IntValue): IO[Id, Unit] = Reader(_.glSamplerParameteri(sampler, name.toInt, value.toInt))

  def bindAttribLocation(program: Int,index: Int,name: String): IO[cats.Id, Unit] = Reader(_.glBindAttribLocation(program, index, name))
  def bindVertexArray(vertexArray: Int): IO[cats.Id, Unit] = Reader(_.glBindVertexArray(vertexArray))
  def blendColor(red: Float,green: Float,blue: Float,alpha: Float): IO[cats.Id,Unit] = Reader(_.glBlendColor(red, green, blue, alpha))
  private[gl] def clearBufferfi(target: Channel, drawBuffer: Int, depth: Float, stencil: Int): IO[cats.Id, Unit] = Reader(_.glClearBufferfi(target.toInt, drawBuffer, depth, stencil))
  private[gl] def clearBufferfv(target: Channel, drawBuffer: Int, value: Array[Float]): IO[cats.Id, Unit] = Reader(_.glClearBufferfv(target.toInt, drawBuffer, value))
  private[gl] def clearBufferiv(target: Channel, drawBuffer: Int,value: Array[Int]): IO[cats.Id, Unit] = Reader(_.glClearBufferiv(target.toInt, drawBuffer, value))
  private[gl] def clearBufferuiv(target: Channel, drawBuffer: Int,value: Array[Int]): IO[cats.Id, Unit] = Reader(_.glClearBufferuiv(target.toInt, drawBuffer, value))
  def copyBufferSubData(read: BufferTarget, write: BufferTarget, readOffset: Int, writeOffset: Int, size: Int): IO[cats.Id,Unit] = Reader(_.glCopyBufferSubData(read.toInt, write.toInt, readOffset, writeOffset, size))
  def drawBuffers(num: Int, buffers: Seq[ColorOutputTarget]): IO[cats.Id,Unit] = Reader(_.glDrawBuffers(num, Buffer(buffers.map(_.toInt):_*)))
  def drawElementsInstanced(mode: PrimitiveType, count: Int, `type`: IndexType, ptr: NBuffer, primCount: Int): IO[cats.Id,Unit] = Reader(_.glDrawElementsInstanced(mode.toInt, count, `type`.toInt, ptr, primCount))
  def drawElementsInstanced(mode: PrimitiveType, count: Int, `type`: IndexType, offset: Int, primCount: Int): IO[cats.Id,Unit] = Reader(_.glDrawElementsInstanced(mode.toInt, count, `type`.toInt, offset, primCount))
  def readBuffer(src: DrawBuffer): IO[cats.Id,Unit] = Reader(_.glReadBuffer(src.toInt))
}
