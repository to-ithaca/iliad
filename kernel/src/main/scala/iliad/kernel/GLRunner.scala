package iliad
package kernel

import iliad.kernel.platform._

import java.nio.{IntBuffer, Buffer => NBuffer}

import cats._
import cats.implicits._
import cats.data._

import GL._
import GLConstants._

private[kernel] object GLRunner extends GL[Id] {

  def blitFramebuffer(src: Rect[Int], dest: Rect[Int], bitMask: ChannelBitMask, filter: BlitFilter): IO[Id, Unit] = {
    val sbr = src.bottomRight
    val dbr = dest.bottomRight
    Reader(_.glBlitFramebuffer(src.topLeft.x, src.topLeft.y, sbr.x, sbr.y, dest.topLeft.x, dest.topLeft.y, dbr.x, dbr.y, bitMask.value, filter.value))
  }
  def viewport(rect: Rect[Int]): IO[Id, Unit] = Reader(_.glViewport(rect.topLeft.x, rect.topLeft.y, rect.width, rect.height))
  def enable(cap: Capability): IO[Id, Unit] = Reader(_.glEnable(cap.value))
  def disable(cap: Capability): IO[Id, Unit] = Reader(_.glDisable(cap.value))
  def flush: IO[Id, Unit] = Reader(_.glFlush())
  def clear(bitMask: ChannelBitMask): IO[Id, Unit] = Reader(_.glClear(bitMask.value))
  def clearColor(red: Float, green: Float, blue: Float, alpha: Float): IO[Id, Unit] = Reader(_.glClearColor(red, green, blue, alpha))
  def getError: IO[Id, Option[Int Xor ErrorCode]] = Reader(_.glGetError() match {
    case v if v == GL_NO_ERROR.value => None
    case v => Some(SealedEnum.values[ErrorCode].find(_.value == v).toRightXor(v))
  })
  def createShader(`type`: ShaderType): IO[Id, Int] = Reader(_.glCreateShader(`type`.value))
  def shaderSource(shader: Int, count: Int, sources: Seq[String]): IO[Id, Unit] = {
    val ls = sources.map(_.size).toArray
    Reader(_.glShaderSource(shader, count, sources.toArray, ls))
  }
  def deleteShader(shader: Int): IO[Id, Unit] = Reader(_.glDeleteShader(shader))
  def compileShader(shader: Int): IO[Id, Unit] = Reader(_.glCompileShader(shader))
  def attachShader(program: Int, shader: Int): IO[Id, Unit] = Reader(_.glAttachShader(program, shader))

  def getShaderiv(shader: Int, pname: ShaderParameter): IO[Id, Int] = Reader { gl =>
    val ptr = Buffer.capacity[Int](1)
    gl.glGetShaderiv(shader, pname.value, ptr)
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
  private def genObject(f: GLES30Library => (Int, IntBuffer) => Unit, num: Int): IO[Id, Array[Int]] = Reader { gl =>
    val ptr = Buffer.capacity[Int](num)
    f(gl)(num, ptr)
    val arr = new Array[Int](num)
    ptr.get(arr, 0, num)
    arr
  }

  def genBuffers(num: Int): IO[Id, Array[Int]] = genObject(_.glGenBuffers, num)
  def bindBuffer(target: BufferTarget, buffer: Int): IO[Id, Unit] = Reader(_.glBindBuffer(target.value, buffer))
  def bufferData(target: BufferTarget, size: Int, data: NBuffer, usage: BufferUsage): IO[Id, Unit] = Reader(_.glBufferData(target.value, size, data, usage.value))
  def bufferSubData(target: BufferTarget, offset: Int, size: Int, data: NBuffer): IO[Id, Unit] = Reader(_.glBufferSubData(target.value, offset, size, data))
  def enableVertexAttribArray(location: Int): IO[Id, Unit] = Reader(_.glEnableVertexAttribArray(location))
  def vertexAttribPointer(location: Int, size: Int, `type`: VertexAttribType, normalized: Boolean, stride: Int, offset: Int): IO[Id, Unit] = Reader(_.glVertexAttribPointer(location, size, `type`.value, normalized, stride, offset))
  def genFramebuffers(num: Int): IO[Id, Array[Int]] = genObject(_.glGenFramebuffers, num)
  def bindFramebuffer(target: FramebufferTarget, framebuffer: Int): IO[Id, Unit] = Reader(_.glBindFramebuffer(target.value, framebuffer))
  def framebufferRenderbuffer(target: FramebufferTarget, attachment: FramebufferAttachment, renderbuffer: Int): IO[Id, Unit] = Reader(_.glFramebufferRenderbuffer(target.value, attachment.value, GL_RENDERBUFFER.value, renderbuffer))
  def checkFramebufferStatus(target: FramebufferTarget): IO[Id, FramebufferStatus] = Reader[GLES30Library, Unit](_.glCheckFramebufferStatus(target.value)).map(c => SealedEnum.values[FramebufferStatus].find(_.value == c).get)
  def framebufferTexture2D(target: FramebufferTarget, attachment: FramebufferAttachment, texTarget: FramebufferTexTarget, texture: Int, level: Int): IO[Id, Unit] = Reader(_.glFramebufferTexture2D(target.value, attachment.value, texTarget.value, texture, level))
  def genRenderbuffers(num: Int): IO[Id, Array[Int]] = genObject(_.glGenRenderbuffers, num)
  def bindRenderbuffer(renderbuffer: Int): IO[Id, Unit] = Reader(_.glBindBuffer(GL_RENDERBUFFER.value, renderbuffer))
  def renderbufferStorage(format: RenderbufferInternalFormat, width: Int, height: Int): IO[Id, Unit] = Reader(_.glRenderbufferStorage(GL_RENDERBUFFER.value, format.value, width, height))
  def bindTexture(target: TextureTarget, texture: Int): IO[Id, Unit] = Reader(_.glBindTexture(target.value, texture))
  def genTextures(num: Int): IO[Id, Array[Int]] = genObject(_.glGenTextures, num)
  def texParameteri(target: TextureTarget, name: TextureParameter, value: Int): IO[Id, Unit] = Reader(_.glTexParameteri(target.value, name.value, value))
  def texParameteri(target: TextureTarget, name: TextureParameter, value: IntConstant): IO[Id, Unit] = Reader(_.glTexParameteri(target.value, name.value, value.value))

  def texImage2D(target: TextureTarget, level: Int, internalFormat: TextureInternalFormat, width: Int, height: Int, format: TextureFormat, `type`: TexturePixelType, data: NBuffer): IO[Id, Unit] = Reader(_.glTexImage2D(target.value, level, internalFormat.value, width, height, 0, internalFormat.value, `type`.value, data))

  def pixelStorei(name: PixelStoreParameter, value: Int): IO[Id, Unit] = Reader(_.glPixelStorei(name.value, value))
  def activeTexture(texture: Int): IO[Id, Unit] = Reader(_.glActiveTexture(texture))

  def drawArrays(mode: PrimitiveType, first: Int, count: Int): IO[Id, Unit] = Reader(_.glDrawArrays(mode.value, first, count))
  def drawElements(mode: PrimitiveType, count: Int, `type`: IndexType, offset: Int): IO[Id, Unit] = Reader(_.glDrawElements(mode.value, count, `type`.value, offset))
  def drawElements(mode: PrimitiveType, count: Int, `type`: IndexType, indices: NBuffer): IO[Id, Unit] = Reader(_.glDrawElements(mode.value, count, `type`.value, indices))

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
  
  def samplerParameteri(sampler: Int, name: SamplerParameter, value: Int): IO[Id, Unit] = Reader(_.glSamplerParameteri(sampler, name.value, value))
  def samplerParameteri(sampler: Int, name: SamplerParameter, value: IntConstant): IO[Id, Unit] = Reader(_.glSamplerParameteri(sampler, name.value, value.value))

  def bindAttribLocation(program: Int,index: Int,name: String): IO[cats.Id, Unit] = Reader(_.glBindAttribLocation(program, index, name))
  def bindVertexArray(vertexArray: Int): IO[cats.Id, Unit] = Reader(_.glBindVertexArray(vertexArray))
  def blendColor(red: Float,green: Float,blue: Float,alpha: Float): IO[cats.Id,Unit] = Reader(_.glBlendColor(red, green, blue, alpha))
  private[kernel] def clearBufferfi(target: Channel, drawBuffer: Int, depth: Float, stencil: Int): IO[cats.Id, Unit] = Reader(_.glClearBufferfi(target.value, drawBuffer, depth, stencil))
  private[kernel] def clearBufferfv(target: Channel, drawBuffer: Int, value: Array[Float]): IO[cats.Id, Unit] = Reader(_.glClearBufferfv(target.value, drawBuffer, value))
  private[kernel] def clearBufferiv(target: Channel, drawBuffer: Int,value: Array[Int]): IO[cats.Id, Unit] = Reader(_.glClearBufferiv(target.value, drawBuffer, value))
  private[kernel] def clearBufferuiv(target: Channel, drawBuffer: Int,value: Array[Int]): IO[cats.Id, Unit] = Reader(_.glClearBufferuiv(target.value, drawBuffer, value))
  def copyBufferSubData(read: BufferTarget, write: BufferTarget, readOffset: Int, writeOffset: Int, size: Int): IO[cats.Id,Unit] = Reader(_.glCopyBufferSubData(read.value, write.value, readOffset, writeOffset, size))
  def drawBuffers(num: Int, buffers: Seq[ColorOutputTarget]): IO[cats.Id,Unit] = Reader(_.glDrawBuffers(num, Buffer(buffers.map(_.value):_*)))
  def drawElementsInstanced(mode: PrimitiveType, count: Int, `type`: IndexType, ptr: NBuffer, primCount: Int): IO[cats.Id,Unit] = Reader(_.glDrawElementsInstanced(mode.value, count, `type`.value, ptr, primCount))
  def drawElementsInstanced(mode: PrimitiveType, count: Int, `type`: IndexType, offset: Int, primCount: Int): IO[cats.Id,Unit] = Reader(_.glDrawElementsInstanced(mode.value, count, `type`.value, offset, primCount))
  def readBuffer(src: DrawBuffer): IO[cats.Id,Unit] = Reader(_.glReadBuffer(src.value))
}
