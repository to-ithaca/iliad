package iliad
package kernel

import cats._
import cats.data._
import cats.implicits._

import GL._
import GLConstants._

private[kernel] final class GLDebugger[F[_]](gl: GL[F])(implicit A: Applicative[F], val M: Monad[DebugEffect[F, ?]]) extends GL[DebugEffect[F, ?]] {

  private def errorText(method: String)(code: Int Xor ErrorCode): String = code match {
    case Xor.Right(c) => s"call $method failed with error code $c"
    case Xor.Left(i) => s"call $method failed with undefined error code $i"
  }
  private def lift[A](io: IO[F, A]): IO[DebugEffect[F, ?], A] = io.mapF(_.liftT[DebugEffect])

  private def error[A](method: String): IO[DebugEffect[F, ?], A => A] = getError.mapF(_.transform {
    case Xor.Right(Some(c)) => Xor.Left(errorText(method)(c))
    case Xor.Right(None) => Xor.Right(identity)
    case Xor.Left(msg) => Xor.Left(msg)
  })

  private def debug[A](io: IO[F, A])(method: String): IO[DebugEffect[F, ?], A] = lift(io).ap(error(method))

  def getError: IO[DebugEffect[F, ?], Option[Int Xor ErrorCode]] = lift(gl.getError)

  def blitFramebuffer(src: Rect[Int], dest: Rect[Int], bitMask: ChannelBitMask, filter: BlitFilter): IO[DebugEffect[F, ?], Unit] = debug(gl.blitFramebuffer(src, dest, bitMask, filter))("glBlitFramebuffer")

  def viewport(rect: Rect[Int]): IO[DebugEffect[F, ?], Unit] = debug(gl.viewport(rect))("glViewport")
  def flush: IO[DebugEffect[F, ?], Unit] = debug(gl.flush)("glFlush")
  def enable(cap: Capability): IO[DebugEffect[F, ?], Unit] = debug(gl.enable(cap))(s"glEnable($cap)")

  def activeTexture(texture: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.activeTexture(texture))("glActiveTexture")
  def attachShader(program: Int, shader: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.attachShader(program, shader))("glAttachShader")
  def bindAttribLocation(program: Int, index: Int, name: String): IO[DebugEffect[F, ?], Unit] = debug(gl.bindAttribLocation(program, index, name))("glBindAttribLocation")
  def bindBuffer(target: BufferTarget, buffer: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.bindBuffer(target, buffer))("glBindBuffer")
  def bindFramebuffer(target: FramebufferTarget, framebuffer: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.bindFramebuffer(target, framebuffer))("glBindFramebuffer")
  def bindRenderbuffer(renderbuffer: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.bindRenderbuffer(renderbuffer))("glBindRenderbuffer")
  def bindTexture(target: TextureTarget, texture: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.bindTexture(target, texture))("glBindTexture")
  def bindVertexArray(vertexArray: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.bindVertexArray(vertexArray))("glBindVertexArray")
  def blendColor(red: Float, green: Float, blue: Float, alpha: Float): IO[DebugEffect[F, ?], Unit] = debug(gl.blendColor(red, green, blue, alpha))("glBlendColor")
  def bufferData(target: BufferTarget, size: Int, data: java.nio.Buffer, usage: BufferUsage): IO[DebugEffect[F, ?], Unit] = debug(gl.bufferData(target, size, data, usage))("glBufferData")
  def bufferSubData(target: BufferTarget, offset: Int, size: Int, data: java.nio.Buffer): IO[DebugEffect[F, ?], Unit] = debug(gl.bufferSubData(target, offset, size, data))("glSubBufferData")
  def checkFramebufferStatus(target: FramebufferTarget): IO[DebugEffect[F, ?], FramebufferStatus] = debug(gl.checkFramebufferStatus(target))("glCheckFramebufferStatus")
  def clear(bitMask: ChannelBitMask): IO[DebugEffect[F, ?], Unit] = debug(gl.clear(bitMask))("glClear")
  private[kernel] def clearBufferfi(target: Channel, drawBuffer: Int, depth: Float, stencil: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.clearBufferfi(target, drawBuffer, depth, stencil))("glClearbufferfi")
  private[kernel] def clearBufferfv(target: Channel, drawBuffer: Int, value: Array[Float]): IO[DebugEffect[F, ?], Unit] = debug(gl.clearBufferfv(target, drawBuffer, value))("glClearBufferfv")
  private[kernel] def clearBufferiv(target: Channel, drawBuffer: Int, value: Array[Int]): IO[DebugEffect[F, ?], Unit] = debug(gl.clearBufferiv(target, drawBuffer, value))("glClearBufferiv")
  private[kernel] def clearBufferuiv(target: Channel, drawBuffer: Int, value: Array[Int]): IO[DebugEffect[F, ?], Unit] = debug(gl.clearBufferuiv(target, drawBuffer, value))("glClearBufferuiv")
  def clearColor(red: Float, green: Float, blue: Float, alpha: Float): IO[DebugEffect[F, ?], Unit] = debug(gl.clearColor(red, green, blue, alpha))("glClearColor")
  def compileShader(shader: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.compileShader(shader))("glCompileShader")
  def copyBufferSubData(read: BufferTarget, write: BufferTarget, readOffset: Int, writeOffset: Int, size: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.copyBufferSubData(read, write, readOffset, writeOffset, size))("glCopyBufferSubData")
  def createProgram: IO[DebugEffect[F, ?], Int] = debug(gl.createProgram)("glCreateProgram")
  def createShader(`type`: ShaderType): IO[DebugEffect[F, ?], Int] = debug(gl.createShader(`type`))("glCreateShader")
  def deleteShader(shader: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.deleteShader(shader))("glDeleteShader")
  def disable(cap: Capability): IO[DebugEffect[F, ?], Unit] = debug(gl.disable(cap))("glDisable")
  def drawArrays(mode: PrimitiveType, first: Int, count: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.drawArrays(mode, first, count))("glDrawArrays")
  def drawBuffers(num: Int, buffers: Seq[ColorOutputTarget]): IO[DebugEffect[F, ?], Unit] = debug(gl.drawBuffers(num, buffers))("glDrawBuffers")
  def drawElements(mode: PrimitiveType, count: Int, `type`: IndexType, indices: java.nio.Buffer): IO[DebugEffect[F, ?], Unit] = debug(gl.drawElements(mode, count, `type`, indices))("glDrawElements")
  def drawElements(mode: PrimitiveType, count: Int, `type`: IndexType, offset: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.drawElements(mode, count, `type`, offset))("glDrawElements")
  def drawElementsInstanced(mode: PrimitiveType, count: Int, `type`: IndexType, ptr: java.nio.Buffer, primCount: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.drawElementsInstanced(mode, count, `type`, ptr, primCount))("glDrawElementsInstanced")
  def drawElementsInstanced(mode: PrimitiveType, count: Int, `type`: IndexType, offset: Int, primCount: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.drawElementsInstanced(mode, count, `type`, offset, primCount))("glDrawElementsInstanced")
  def enableVertexAttribArray(location: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.enableVertexAttribArray(location))("glEnableVertexAttribArray")
  def framebufferRenderbuffer(target: FramebufferTarget, attachment: FramebufferAttachment, renderbuffer: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.framebufferRenderbuffer(target, attachment, renderbuffer))("glFramebufferRenderbuffer")
  def framebufferTexture2D(target: FramebufferTarget, attachment: FramebufferAttachment, texTarget: FramebufferTexTarget, texture: Int, level: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.framebufferTexture2D(target, attachment, texTarget, texture, level))("glFramebufferTexture2D")
  private[kernel] def genBuffers(num: Int): IO[DebugEffect[F, ?], Array[Int]] = debug(gl.genBuffers(num))("glGenBuffers")
  private[kernel] def genFramebuffers(num: Int): IO[DebugEffect[F, ?], Array[Int]] = debug(gl.genFramebuffers(num))("glGenFramebuffers")
  private[kernel] def genRenderbuffers(num: Int): IO[DebugEffect[F, ?], Array[Int]] = debug(gl.genRenderbuffers(num))("glGenRenderbuffers")
  private[kernel] def genSamplers(num: Int): IO[DebugEffect[F, ?], Array[Int]] = debug(gl.genSamplers(num))("glGenSamplers")
  private[kernel] def genTextures(num: Int): IO[DebugEffect[F, ?], Array[Int]] = debug(gl.genTextures(num))("glGenTextures")
  def getAttribLocation(program: Int, name: String): IO[DebugEffect[F, ?], Int] = debug(gl.getAttribLocation(program, name))("glGetAttribLocation")
  private[kernel] def getShaderInfoLog(shader: Int, maxLength: Int): IO[DebugEffect[F, ?], String] = debug(gl.getShaderInfoLog(shader, maxLength))("glGetShaderInfoLog")
  private[kernel] def getShaderiv(shader: Int, pname: ShaderParameter): IO[DebugEffect[F, ?], Int] = debug(gl.getShaderiv(shader, pname))("glGetShaderiv")
  def getUniformLocation(program: Int, name: String): IO[DebugEffect[F, ?], Int] = debug(gl.getUniformLocation(program, name))("glGetUniformLocation")
  def linkProgram(program: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.linkProgram(program))("glLinkProgram")
  def pixelStorei(name: PixelStoreParameter, value: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.pixelStorei(name, value))("glPixelStorei")
  def readBuffer(src: DrawBuffer): IO[DebugEffect[F, ?], Unit] = debug(gl.readBuffer(src))("glReadBuffer")
  def renderbufferStorage(format: RenderbufferInternalFormat, width: Int, height: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.renderbufferStorage(format, width, height))("glRenderbufferStorage")
  private[kernel] def samplerParameteri(sampler: Int, name: SamplerParameter, value: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.samplerParameteri(sampler, name, value))("glSamplerParameteri")
  private[kernel] def samplerParameteri(sampler: Int, name: SamplerParameter, value: IntConstant): IO[DebugEffect[F, ?], Unit] = debug(gl.samplerParameteri(sampler, name, value))("glSamplerParameteri")
  def shaderSource(shader: Int, count: Int, sources: Seq[String]): IO[DebugEffect[F, ?], Unit] = debug(gl.shaderSource(shader, count, sources))("glShaderSource")
  def texImage2D(target: TextureTarget, level: Int, internalFormat: TextureInteralFormat, width: Int, height: Int, format: TextureFormat, `type`: TexturePixelType, data: java.nio.Buffer): IO[DebugEffect[F, ?], Unit] = debug(gl.texImage2D(target, level, internalFormat, width, height, format, `type`, data))("glShaderSource")
  private[kernel] def texParameteri(target: TextureTarget, name: TextureParameter, value: IntConstant): IO[DebugEffect[F, ?], Unit] = debug(gl.texParameteri(target, name, value))("glTexParameteri")
  private[kernel] def texParameteri(target: TextureTarget, name: TextureParameter, value: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.texParameteri(target, name, value))("glTexParameteri")
  private[kernel] def uniform1f(location: Int, arg0: Float): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform1f(location, arg0))("glUniform1f")
  private[kernel] def uniform1fv(location: Int, count: Int, arr: Array[Float]): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform1fv(location, count, arr))("glUniform1fv")
  private[kernel] def uniform1i(location: Int, arg0: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform1i(location, arg0))("glUniform1i")
  private[kernel] def uniform1iv(location: Int, count: Int, arr: Array[Int]): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform1iv(location, count, arr))("glUniform1iv")
  private[kernel] def uniform2f(location: Int, arg0: Float, arg1: Float): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform2f(location, arg0, arg1))("glUniform2f")
  private[kernel] def uniform2fv(location: Int, count: Int, arr: Array[Float]): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform2fv(location, count, arr))("glUniform2fv")
  private[kernel] def uniform2i(location: Int, arg0: Int, arg1: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform2i(location, arg0, arg1))("glUniform2i")
  private[kernel] def uniform2iv(location: Int, count: Int, arr: Array[Int]): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform2iv(location, count, arr))("glUniform2iv")
  private[kernel] def uniform3f(location: Int, arg0: Float, arg1: Float, arg2: Float): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform3f(location, arg0, arg1, arg2))("glUniform3f")
  private[kernel] def uniform3fv(location: Int, count: Int, arr: Array[Float]): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform3fv(location, count, arr))("glUniform3fv")
  private[kernel] def uniform3i(location: Int, arg0: Int, arg1: Int, arg2: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform3i(location, arg0, arg1, arg2))("glUniform3i")
  private[kernel] def uniform3iv(location: Int, count: Int, arr: Array[Int]): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform3iv(location, count, arr))("glUniform3iv")
  private[kernel] def uniform4f(location: Int, arg0: Float, arg1: Float, arg2: Float, arg3: Float): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform4f(location, arg0, arg1, arg2, arg3))("glUniform4f")
  private[kernel] def uniform4fv(location: Int, count: Int, arr: Array[Float]): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform4fv(location, count, arr))("glUniform4fv")
  private[kernel] def uniform4i(location: Int, arg0: Int, arg1: Int, arg2: Int, arg3: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform4i(location, arg0, arg1, arg2, arg3))("glUniform4i")
  private[kernel] def uniform4iv(location: Int, count: Int, arr: Array[Int]): IO[DebugEffect[F, ?], Unit] = debug(gl.uniform4iv(location, count, arr))("glUniform4iv")
  private[kernel] def uniformMatrix2fv(location: Int, count: Int, transpose: Boolean, arg0: Array[Float]): IO[DebugEffect[F, ?], Unit] = debug(gl.uniformMatrix2fv(location, count, transpose, arg0))("glUniformMatrix2fv")
  private[kernel] def uniformMatrix3fv(location: Int, count: Int, transpose: Boolean, arg0: Array[Float]): IO[DebugEffect[F, ?], Unit] = debug(gl.uniformMatrix3fv(location, count, transpose, arg0))("glUniformMatrix3fv")
  private[kernel] def uniformMatrix4fv(location: Int, count: Int, transpose: Boolean, arg0: Array[Float]): IO[DebugEffect[F, ?], Unit] = debug(gl.uniformMatrix4fv(location, count, transpose, arg0))("glUniformMatrix4fv")
  def useProgram(program: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.useProgram(program))("glUseProgram")
  def vertexAttribPointer(location: Int, size: Int, `type`: VertexAttribType, normalized: Boolean, stride: Int, offset: Int): IO[DebugEffect[F, ?], Unit] = debug(gl.vertexAttribPointer(location, size, `type`, normalized, stride, offset))("glVertexAttribPointer")
}
