package iliad
package kernel
package gl

import cats._
import cats.data._
import cats.implicits._

import Constant._
import GL._


private[gl] final class Logging[F[_]](config: LoggerConfig, gl: GL[F])(implicit F: Functor[F], val S: Semigroup[Logger[F, Unit]], val M: Monad[Logger[F, ?]]) extends GL[Logger[F, ?]] {  
  def log[A](io: IO[F, A])(s: String): IO[Logger[F, ?], A] =
    io.mapF[Logger[F, ?], A](_.liftT[Logger[?[_], ?]].mapWritten(_ => List(s)))
  def blitFramebuffer(src: Rectangle, dest: Rectangle, bitMask: ChannelBitMask, filter: BlitFilter): IO[Logger[F, ?], Unit] =
    log(gl.blitFramebuffer(src, dest, bitMask, filter))("glBlitFramebuffer")
  def viewport(rect: Rectangle): IO[Logger[F, ?], Unit] = ???
  def flush: IO[Logger[F, ?], Unit] = ???
  def enable(cap: Capability): IO[Logger[F, ?], Unit] = log(gl.enable(cap))(s"glEnable($cap)")
  def getError: IO[Logger[F, ?], Option[Int Xor ErrorCode]] = log(gl.getError)("glGetError")

  def activeTexture(texture: Int): IO[Logger[F, ?], Unit] = log(gl.activeTexture(texture))(s"glActiveTexture($texture)")
  def attachShader(program: Int,shader: Int): IO[Logger[F, ?], Unit] = log(gl.attachShader(program, shader))(s"glAttachShader($program, $shader)")
  def bindAttribLocation(program: Int,index: Int,name: String): IO[Logger[F, ?], Unit] = log(gl.bindAttribLocation(program, index, name))(s"glBindAttribLocation($program, $index, $name)")
  def bindBuffer(target: BufferTarget,buffer: Int): IO[Logger[F, ?], Unit] = log(gl.bindBuffer(target, buffer))(s"glBindBuffer($target, $buffer)")
  def bindFramebuffer(target: FramebufferTarget,framebuffer: Int): IO[Logger[F, ?], Unit] = log(gl.bindFramebuffer(target, framebuffer))(s"glBindFramebuffer($target, $framebuffer)")
  def bindRenderbuffer(renderbuffer: Int): IO[Logger[F, ?], Unit] = log(gl.bindRenderbuffer(renderbuffer))(s"glBindRenderbuffer($renderbuffer)")
  def bindTexture(target: TextureTarget,texture: Int): IO[Logger[F, ?], Unit] = log(gl.bindTexture(target, texture))(s"glBindTexture($target, $texture)")
  def bindVertexArray(vertexArray: Int): IO[Logger[F, ?], Unit] = log(gl.bindVertexArray(vertexArray))(s"glBindVertexArray($vertexArray)")
  def blendColor(red: Float,green: Float,blue: Float,alpha: Float): IO[Logger[F, ?], Unit] = log(gl.blendColor(red, green, blue, alpha))(s"glBlendColor($red, $green, $blue, $alpha)")
  def bufferData(target: BufferTarget,size: Int,data: java.nio.Buffer,usage: BufferUsage): IO[Logger[F, ?], Unit] = log(gl.bufferData(target, size, data, usage))(s"glBufferData($target, $size, data, $usage)")
  def bufferSubData(target: BufferTarget,offset: Int,size: Int,data: java.nio.Buffer): IO[Logger[F, ?], Unit] = log(gl.bufferSubData(target, offset, size, data))(s"glBufferSubData($target, $offset, $size, data)")
  def checkFramebufferStatus(target: FramebufferTarget): IO[Logger[F, ?], FramebufferStatus] = log(gl.checkFramebufferStatus(target))(s"glCheckFramebufferStatus($target)")
  def clear(bitMask: iliad.kernel.gl.ChannelBitMask): IO[Logger[F, ?], Unit] = log(gl.clear(bitMask))(s"glClear($bitMask)")
  private[gl] def clearBufferfi(target: Channel,drawBuffer: Int,depth: Float,stencil: Int): IO[Logger[F, ?], Unit] = log(gl.clearBufferfi(target, drawBuffer, depth, stencil))(s"glClearBufferfi($target, $drawBuffer, $depth, $stencil)")
  private[gl] def clearBufferfv(target: Channel,drawBuffer: Int,value: Array[Float]): IO[Logger[F, ?], Unit] = log(gl.clearBufferfv(target, drawBuffer, value))(s"glClearBufferfv($target, $drawBuffer, ${value.toList})")
  private[gl] def clearBufferiv(target: Channel,drawBuffer: Int,value: Array[Int]): IO[Logger[F, ?], Unit] = log(gl.clearBufferiv(target, drawBuffer, value))(s"glClearBufferiv($target, $drawBuffer, ${value.toList})")
  private[gl] def clearBufferuiv(target: Channel,drawBuffer: Int,value: Array[Int]): IO[Logger[F, ?], Unit] = log(gl.clearBufferuiv(target, drawBuffer, value))(s"glclearBufferuiv($target, $drawBuffer, ${value.toList})")
  def clearColor(red: Float,green: Float,blue: Float,alpha: Float): IO[Logger[F, ?], Unit] = log(gl.clearColor(red, green, blue, alpha))(s"glClearColor($red, $green, $blue, $alpha)")
  def compileShader(shader: Int): IO[Logger[F, ?], Unit] = log(gl.compileShader(shader))(s"glCompileShader($shader)")
  def copyBufferSubData(read: BufferTarget,write: BufferTarget,readOffset: Int,writeOffset: Int,size: Int): IO[Logger[F, ?], Unit] = log(gl.copyBufferSubData(read, write, readOffset, writeOffset, size))(s"glCopyBufferSubData($read, $write, $readOffset, $writeOffset, $size)")
  def createProgram: IO[Logger[F, ?], Int] = log(gl.createProgram)("glCreateProgram")
  def createShader(`type`: ShaderType): IO[Logger[F, ?], Int] = log(gl.createShader(`type`))(s"glCreateShader(${`type`})")
  def deleteShader(shader: Int): IO[Logger[F, ?], Unit] = log(gl.deleteShader(shader))(s"glDeleteShader($shader)")
  def disable(cap: Capability): IO[Logger[F, ?], Unit] = log(gl.disable(cap))(s"glDisable($cap)")
  def drawArrays(mode: PrimitiveType,first: Int,count: Int): IO[Logger[F, ?], Unit] = log(gl.drawArrays(mode, first, count))(s"glDrawArrays($mode, $first, $count)")
  def drawBuffers(num: Int,buffers: Seq[ColorOutputTarget]): IO[Logger[F, ?], Unit] = log(gl.drawBuffers(num, buffers))(s"glDrawBuffers($num, $buffers)")
  def drawElements(mode: PrimitiveType,count: Int,`type`: IndexType,indices: java.nio.Buffer): IO[Logger[F, ?], Unit] = log(gl.drawElements(mode, count, `type`, indices))(s"glDrawElements($mode, $count, ${`type`}, $indices)")
  def drawElements(mode: PrimitiveType,count: Int,`type`: IndexType,offset: Int): IO[Logger[F, ?], Unit] = log(gl.drawElements(mode, count, `type`, offset))(s"glDrawElements($mode, $count, ${`type`}, offset)")
  def drawElementsInstanced(mode: PrimitiveType,count: Int,`type`: IndexType,ptr: java.nio.Buffer,primCount: Int): IO[Logger[F, ?], Unit] = log(gl.drawElementsInstanced(mode, count, `type`, ptr, primCount))(s"glDrawElementsInstanced($mode, $count, ${`type`}, ptr, $primCount)")
  def drawElementsInstanced(mode: PrimitiveType,count: Int,`type`: IndexType,offset: Int,primCount: Int): IO[Logger[F, ?], Unit] = log(gl.drawElementsInstanced(mode, count, `type`, offset, primCount))(s"glDrawElementsInstanced($mode, $count, ${`type`}, $offset, $primCount)")
  def enableVertexAttribArray(location: Int): IO[Logger[F, ?], Unit] = log(gl.enableVertexAttribArray(location))(s"glEnableVertexAttribArray($location)")
  def framebufferRenderbuffer(target: FramebufferTarget,attachment: FramebufferAttachment,renderbuffer: Int): IO[Logger[F, ?], Unit] = log(gl.framebufferRenderbuffer(target, attachment, renderbuffer))(s"glFramebufferRenderbuffer($target, $attachment, $renderbuffer)")
 
  def framebufferTexture2D(target: FramebufferTarget,attachment: FramebufferAttachment,texTarget: FramebufferTexTarget,texture: Int,level: Int): IO[Logger[F, ?], Unit] = log(gl.framebufferTexture2D(target, attachment, texTarget, texture, level))(s"glFramebufferTexture2D($target, $attachment, $texTarget, $texture, $level)")
   private[gl] def genBuffers(num: Int): IO[Logger[F, ?], Array[Int]] = log(gl.genBuffers(num))(s"glGenBuffers($num)")
   private[gl] def genFramebuffers(num: Int): IO[Logger[F, ?], Array[Int]] = log(gl.genFramebuffers(num))(s"glGenFramebuffers($num)")
   private[gl] def genRenderbuffers(num: Int): IO[Logger[F, ?], Array[Int]] = log(gl.genRenderbuffers(num))(s"glGenRenderbuffers($num)")
   private[gl] def genSamplers(num: Int): IO[Logger[F, ?], Array[Int]] = log(gl.genSamplers(num))(s"glGenSamplers($num)")
   private[gl] def genTextures(num: Int): IO[Logger[F, ?], Array[Int]] = log(gl.genTextures(num))(s"glGenTextures($num)")
   def getAttribLocation(program: Int,name: String): IO[Logger[F, ?], Int] = log(gl.getAttribLocation(program, name))(s"glGetAttribLocation($program, $name)")
   private[gl] def getShaderInfoLog(shader: Int,maxLength: Int): IO[Logger[F, ?], String] = log(gl.getShaderInfoLog(shader, maxLength))(s"glGetShaderInfoLog($shader, $maxLength)")
   private[gl] def getShaderiv(shader: Int,pname: ShaderParameter): IO[Logger[F, ?], Int] = log(gl.getShaderiv(shader, pname))(s"glGetShaderiv($shader, $pname)")
   def getUniformLocation(program: Int,name: String): IO[Logger[F, ?], Int] = log(gl.getUniformLocation(program, name))(s"glGetUniformLocation($program, $name)")
   def linkProgram(program: Int): IO[Logger[F, ?], Unit] = log(gl.linkProgram(program))(s"glLinkProgram($program)")
   def pixelStorei(name: PixelStoreParameter,value: Int): IO[Logger[F, ?], Unit] = log(gl.pixelStorei(name, value))(s"glPixelStorei($name, $value)")
   def readBuffer(src: DrawBuffer): IO[Logger[F, ?], Unit] = log(gl.readBuffer(src))(s"glReadBuffer($src)")
   def renderbufferStorage(format: RenderbufferInternalFormat,width: Int,height: Int): IO[Logger[F, ?], Unit] = log(gl.renderbufferStorage(format, width, height))(s"glRenderbufferStorage($format, $width, $height)")
   private[gl] def samplerParameteri(sampler: Int,name: SamplerParameter,value: Int): IO[Logger[F, ?], Unit] = log(gl.samplerParameteri(sampler, name, value))(s"glSamplerParameteri($sampler, $name, $value)")
   private[gl] def samplerParameteri(sampler: Int,name: SamplerParameter,value: IntValue): IO[Logger[F, ?], Unit] = log(gl.samplerParameteri(sampler, name, value))(s"glSamplerParameteri($sampler, $name, $value)")
  def shaderSource(shader: Int,count: Int,sources: Seq[String]): IO[Logger[F, ?], Unit] = log(gl.shaderSource(shader, count, sources))(s"glShaderSource($shader, $count, $sources)")
  def texImage2D(target: TextureTarget,level: Int,internalFormat: TextureInteralFormat,width: Int,height: Int,format: TextureFormat,`type`: TexturePixelType,data: java.nio.Buffer): IO[Logger[F, ?], Unit] = log(gl.texImage2D(target, level, internalFormat, width, height, format, `type`, data))(s"glTexImage2D($target, $level, $internalFormat, $width, $height, $format, ${`type`}, data)")
  private[gl] def texParameteri(target: TextureTarget,name: TextureParameter,value: IntValue): IO[Logger[F, ?], Unit] = log(gl.texParameteri(target, name, value))(s"glTexParameteri($target, $name, $value)")
  private[gl] def texParameteri(target: TextureTarget,name: TextureParameter,value: Int): IO[Logger[F, ?], Unit] = log(gl.texParameteri(target, name, value))(s"glTexParameteri($target, $name, $value)")
  private[gl] def uniform1f(location: Int,arg0: Float): IO[Logger[F, ?], Unit] = log(gl.uniform1f(location, arg0))(s"glUniform1f($location, $arg0)")
  private[gl] def uniform1fv(location: Int,count: Int,arr: Array[Float]): IO[Logger[F, ?], Unit] = log(gl.uniform1fv(location, count, arr))(s"glUniform1fv($location, $count, ${arr.toList})")
   private[gl] def uniform1i(location: Int,arg0: Int): IO[Logger[F, ?], Unit] = log(gl.uniform1i(location, arg0))(s"glUniform1i($location, $arg0)")
   private[gl] def uniform1iv(location: Int,count: Int,arr: Array[Int]): IO[Logger[F, ?], Unit] = log(gl.uniform1iv(location, count, arr))(s"glUniform1iv($location, $count, ${arr.toList})")
   private[gl] def uniform2f(location: Int,arg0: Float,arg1: Float): IO[Logger[F, ?], Unit] = log(gl.uniform2f(location, arg0, arg1))(s"glUniform2f($location, $arg0, $arg1)")
   private[gl] def uniform2fv(location: Int,count: Int,arr: Array[Float]): IO[Logger[F, ?], Unit] = log(gl.uniform2fv(location, count, arr))(s"glUniform2fv($location, $count, ${arr.toList})")
   private[gl] def uniform2i(location: Int,arg0: Int,arg1: Int): IO[Logger[F, ?], Unit] = log(gl.uniform2i(location, arg0, arg1))(s"glUniform2i($location, $arg0, $arg1)")
   private[gl] def uniform2iv(location: Int,count: Int,arr: Array[Int]): IO[Logger[F, ?], Unit] = log(gl.uniform2iv(location, count, arr))(s"glUniform2iv($location, $count, ${arr.toList})")
   private[gl] def uniform3f(location: Int,arg0: Float,arg1: Float,arg2: Float): IO[Logger[F, ?], Unit] = log(gl.uniform3f(location, arg0, arg1, arg2))(s"glUniform3f($location, $arg0, $arg1, $arg2)")
   private[gl] def uniform3fv(location: Int,count: Int,arr: Array[Float]): IO[Logger[F, ?], Unit] = log(gl.uniform3fv(location, count, arr))(s"glUniform3fv($location, $count, ${arr.toList})")
   private[gl] def uniform3i(location: Int,arg0: Int,arg1: Int,arg2: Int): IO[Logger[F, ?], Unit] = log(gl.uniform3i(location, arg0, arg1, arg2))(s"glUniform3i($location, $arg0, $arg1, $arg2)")
   private[gl] def uniform3iv(location: Int,count: Int,arr: Array[Int]): IO[Logger[F, ?], Unit] = log(gl.uniform3iv(location, count, arr))(s"glUniform3iv($location, $count, ${arr.toList})")
   private[gl] def uniform4f(location: Int,arg0: Float,arg1: Float,arg2: Float,arg3: Float): IO[Logger[F, ?], Unit] = log(gl.uniform4f(location, arg0, arg1, arg2, arg3))(s"glUniform4f($location, $arg0, $arg1, $arg2, $arg3)")
   private[gl] def uniform4fv(location: Int,count: Int,arr: Array[Float]): IO[Logger[F, ?], Unit]  = log(gl.uniform4fv(location, count, arr))(s"glUniform4fv($location, $count, ${arr.toList})")
   private[gl] def uniform4i(location: Int,arg0: Int,arg1: Int,arg2: Int,arg3: Int): IO[Logger[F, ?], Unit] = log(gl.uniform4i(location, arg0, arg1, arg2, arg3))(s"glUniform4i($location, $arg0, $arg1, $arg2, $arg3)")
   private[gl] def uniform4iv(location: Int,count: Int,arr: Array[Int]): IO[Logger[F, ?], Unit] = log(gl.uniform4iv(location, count, arr))(s"glUniform4iv($location, $count, ${arr.toList})")
  private[gl] def uniformMatrix2fv(location: Int,count: Int,transpose: Boolean,arg0: Array[Float]): IO[Logger[F, ?], Unit] = log(gl.uniformMatrix2fv(location, count, transpose, arg0))(s"glUniformMatrix2fv($location, $count, $transpose, $arg0)")
   private[gl] def uniformMatrix3fv(location: Int,count: Int,transpose: Boolean,arg0: Array[Float]): IO[Logger[F, ?], Unit] = log(gl.uniformMatrix3fv(location, count, transpose, arg0))(s"glUniformMatrix3fv($location, $count, $transpose, $arg0)")
   private[gl] def uniformMatrix4fv(location: Int,count: Int,transpose: Boolean,arg0: Array[Float]): IO[Logger[F, ?], Unit] = log(gl.uniformMatrix4fv(location, count, transpose, arg0))(s"glUniformMatrix4fv($location, $count, $transpose, $arg0)")
  def useProgram(program: Int): IO[Logger[F, ?], Unit] = log(gl.useProgram(program))(s"glUseProgram($program)")
  def vertexAttribPointer(location: Int,size: Int,`type`: VertexAttribType,normalized: Boolean,stride: Int,offset: Int): IO[Logger[F, ?], Unit] = log(gl.vertexAttribPointer(location, size, `type`, normalized, stride, offset))(s"glVertexAttribPointer($location, $size, ${`type`}, $normalized, $stride, $offset)")

}

