package iliad
package kernel

import cats._
import cats.data._
import cats.implicits._

import GL._

private[kernel] final class GLLogger[F[_]](config: LoggerConfig, gl: GL[F])(
    implicit M: Monad[F], val MM: Monad[LogEffect[F, ?]])
    extends GL[LogEffect[F, ?]] {
  private def lift[A](io: IO[F, A]): IO[LogEffect[F, ?], A] =
    io.mapF(_.transformF(_.liftT[LogEffect]))
  private def log[A](io: IO[F, A])(s: String): IO[LogEffect[F, ?], A] =
    lift(io).mapF(_.transformF(_.mapWritten(_ => List(s))))

  def viewport(rect: Rect[Int]): IO[LogEffect[F, ?], Unit] =
    log(gl.viewport(rect))(s"glViewport($rect)")
  def flush: IO[LogEffect[F, ?], Unit] = log(gl.flush)("glFlush")
  def enable(cap: Capability): IO[LogEffect[F, ?], Unit] =
    log(gl.enable(cap))(s"glEnable($cap)")
  def colorMask(red: Boolean,
                green: Boolean,
                blue: Boolean,
                alpha: Boolean): IO[LogEffect[F, ?], Unit] =
    log(gl.colorMask(red, green, blue, alpha))(
        s"glColorMask($red, $green, $blue, $alpha)")
  def getError: IO[LogEffect[F, ?], Option[Int Xor ErrorCode]] =
    log(gl.getError)("glGetError")

  def activeTexture(texture: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.activeTexture(texture))(s"glActiveTexture($texture)")
  def attachShader(program: Int, shader: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.attachShader(program, shader))(s"glAttachShader($program, $shader)")

  def bindBuffer(
      target: BufferTarget, buffer: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.bindBuffer(target, buffer))(s"glBindBuffer($target, $buffer)")
  def bindFramebuffer(
      target: FramebufferTarget, framebuffer: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.bindFramebuffer(target, framebuffer))(
        s"glBindFramebuffer($target, $framebuffer)")
  def bindRenderbuffer(renderbuffer: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.bindRenderbuffer(renderbuffer))(
        s"glBindRenderbuffer($renderbuffer)")
  def bindTexture(
      target: TextureTarget, texture: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.bindTexture(target, texture))(s"glBindTexture($target, $texture)")

  def bufferData(target: BufferTarget,
                 size: Int,
                 data: java.nio.Buffer,
                 usage: BufferUsage): IO[LogEffect[F, ?], Unit] =
    log(gl.bufferData(target, size, data, usage))(
        s"glBufferData($target, $size, data, $usage)")
  def bufferSubData(target: BufferTarget,
                    offset: Int,
                    size: Int,
                    data: java.nio.Buffer): IO[LogEffect[F, ?], Unit] =
    log(gl.bufferSubData(target, offset, size, data))(
        s"glBufferSubData($target, $offset, $size, data)")
  def checkFramebufferStatus(
      target: FramebufferTarget): IO[LogEffect[F, ?], FramebufferStatus] =
    log(gl.checkFramebufferStatus(target))(
        s"glCheckFramebufferStatus($target)")
  def clear(bitMask: ChannelBitMask): IO[LogEffect[F, ?], Unit] =
    log(gl.clear(bitMask))(s"glClear($bitMask)")

  private[kernel] def clearBufferfv(
      target: Channel,
      drawBuffer: Int,
      value: Array[Float]): IO[LogEffect[F, ?], Unit] =
    log(gl.clearBufferfv(target, drawBuffer, value))(
        s"glClearBufferfv($target, $drawBuffer, ${value.toList})")
  private[kernel] def clearBufferiv(
      target: Channel,
      drawBuffer: Int,
      value: Array[Int]): IO[LogEffect[F, ?], Unit] =
    log(gl.clearBufferiv(target, drawBuffer, value))(
        s"glClearBufferiv($target, $drawBuffer, ${value.toList})")
  private[kernel] def clearBufferuiv(
      target: Channel,
      drawBuffer: Int,
      value: Array[Int]): IO[LogEffect[F, ?], Unit] =
    log(gl.clearBufferuiv(target, drawBuffer, value))(
        s"glclearBufferuiv($target, $drawBuffer, ${value.toList})")
  def clearColor(red: Float,
                 green: Float,
                 blue: Float,
                 alpha: Float): IO[LogEffect[F, ?], Unit] =
    log(gl.clearColor(red, green, blue, alpha))(
        s"glClearColor($red, $green, $blue, $alpha)")
  def compileShader(shader: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.compileShader(shader))(s"glCompileShader($shader)")
  def copyBufferSubData(read: BufferTarget,
                        write: BufferTarget,
                        readOffset: Int,
                        writeOffset: Int,
                        size: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.copyBufferSubData(read, write, readOffset, writeOffset, size))(
        s"glCopyBufferSubData($read, $write, $readOffset, $writeOffset, $size)")
  def createProgram: IO[LogEffect[F, ?], Int] =
    log(gl.createProgram)("glCreateProgram")
  def createShader(`type`: ShaderType): IO[LogEffect[F, ?], Int] =
    log(gl.createShader(`type`))(s"glCreateShader(${`type`})")

  def disable(cap: Capability): IO[LogEffect[F, ?], Unit] =
    log(gl.disable(cap))(s"glDisable($cap)")

  def drawBuffers(
      num: Int, buffers: Seq[ColorOutputTarget]): IO[LogEffect[F, ?], Unit] =
    log(gl.drawBuffers(num, buffers))(s"glDrawBuffers($num, $buffers)")

  def drawElements(mode: PrimitiveType,
                   count: Int,
                   `type`: IndexType,
                   offset: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.drawElements(mode, count, `type`, offset))(
        s"glDrawElements($mode, $count, ${`type`}, offset)")

  def drawElementsInstanced(mode: PrimitiveType,
                            count: Int,
                            `type`: IndexType,
                            offset: Int,
                            primCount: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.drawElementsInstanced(mode, count, `type`, offset, primCount))(
        s"glDrawElementsInstanced($mode, $count, ${`type`}, $offset, $primCount)")
  def enableVertexAttribArray(location: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.enableVertexAttribArray(location))(
        s"glEnableVertexAttribArray($location)")
  def framebufferRenderbuffer(target: FramebufferTarget,
                              attachment: FramebufferAttachment,
                              renderbuffer: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.framebufferRenderbuffer(target, attachment, renderbuffer))(
        s"glFramebufferRenderbuffer($target, $attachment, $renderbuffer)")

  def framebufferTexture2D(target: FramebufferTarget,
                           attachment: FramebufferAttachment,
                           texTarget: FramebufferTexTarget,
                           texture: Int,
                           level: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.framebufferTexture2D(
            target, attachment, texTarget, texture, level))(
        s"glFramebufferTexture2D($target, $attachment, $texTarget, $texture, $level)")
  private[kernel] def genBuffers(num: Int): IO[LogEffect[F, ?], Array[Int]] =
    log(gl.genBuffers(num))(s"glGenBuffers($num)")
  private[kernel] def genFramebuffers(
      num: Int): IO[LogEffect[F, ?], Array[Int]] =
    log(gl.genFramebuffers(num))(s"glGenFramebuffers($num)")
  private[kernel] def genRenderbuffers(
      num: Int): IO[LogEffect[F, ?], Array[Int]] =
    log(gl.genRenderbuffers(num))(s"glGenRenderbuffers($num)")
  private[kernel] def genSamplers(num: Int): IO[LogEffect[F, ?], Array[Int]] =
    log(gl.genSamplers(num))(s"glGenSamplers($num)")
  private[kernel] def genTextures(num: Int): IO[LogEffect[F, ?], Array[Int]] =
    log(gl.genTextures(num))(s"glGenTextures($num)")
  def getAttribLocation(program: Int, name: String): IO[LogEffect[F, ?], Int] =
    log(gl.getAttribLocation(program, name))(
        s"glGetAttribLocation($program, $name)")
  private[kernel] def getShaderInfoLog(
      shader: Int, maxLength: Int): IO[LogEffect[F, ?], String] =
    log(gl.getShaderInfoLog(shader, maxLength))(
        s"glGetShaderInfoLog($shader, $maxLength)")
  private[kernel] def getShaderiv(
      shader: Int, pname: ShaderParameter): IO[LogEffect[F, ?], Int] =
    log(gl.getShaderiv(shader, pname))(s"glGetShaderiv($shader, $pname)")
  def getUniformLocation(
      program: Int, name: String): IO[LogEffect[F, ?], Int] =
    log(gl.getUniformLocation(program, name))(
        s"glGetUniformLocation($program, $name)")
  def linkProgram(program: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.linkProgram(program))(s"glLinkProgram($program)")

  def renderbufferStorage(format: RenderbufferInternalFormat,
                          width: Int,
                          height: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.renderbufferStorage(format, width, height))(
        s"glRenderbufferStorage($format, $width, $height)")
  private[kernel] def samplerParameteri(
      sampler: Int,
      name: SamplerParameter,
      value: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.samplerParameteri(sampler, name, value))(
        s"glSamplerParameteri($sampler, $name, $value)")
  override def samplerParameteri(
      sampler: Int,
      name: SamplerParameter,
      value: IntConstant): IO[LogEffect[F, ?], Unit] =
    log(gl.samplerParameteri(sampler, name, value))(
        s"glSamplerParameteri($sampler, $name, $value)")
  def shaderSource(shader: Int,
                   count: Int,
                   sources: Seq[String]): IO[LogEffect[F, ?], Unit] =
    log(gl.shaderSource(shader, count, sources))(
        s"glShaderSource($shader, $count, $sources)")
  def texImage2D(target: TextureTarget,
                 level: Int,
                 internalFormat: TextureInternalFormat,
                 width: Int,
                 height: Int,
                 format: TextureFormat,
                 `type`: TexturePixelType,
                 data: java.nio.Buffer): IO[LogEffect[F, ?], Unit] =
    log(
        gl.texImage2D(target,
                      level,
                      internalFormat,
                      width,
                      height,
                      format,
                      `type`,
                      data))(
        s"glTexImage2D($target, $level, $internalFormat, $width, $height, $format, ${`type`}, data)")

  private[kernel] def uniform1f(
      location: Int, arg0: Float): IO[LogEffect[F, ?], Unit] =
    log(gl.uniform1f(location, arg0))(s"glUniform1f($location, $arg0)")

  private[kernel] def uniform1i(
      location: Int, arg0: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.uniform1i(location, arg0))(s"glUniform1i($location, $arg0)")

  private[kernel] def uniform2f(
      location: Int, arg0: Float, arg1: Float): IO[LogEffect[F, ?], Unit] =
    log(gl.uniform2f(location, arg0, arg1))(
        s"glUniform2f($location, $arg0, $arg1)")

  private[kernel] def uniform2i(
      location: Int, arg0: Int, arg1: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.uniform2i(location, arg0, arg1))(
        s"glUniform2i($location, $arg0, $arg1)")

  private[kernel] def uniform3f(location: Int,
                                arg0: Float,
                                arg1: Float,
                                arg2: Float): IO[LogEffect[F, ?], Unit] =
    log(gl.uniform3f(location, arg0, arg1, arg2))(
        s"glUniform3f($location, $arg0, $arg1, $arg2)")

  private[kernel] def uniform3i(location: Int,
                                arg0: Int,
                                arg1: Int,
                                arg2: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.uniform3i(location, arg0, arg1, arg2))(
        s"glUniform3i($location, $arg0, $arg1, $arg2)")

  private[kernel] def uniform4f(location: Int,
                                arg0: Float,
                                arg1: Float,
                                arg2: Float,
                                arg3: Float): IO[LogEffect[F, ?], Unit] =
    log(gl.uniform4f(location, arg0, arg1, arg2, arg3))(
        s"glUniform4f($location, $arg0, $arg1, $arg2, $arg3)")

  private[kernel] def uniform4i(location: Int,
                                arg0: Int,
                                arg1: Int,
                                arg2: Int,
                                arg3: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.uniform4i(location, arg0, arg1, arg2, arg3))(
        s"glUniform4i($location, $arg0, $arg1, $arg2, $arg3)")

  def useProgram(program: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.useProgram(program))(s"glUseProgram($program)")

  def vertexAttribPointer(location: Int,
                          size: Int,
                          `type`: VertexAttribType,
                          normalized: Boolean,
                          stride: Int,
                          offset: Int): IO[LogEffect[F, ?], Unit] =
    log(gl.vertexAttribPointer(
            location, size, `type`, normalized, stride, offset))(
        s"glVertexAttribPointer($location, $size, ${`type`}, $normalized, $stride, $offset)")
}
