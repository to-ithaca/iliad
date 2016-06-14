package iliad
package kernel

import iliad.kernel.platform._

import java.nio.{IntBuffer, Buffer => NBuffer}

import cats._
import cats.implicits._
import cats.data._

import GL._

private[kernel] object GLRunner extends GL[Id] {

  private def lift[A](f: GLES30Library => A): IO[Id, A] =
    ReaderT(lib => StateT.pure(f(lib)))

  def createShader(`type`: ShaderType): IO[Id, Int] =
    lift(_.glCreateShader(`type`.value))
  def shaderSource(
      shader: Int, count: Int, sources: Seq[String]): IO[Id, Unit] = {
    val ls = sources.map(_.size).toArray
    lift(_.glShaderSource(shader, count, sources.toArray, ls))
  }
  def compileShader(shader: Int): IO[Id, Unit] =
    lift(_.glCompileShader(shader))

  def getShaderiv(shader: Int, pname: ShaderParameter): IO[Id, Int] = lift {
    gl =>
      val ptr = Buffer.capacity[Int](1)
      gl.glGetShaderiv(shader, pname.value, ptr)
      ptr.get()
  }

  def getShaderInfoLog(shader: Int, maxLength: Int): IO[Id, String] = lift {
    gl =>
      val lenPtr = Buffer.capacity[Int](1)
      val logPtr = Buffer.capacity[Byte](maxLength)
      gl.glGetShaderInfoLog(shader, maxLength, lenPtr, logPtr)
      val len = lenPtr.get()
      val arr = new Array[Byte](len)
      logPtr.get(arr, 0, len)
      new String(arr)
  }

  def createProgram: IO[Id, Int] = lift(_.glCreateProgram())
  def attachShader(program: Int, shader: Int): IO[Id, Unit] =
    lift(_.glAttachShader(program, shader))
  def linkProgram(program: Int): IO[Id, Unit] = lift(_.glLinkProgram(program))
  def useProgram(program: Int): IO[Id, Unit] = lift(_.glUseProgram(program))

  def getAttribLocation(program: Int, name: String): IO[Id, Int] =
    lift(_.glGetAttribLocation(program, name))
  def getUniformLocation(program: Int, name: String): IO[Id, Int] =
    lift(_.glGetUniformLocation(program, name))

  def genSamplers(num: Int): IO[Id, Array[Int]] =
    genObject(_.glGenSamplers, num)
  def samplerParameteri(
      sampler: Int, name: SamplerParameter, value: Int): IO[Id, Unit] =
    lift(_.glSamplerParameteri(sampler, name.value, value))

  def genRenderbuffers(num: Int): IO[Id, Array[Int]] =
    genObject(_.glGenRenderbuffers, num)
  def bindRenderbuffer(renderbuffer: Int): IO[Id, Unit] =
    lift(_.glBindBuffer(GL_RENDERBUFFER.value, renderbuffer))
  def renderbufferStorage(format: RenderbufferInternalFormat,
                          width: Int,
                          height: Int): IO[Id, Unit] =
    lift(
        _.glRenderbufferStorage(
            GL_RENDERBUFFER.value, format.value, width, height))

  def bindTexture(target: TextureTarget, texture: Int): IO[Id, Unit] =
    lift(_.glBindTexture(target.value, texture))
  def genTextures(num: Int): IO[Id, Array[Int]] =
    genObject(_.glGenTextures, num)

  def texImage2D(target: TextureTarget,
                 level: Int,
                 internalFormat: TextureInternalFormat,
                 width: Int,
                 height: Int,
                 format: TextureFormat,
                 `type`: TexturePixelType,
                 data: NBuffer): IO[Id, Unit] =
    lift(
        _.glTexImage2D(target.value,
                       level,
                       internalFormat.value,
                       width,
                       height,
                       0,
                       internalFormat.value,
                       `type`.value,
                       data))

  def activeTexture(texture: Int): IO[Id, Unit] =
    lift(_.glActiveTexture(texture))

  def genFramebuffers(num: Int): IO[Id, Array[Int]] =
    genObject(_.glGenFramebuffers, num)
  def bindFramebuffer(
      target: FramebufferTarget, framebuffer: Int): IO[Id, Unit] =
    lift(_.glBindFramebuffer(target.value, framebuffer))
  def framebufferRenderbuffer(target: FramebufferTarget,
                              attachment: FramebufferAttachment,
                              renderbuffer: Int): IO[Id, Unit] =
    lift(
        _.glFramebufferRenderbuffer(target.value,
                                    attachment.value,
                                    GL_RENDERBUFFER.value,
                                    renderbuffer))
  def checkFramebufferStatus(
      target: FramebufferTarget): IO[Id, FramebufferStatus] =
    lift(_.glCheckFramebufferStatus(target.value)).map(c =>
          SealedEnum.values[FramebufferStatus].find(_.value == c).get)
  def framebufferTexture2D(target: FramebufferTarget,
                           attachment: FramebufferAttachment,
                           texTarget: FramebufferTexTarget,
                           texture: Int,
                           level: Int): IO[Id, Unit] =
    lift(
        _.glFramebufferTexture2D(
            target.value, attachment.value, texTarget.value, texture, level))

  def viewport(rect: Rect[Int]): IO[Id, Unit] =
    lift(_.glViewport(rect.topLeft.x, rect.topLeft.y, rect.width, rect.height))
  def enable(cap: Capability): IO[Id, Unit] = lift(_.glEnable(cap.value))
  def disable(cap: Capability): IO[Id, Unit] = lift(_.glDisable(cap.value))
  def flush: IO[Id, Unit] = lift(_.glFlush())
  def clear(bitMask: ChannelBitMask): IO[Id, Unit] =
    lift(_.glClear(bitMask.value))
  def clearColor(
      red: Float, green: Float, blue: Float, alpha: Float): IO[Id, Unit] =
    lift(_.glClearColor(red, green, blue, alpha))
  def colorMask(red: Boolean,
                green: Boolean,
                blue: Boolean,
                alpha: Boolean): IO[Id, Unit] =
    lift(_.glColorMask(red, green, blue, alpha))
  def getError: IO[Id, Option[Int Xor ErrorCode]] =
    lift(
        _.glGetError() match {
      case v if v == GL_NO_ERROR.value => None
      case v =>
        Some(SealedEnum.values[ErrorCode].find(_.value == v).toRightXor(v))
    })

  //nasty but the signature of gen means the alternatives are worse!
  private def genObject(f: GLES30Library => (Int, IntBuffer) => Unit,
                        num: Int): IO[Id, Array[Int]] = lift { gl =>
    val ptr = Buffer.capacity[Int](num)
    f(gl)(num, ptr)
    val arr = new Array[Int](num)
    ptr.get(arr, 0, num)
    arr
  }

  def genBuffers(num: Int): IO[Id, Array[Int]] = genObject(_.glGenBuffers, num)
  def bindBuffer(target: BufferTarget, buffer: Int): IO[Id, Unit] =
    lift(_.glBindBuffer(target.value, buffer))
  def bufferData(target: BufferTarget,
                 size: Int,
                 data: NBuffer,
                 usage: BufferUsage): IO[Id, Unit] =
    lift(_.glBufferData(target.value, size, data, usage.value))
  def bufferSubData(target: BufferTarget,
                    offset: Int,
                    size: Int,
                    data: NBuffer): IO[Id, Unit] =
    lift(_.glBufferSubData(target.value, offset, size, data))
  def enableVertexAttribArray(location: Int): IO[Id, Unit] =
    lift(_.glEnableVertexAttribArray(location))
  def vertexAttribPointer(location: Int,
                          size: Int,
                          `type`: VertexAttribType,
                          normalized: Boolean,
                          stride: Int,
                          offset: Int): IO[Id, Unit] =
    lift(
        _.glVertexAttribPointer(
            location, size, `type`.value, normalized, stride, offset))

  def drawElements(mode: PrimitiveType,
                   count: Int,
                   `type`: IndexType,
                   offset: Int): IO[Id, Unit] =
    lift(_.glDrawElements(mode.value, count, `type`.value, offset))

  def uniform1i(location: Int, arg0: Int): IO[Id, Unit] =
    lift(_.glUniform1i(location, arg0))
  def uniform1f(location: Int, arg0: Float): IO[Id, Unit] =
    lift(_.glUniform1f(location, arg0))

  def uniform2i(location: Int, arg0: Int, arg1: Int): IO[Id, Unit] =
    lift(_.glUniform2i(location, arg0, arg0))
  def uniform2f(location: Int, arg0: Float, arg1: Float): IO[Id, Unit] =
    lift(_.glUniform2f(location, arg0, arg1))
  def uniform3i(location: Int, arg0: Int, arg1: Int, arg2: Int): IO[Id, Unit] =
    lift(_.glUniform3i(location, arg0, arg1, arg2))
  def uniform3f(
      location: Int, arg0: Float, arg1: Float, arg2: Float): IO[Id, Unit] =
    lift(_.glUniform3f(location, arg0, arg1, arg2))
  def uniform4i(location: Int,
                arg0: Int,
                arg1: Int,
                arg2: Int,
                arg3: Int): IO[Id, Unit] =
    lift(_.glUniform4i(location, arg0, arg1, arg2, arg3))
  def uniform4f(location: Int,
                arg0: Float,
                arg1: Float,
                arg2: Float,
                arg3: Float): IO[Id, Unit] =
    lift(_.glUniform4f(location, arg0, arg1, arg2, arg3))

  def clearBufferfv(target: Channel,
                    drawBuffer: Int,
                    value: Array[Float]): IO[cats.Id, Unit] =
    lift(_.glClearBufferfv(target.value, drawBuffer, value))
  def clearBufferiv(
      target: Channel, drawBuffer: Int, value: Array[Int]): IO[cats.Id, Unit] =
    lift(_.glClearBufferiv(target.value, drawBuffer, value))
  def clearBufferuiv(
      target: Channel, drawBuffer: Int, value: Array[Int]): IO[cats.Id, Unit] =
    lift(_.glClearBufferuiv(target.value, drawBuffer, value))
  def copyBufferSubData(read: BufferTarget,
                        write: BufferTarget,
                        readOffset: Int,
                        writeOffset: Int,
                        size: Int): IO[cats.Id, Unit] =
    lift(
        _.glCopyBufferSubData(
            read.value, write.value, readOffset, writeOffset, size))
  def drawBuffers(
      num: Int, buffers: Seq[ColorOutputTarget]): IO[cats.Id, Unit] =
    lift(_.glDrawBuffers(num, Buffer(buffers.map(_.value): _*)))

  def drawElementsInstanced(mode: PrimitiveType,
                            count: Int,
                            `type`: IndexType,
                            offset: Int,
                            primCount: Int): IO[cats.Id, Unit] =
    lift(
        _.glDrawElementsInstanced(
            mode.value, count, `type`.value, offset, primCount))
}
