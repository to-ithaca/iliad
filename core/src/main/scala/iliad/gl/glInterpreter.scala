package iliad
package gl

import iliad.kernel.platform.GLES30Library

import cats._
import cats.data._
import cats.implicits._

import CatsExtra._

import java.nio.IntBuffer

import com.typesafe.scalalogging._

object GLInterpreter extends (OpenGL.Interpreter[OpenGL.NoEffect]) {

  private def genObject(r: Reader[GLES30Library, (Int, IntBuffer) => Unit],
                        num: Int): Reader[GLES30Library, Set[Int]] = r map {
    f =>
      val ptr = Buffer.capacity[Int](num)
      f(num, ptr)
      val arr = new Array[Int](num)
      ptr.get(arr, 0, num)
      arr.toSet
  }

  def apply[A](gl: OpenGL[A]): Reader[GLES30Library, A] = gl match {

    case GLGetError => Reader(_.glGetError())

    case GLGetShaderiv(shader, pname) =>
      Reader { lib =>
        val ptr = Buffer.capacity[Int](1)
        lib.glGetShaderiv(shader, pname.value, ptr)
        ptr.get()
      }

    case GLGetShaderInfoLog(shader, maxLength) =>
      Reader { lib =>
        val lenPtr = Buffer.capacity[Int](1)
        val logPtr = Buffer.capacity[Byte](maxLength)
        lib.glGetShaderInfoLog(shader, maxLength, lenPtr, logPtr)
        val len = lenPtr.get()
        val arr = new Array[Byte](len)
        logPtr.get(arr, 0, len)
        new String(arr)
      }
    case GLGetProgramiv(program, pname) =>
      Reader { lib =>
        val ptr = Buffer.capacity[Int](1)
        lib.glGetProgramiv(program, pname.value, ptr)
        ptr.get()
      }
    case GLGetProgramInfoLog(program, maxLength) =>
      Reader { lib =>
        val lenPtr = Buffer.capacity[Int](1)
        val logPtr = Buffer.capacity[Byte](maxLength)
        lib.glGetProgramInfoLog(program, maxLength, lenPtr, logPtr)
        val len = lenPtr.get()
        val arr = new Array[Byte](len)
        logPtr.get(arr, 0, len)
        new String(arr)
      }
    case GLCreateShader(t) => Reader(_.glCreateShader(t.value))
    case GLShaderSource(shader, sources) =>
      Reader(
          _.glShaderSource(shader,
                           sources.size,
                           sources.toArray,
                           sources.map(_.length).toArray))
    case GLCompileShader(shader) => Reader(_.glCompileShader(shader))
    case GLCreateProgram => Reader(_.glCreateProgram())
    case GLAttachShader(program, shader) =>
      Reader(_.glAttachShader(program, shader))
    case GLLinkProgram(program) => Reader(_.glLinkProgram(program))
    case GLGetAttribLocation(program, name) =>
      Reader(_.glGetAttribLocation(program, name))
    case GLGetUniformLocation(program, name) =>
      Reader(_.glGetUniformLocation(program, name))
    case GLGenBuffers(number) => genObject(Reader(_.glGenBuffers), number)
    case GLBindBuffer(target, buffer) =>
      Reader(_.glBindBuffer(target.value, buffer))
    case GLBufferData(target, size, data, usage) =>
      Reader(_.glBufferData(target.value, size, data, usage.value))
    case GLBufferSubData(target, offset, size, data) =>
      Reader(_.glBufferSubData(target.value, offset, size, data))
    case GLCopyBufferSubData(read, write, readOffset, writeOffset, size) =>
      Reader(
          _.glCopyBufferSubData(read.value,
                                write.value,
                                readOffset,
                                writeOffset,
                                size))
    case GLGenTextures(number) => genObject(Reader(_.glGenTextures), number)
    case GLBindTexture(texture) =>
      Reader(_.glBindTexture(GL_TEXTURE_2D.value, texture))
    case GLTexImage2D(internalFormat,
                      width,
                      height,
                      format,
                      pixelType,
                      data) =>
      Reader(
          _.glTexImage2D(GL_TEXTURE_2D.value,
                         0,
                         internalFormat.value,
                         width,
                         height,
                         0,
                         format.value,
                         pixelType.value,
                         data))
    case GLTexSubImage2D(xOffset,
                         yOffset,
                         width,
                         height,
                         format,
                         pixelType,
                         data) =>
      Reader(
          _.glTexSubImage2D(GL_TEXTURE_2D.value,
                            0,
                            xOffset,
                            yOffset,
                            width,
                            height,
                            format.value,
                            pixelType.value,
                            data))

    case GLGenRenderbuffers(number) =>
      genObject(Reader(_.glGenRenderbuffers), number)
    case GLBindRenderbuffer(renderbuffer) =>
      Reader(_.glBindRenderbuffer(GL_RENDERBUFFER.value, renderbuffer))
    case GLRenderbufferStorage(format, width, height) =>
      Reader(
          _.glRenderbufferStorage(GL_RENDERBUFFER.value,
                                  format.value,
                                  width,
                                  height))

    case GLGenFramebuffers(number) =>
      genObject(Reader(_.glGenFramebuffers), number)
    case GLBindFramebuffer(target, framebuffer) =>
      Reader(_.glBindFramebuffer(target.value, framebuffer))
    case GLFramebufferRenderbuffer(channel, renderbuffer) =>
      Reader(
          _.glFramebufferRenderbuffer(GL_FRAMEBUFFER.value,
                                      channel.value,
                                      GL_RENDERBUFFER.value,
                                      renderbuffer))
    case GLFramebufferTexture2D(channel, texture) =>
      Reader(
          _.glFramebufferTexture2D(GL_FRAMEBUFFER.value,
                                   channel.value,
                                   GL_TEXTURE_2D.value,
                                   texture,
                                   0))
    case GLDrawBuffers(bufs) =>
      val b = Buffer(bufs.map(_.value): _*)
      Reader(_.glDrawBuffers(bufs.size, b))

    case GLGenSamplers(number) => genObject(Reader(_.glGenSamplers), number)
    case GLSamplerParameteri(sampler, name, value) =>
      Reader(_.glSamplerParameteri(sampler, name.value, value.value))

    case GLEnable(cap) => Reader(_.glEnable(cap.value))
    case GLDisable(cap) => Reader(_.glDisable(cap.value))
    case GLColorMask(r, g, b, a) => Reader(_.glColorMask(r, g, b, a))
    case GLUseProgram(program) => Reader(_.glUseProgram(program))
    case GLEnableVertexAttribArray(location) =>
      Reader(_.glEnableVertexAttribArray(location))
    case GLVertexAttribPointer(location,
                               size,
                               t,
                               normalized,
                               stride,
                               offset) =>
      Reader(
          _.glVertexAttribPointer(location,
                                  size,
                                  t.value,
                                  normalized,
                                  stride,
                                  offset))
    case GLVertexAttribIPointer(location, size, t, stride, offset) =>
      Reader(_.glVertexAttribIPointer(location, size, t.value, stride, offset))
    case GLActiveTexture(unit) => Reader(_.glActiveTexture(unit.value))
    case GLBindSampler(unit, sampler) =>
      Reader(_.glBindSampler(Bounded.indexOf(unit), sampler))
    case GLUniform1i(location, value) => Reader(_.glUniform1i(location, value))
    case GLUniform1f(location, value) => Reader(_.glUniform1f(location, value))
    case GLUniform2i(location, value) =>
      Reader(_.glUniform2i(location, value(0), value(1)))
    case GLUniform2f(location, value) =>
      Reader(_.glUniform2f(location, value(0), value(1)))
    case GLUniform3i(location, value) =>
      Reader(_.glUniform3i(location, value(0), value(1), value(2)))
    case GLUniform3f(location, value) =>
      Reader(_.glUniform3f(location, value(0), value(1), value(2)))
    case GLUniform4i(location, value) =>
      Reader(_.glUniform4i(location, value(0), value(1), value(2), value(3)))
    case GLUniform4f(location, value) =>
      Reader(_.glUniform4f(location, value(0), value(1), value(2), value(3)))

    case GLUniformMatrix2f(location, value) =>
      Reader(_.glUniformMatrix2fv(location, 1, true, value.toArray))
    case GLUniformMatrix3f(location, value) =>
      Reader(_.glUniformMatrix3fv(location, 1, true, value.toArray))
    case GLUniformMatrix4f(location, value) =>
      Reader(_.glUniformMatrix4fv(location, 1, true, value.toArray))

    case GLDrawElements(mode, count, t, offset) =>
      Reader(_.glDrawElements(mode.value, count, t.value, offset))
    case GLClear(bitMask) =>
      Reader { r =>
        r.glClearColor(1f, 0f, 0f, 1f)
        r.glClear(bitMask.value)
      }
  }
}

final class GLLogInterpreter[F[_]: Monad](
    interpret: OpenGL.Interpreter[OpenGL.Effect[F, ?]])
    extends (OpenGL.Interpreter[OpenGL.LogEffect[F, ?]]) {

  private val logAfter: F ~> OpenGL.Logger[F, ?] =
    new (F ~> OpenGL.Logger[F, ?]) {
      def apply[A](fa: F[A]): OpenGL.Logger[F, A] =
        WriterT(fa.map(v => (List(s"returned $v"), v)))
    }

  def logBefore(s: String): OpenGL.LogEffect[F, Unit] =
    ReaderT(_ => WriterT.tell[F, List[String]](List(s)))

  def apply[A](gl: OpenGL[A]): OpenGL.LogEffect[F, A] = {
    logBefore(s"calling $gl") >>
    interpret(gl).transform(logAfter)
  }
}

final class GLEffectfulLogInterpreter[F[_]: Monad](
    interpret: OpenGL.Interpreter[OpenGL.Effect[F, ?]])
    extends (OpenGL.Interpreter[OpenGL.Effect[F, ?]])
    with LazyLogging {

  private val logAfter: F ~> OpenGL.Logger[F, ?] =
    new (F ~> OpenGL.Logger[F, ?]) {
      def apply[A](fa: F[A]): OpenGL.Logger[F, A] =
        WriterT(fa.map(v => (List(s"returned $v"), v)))
    }

  def logBefore(s: String): OpenGL.LogEffect[F, Unit] =
    ReaderT(_ => WriterT.tell[F, List[String]](List(s)))

  def apply[A](gl: OpenGL[A]): OpenGL.Effect[F, A] = {
    logger.debug(s"calling $gl")
    interpret(gl).map { v =>
      logger.debug(s"returned $v")
      v
    }
  }
}

final class GLDebugInterpreter[F[_]: MonadRec](
    interpret: OpenGL.Interpreter[OpenGL.Effect[F, ?]])
    extends (OpenGL.Interpreter[OpenGL.DebugEffect[F, ?]]) {

  private val lift: F ~> XorT[F, GLError, ?] = new (F ~> XorT[F, GLError, ?]) {
    def apply[A](fa: F[A]): XorT[F, GLError, A] = XorT.right(fa)
  }

  private val _errorCodes: Set[ErrorCode] = SealedEnum.values[ErrorCode]

  private def onError(method: String)(value: Int): GLError Xor Unit =
    if (value == GL_NO_ERROR.value) ().right
    else
      _errorCodes.find(_.value == value) match {
        case Some(code) =>
          CallFailedError(method, code).left
        case None =>
          CallFailedUnknownError(method, value).left
      }

  private def debug(method: String): OpenGL.DebugEffect[F, Unit] =
    OpenGL.getError
      .foldMap(interpret)
      .transform(lift)
      .mapF(_.subflatMap(onError(method)))

  private def onCompileError(
      log: Option[String]): ShaderCompileError Xor Unit =
    log.map(l => ShaderCompileError(l)).toLeftXor(())

  private def onLinkError(log: Option[String]): ProgramLinkError Xor Unit =
    log.map(l => ProgramLinkError(l)).toLeftXor(())

  private def shaderLog(shader: Int): OpenGL.DebugEffect[F, Unit] =
    OpenGL
      .getCompileError(shader)
      .foldMap(interpret)
      .transform(lift)
      .mapF(_.subflatMap(onCompileError))

  private def programLog(program: Int): OpenGL.DebugEffect[F, Unit] =
    OpenGL
      .getLinkError(program)
      .foldMap(interpret)
      .transform(lift)
      .mapF(_.subflatMap(onLinkError))

  def apply[A](gl: OpenGL[A]): OpenGL.DebugEffect[F, A] = gl match {

    case GLCompileShader(shader) =>
      for {
        _ <- interpret(gl).transform(lift)
        _ <- shaderLog(shader)
        _ <- debug(gl.toString)
      } yield ()
    case GLLinkProgram(program) =>
      for {
        _ <- interpret(gl).transform(lift)
        _ <- programLog(program)
        _ <- debug(gl.toString)
      } yield ()

    case _ =>
      for {
        a <- interpret(gl).transform(lift)
        _ <- debug(gl.toString)
      } yield a
  }
}
