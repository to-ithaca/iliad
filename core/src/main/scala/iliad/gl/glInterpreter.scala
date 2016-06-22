package iliad
package gl

import iliad.kernel.platform.GLES30Library

import cats._
import cats.data._
import cats.implicits._

import java.nio.IntBuffer

object GLInterpreter extends (GL.Interpreter[GL.NoEffect]) {

  private def genObject(r: Reader[GLES30Library, (Int, IntBuffer) => Unit],
                        num: Int): Reader[GLES30Library, List[Int]] = r map {
    f =>
      val ptr = Buffer.capacity[Int](num)
      f(num, ptr)
      val arr = new Array[Int](num)
      ptr.get(arr, 0, num)
      arr.toList
  }

  def apply[A](gl: GL[A]): Reader[GLES30Library, A] = gl match {

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

    case GLGenBuffers(number) => genObject(Reader(_.glGenBuffers), number)
    case GLBindBuffer(target, buffer) =>
      Reader(_.glBindBuffer(target.value, buffer))
    case GLBufferData(target, size, data, usage) =>
      Reader(_.glBufferData(target.value, size, data, usage.value))
    case GLBufferSubData(target, offset, size, data) =>
      Reader(_.glBufferSubData(target.value, offset, size, data))
    case GLCopyBufferSubData(read, write, readOffset, writeOffset, size) =>
      Reader(_.glCopyBufferSubData(
              read.value, write.value, readOffset, writeOffset, size))

    case GLBindFramebuffer(target, framebuffer) =>
      Reader(_.glBindFramebuffer(target.value, framebuffer))
    case GLEnable(cap) => Reader(_.glEnable(cap.value))
    case GLDisable(cap) => Reader(_.glDisable(cap.value))
    case GLColorMask(r, g, b, a) => Reader(_.glColorMask(r, g, b, a))
    case GLUseProgram(program) => Reader(_.glUseProgram(program))
    case GLEnableVertexAttribArray(location) =>
      Reader(_.glEnableVertexAttribArray(location))
    case GLVertexAttribPointer(
        location, size, t, normalized, stride, offset) =>
      Reader(
          _.glVertexAttribPointer(
              location, size, t.value, normalized, stride, offset))
    case GLDrawElements(mode, count, t, offset) =>
      Reader(_.glDrawElements(mode.value, count, t.value, offset))
    case GLClear(bitMask) => Reader(_.glClear(bitMask.value))
  }
}

final class GLLogInterpreter[F[_]: Monad](
    interpret: GL.Interpreter[GL.Effect[F, ?]])
    extends (GL.Interpreter[GL.LogEffect[F, ?]]) {

  private val logAfter: F ~> GL.Logger[F, ?] = new (F ~> GL.Logger[F, ?]) {
    def apply[A](fa: F[A]): GL.Logger[F, A] =
      WriterT(fa.map(v => (List(s"returned $v"), v)))
  }

  def logBefore(s: String): GL.LogEffect[F, Unit] =
    ReaderT(_ => WriterT.tell[F, List[String]](List(s)))

  def apply[A](gl: GL[A]): GL.LogEffect[F, A] = {
    logBefore(s"calling $gl") >>
    interpret(gl).transform(logAfter)
  }
}

final class GLDebugInterpreter[F[_]: Monad](
    interpret: GL.Interpreter[GL.Effect[F, ?]])
    extends (GL.Interpreter[GL.DebugEffect[F, ?]]) {

  private val lift: F ~> XorT[F, String, ?] = new (F ~> XorT[F, String, ?]) {
    def apply[A](fa: F[A]): XorT[F, String, A] = XorT.right(fa)
  }

  private val _errorCodes: Set[ErrorCode] = SealedEnum.values[ErrorCode]

  private def onError(method: String)(value: Int): String Xor Unit =
    if (value == GL_NO_ERROR.value) ().right
    else
      _errorCodes.find(_.value == value) match {
        case Some(code) => s"Call failed with error $code".left
        case None => s"Call failed with unknown error $value".left
      }

  private def debug(method: String): GL.DebugEffect[F, Unit] =
    GL.getError
      .foldMap(interpret)
      .transform(lift)
      .mapF(_.subflatMap(onError(method)))

  private def onCompileError(log: Option[String]): String Xor Unit =
    log.map(l => s"Compilation failed with error $l").toLeftXor(())

 private def onLinkError(log: Option[String]): String Xor Unit =
    log.map(l => s"Link failed with error $l").toLeftXor(())


  private def shaderLog(shader: Int) =
    GL.getCompileError(shader)
      .foldMap(interpret)
      .transform(lift)
      .mapF(_.subflatMap(onCompileError))

  private def programLog(program: Int) = GL.getLinkError(program)
  .foldMap(interpret)
  .transform(lift)
  .mapF(_.subflatMap(onLinkError))

  def apply[A](gl: GL[A]): GL.DebugEffect[F, A] = gl match {

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
