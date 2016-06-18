package iliad
package gl

import iliad.kernel.platform.GLES30Library
import iliad.kernel.Buffer //TODO: move this to a different place

import cats._
import cats.data._
import cats.implicits._

import java.nio.IntBuffer

object GLInterpreter extends (GL.Interpreter[GL.NoEffect]) {

  //nasty but the signature of gen means the alternatives are worse!
  private def genObject(f: GLES30Library => (Int, IntBuffer) => Unit,
                        num: Int): Reader[GLES30Library, List[Int]] = Reader {
    lib =>
      val ptr = Buffer.capacity[Int](num)
      f(lib)(num, ptr)
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

    case GLGenBuffers(number) => genObject(_.glGenBuffers, number)
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

object GLLogInterpreter extends (GL.Interpreter[GL.LogEffect]) {

  private val logAfter: Id ~> Writer[List[String], ?] =
    new (Id ~> Writer[List[String], ?]) {
      def apply[A](a: Id[A]): Writer[List[String], A] =
        a.writer(List(s"returned $a"))
    }

  def apply[A](gl: GL[A]): GL.LogEffect[A] = {
    List(s"called $gl").tell.liftT[ReaderT[?[_], GLES30Library, ?]] >>
    GLInterpreter(gl).transform(logAfter)
  }
}

object GLDebugInterpreter extends (GL.Interpreter[GL.DebugEffect]) {

  private val lift: Writer[List[String], ?] ~> XorT[
      Writer[List[String], ?], String, ?] =
    new (Writer[List[String], ?] ~> XorT[Writer[List[String], ?], String, ?]) {
      def apply[A](w: Writer[List[String], A])
        : XorT[Writer[List[String], ?], String, A] = {
        XorT.right(w)
      }
    }

  private val _errorCodes: Set[ErrorCode] = SealedEnum.values

  private def onError(method: String)(value: Int): String Xor Unit =
    if (value == GL_NO_ERROR.value) ().right
    else
      _errorCodes.find(_.value == value) match {
        case Some(code) => s"Call failed with error $code".left
        case None => s"Call failed with unknown error $value".left
      }

  private def debug(method: String): GL.DebugEffect[Unit] =
    GL.getError
      .foldMap(GLLogInterpreter)
      .transform(lift)
      .mapF(_.subflatMap(onError(method)))

  private def onCompileError(log: Option[String]): String Xor Unit =
    log.map(l => s"Compilation failed with error $l").toLeftXor(())

  private def shaderLog(shader: Int) =
    GL.getCompileError(shader)
      .foldMap(GLLogInterpreter)
      .transform(lift)
      .mapF(_.subflatMap(onCompileError))

  def apply[A](gl: GL[A]): GL.DebugEffect[A] = gl match {

    case GLCompileShader(shader) =>
      for {
        _ <- GLLogInterpreter(gl).transform(lift)
        _ <- shaderLog(shader)
        _ <- debug(gl.toString)
      } yield ()

    case _ =>
      for {
        a <- GLLogInterpreter(gl).transform(lift)
        _ <- debug(gl.toString)
      } yield a
  }
}
