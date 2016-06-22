package iliad
package gl

import iliad.kernel.platform.GLES30Library
import iliad.CatsExtra._
import iliad.std.int._

import cats._
import cats.data._
import cats.std._
import cats.free._, Free._
import cats.implicits._

import java.nio.Buffer

object GL {

  type DSL[A] = Free[GL, A]
  type Effect[F[_], A] = ReaderT[F, GLES30Library, A]

  type NoEffect[A] = Effect[Id, A]

  type Logger[F[_], A] = WriterT[F, List[String], A]
  type LogEffect[F[_], A] = Effect[Logger[F, ?], A]

  type Debugger[F[_], A] = XorT[F, String, A]
  type DebugEffect[F[_], A] = Effect[Debugger[F, ?], A]

  type Interpreter[F[_]] = GL ~> F

  val run: Interpreter[NoEffect] = GLInterpreter
  val log: Interpreter[LogEffect[Id, ?]] = new GLLogInterpreter(run)

  val debugLog: Interpreter[DebugEffect[Logger[Id, ?], ?]] =
    new GLDebugInterpreter(log)

  def interpret[F[_]: Monad](f: Interpreter[F]): (DSL ~> F) = new (DSL ~> F) {
    def apply[A](gl: DSL[A]): F[A] = gl.foldMap(f)
  }

  val getError: DSL[Int] = GLGetError.free

  private def getShaderLogLength(shader: Int): DSL[Int] =
    GLGetShaderiv(shader, GL_INFO_LOG_LENGTH).free
  private def getCompileStatus(shader: Int): DSL[Int] =
    GLGetShaderiv(shader, GL_COMPILE_STATUS).free

  def getCompileError(shader: Int): DSL[Option[String]] =
    getCompileStatus(shader) flatMap { status =>
      if (status == GL_TRUE.value) Free.pure(None)
      else
        for {
          l <- getShaderLogLength(shader)
          s <- liftF(GLGetShaderInfoLog(shader, l))
        } yield Some(s)
    }

  private def getProgramLogLength(program: Int): DSL[Int] = 
    GLGetProgramiv(program, GL_INFO_LOG_LENGTH).free
  private def getLinkStatus(program: Int): DSL[Int] = 
    GLGetProgramiv(program, GL_LINK_STATUS).free
  def getLinkError(program: Int): DSL[Option[String]] = 
    getLinkStatus(program) flatMap { status =>
      if(status == GL_TRUE.value) Free.pure(None)
      else for {
        l <- getProgramLogLength(program)
        s <- GLGetProgramInfoLog(program, l).free
        } yield Some(s)
    }


  def makeVertexShader(source: String): DSL[Int] =
    for {
      id <- GLCreateShader(GL_VERTEX_SHADER).free
      _ <- GLShaderSource(id, List(source)).free
      _ <- GLCompileShader(id).free
    } yield id

  def makeFragmentShader(source: String): DSL[Int] =
    for {
      id <- GLCreateShader(GL_FRAGMENT_SHADER).free
      _ <- GLShaderSource(id, List(source)).free
      _ <- GLCompileShader(id).free
    } yield id

  def makeProgram(vertexId: Int, fragmentId: Int): DSL[Int] =
    for {
      id <- GLCreateProgram.free
      _ <- GLAttachShader(id, vertexId).free
      _ <- GLAttachShader(id, fragmentId).free
      _ <- GLLinkProgram(id).free
    } yield id

  private def traverseKeys[A, B, G[_]: Traverse](keys: G[A])(
      f: A => DSL[B]): DSL[G[(A, B)]] =
    keys.traverse[DSL, (A, B)](s => f(s).map(s -> _))

  def getAttributeLocations(
      program: Int, attributes: List[String]): DSL[List[(String, Int)]] =
    traverseKeys(attributes)(a => GLGetAttribLocation(program, a).free)

  private val genBuffer: DSL[Int] = GLGenBuffers(1).free.map(_.head)

  private def makeEmptyBuffer(target: BufferTarget, capacity: Int): DSL[Int] =
    for {
      id <- genBuffer
      _ <- GLBindBuffer(target, id).free
      _ <- GLBufferData(target, capacity, null, GL_STATIC_DRAW).free
    } yield id

  private def makeBuffer(
      target: BufferTarget, data: Buffer, size: Int, capacity: Int): DSL[Int] =
    for {
      id <- makeEmptyBuffer(target, capacity)
      _ <- GLBufferSubData(target, 0, size, data).free
    } yield id

  def makeVertexBuffer(data: Buffer, size: Int, capacity: Int): DSL[Int] =
    makeBuffer(GL_ARRAY_BUFFER, data, size, capacity)

  def makeElementBuffer(data: Buffer, size: Int, capacity: Int): DSL[Int] =
    makeBuffer(GL_ELEMENT_ARRAY_BUFFER, data, size, capacity)

  private def insertInBuffer(target: BufferTarget,
                             buffer: Int,
                             offset: Int,
                             size: Int,
                             data: Buffer): DSL[Unit] =
    for {
      _ <- GLBindBuffer(target, buffer).free
      _ <- GLBufferSubData(target, offset, size, data).free
    } yield ()

  def insertVertices(buffer: Int, offset: Int, size: Int, data: Buffer) =
    insertInBuffer(GL_ARRAY_BUFFER, buffer, offset, size, data)

  def insertElements(buffer: Int, offset: Int, size: Int, data: Buffer) =
    insertInBuffer(GL_ELEMENT_ARRAY_BUFFER, buffer, offset, size, data)

  private def copyToNewBuffer(oldId: Int,
                              target: BufferTarget,
                              offset: Int,
                              size: Int,
                              data: Buffer,
                              capacity: Int): DSL[Int] =
    for {
      id <- makeEmptyBuffer(target, capacity)
      _ <- GLBindBuffer(GL_COPY_READ_BUFFER, oldId).free
      _ <- GLCopyBufferSubData(GL_COPY_READ_BUFFER, target, 0, 0, offset).free
      _ <- GLBufferSubData(target, offset, size, data).free
    } yield id

  def copyVertices(oldId: Int,
                   offset: Int,
                   size: Int,
                   data: Buffer,
                   capacity: Int): DSL[Int] =
    copyToNewBuffer(oldId, GL_ARRAY_BUFFER, offset, size, data, capacity)

  def copyElements(oldId: Int,
                   offset: Int,
                   size: Int,
                   data: Buffer,
                   capacity: Int): DSL[Int] =
    copyToNewBuffer(
        oldId, GL_ELEMENT_ARRAY_BUFFER, offset, size, data, capacity)

  def bindFramebuffer(framebuffer: Int): DSL[Unit] =
    GLBindFramebuffer(GL_FRAMEBUFFER, framebuffer).free
  def clear(mask: ChannelBitMask): DSL[Unit] = GLClear(mask).free
  def useProgram(program: Int): DSL[Unit] = GLUseProgram(program).free
  def bindVertexBuffer(buffer: Int): DSL[Unit] =
    GLBindBuffer(GL_ARRAY_BUFFER, buffer).free
  def bindElementBuffer(buffer: Int): DSL[Unit] =
    GLBindBuffer(GL_ELEMENT_ARRAY_BUFFER, buffer).free

  def enableAttribute(location: Int,
                      numElements: Int,
                      `type`: VertexAttribType,
                      stride: Int,
                      offset: Int): DSL[Unit] =
    for {
      _ <- GLEnableVertexAttribArray(location).free
      _ <- GLVertexAttribPointer(
              location, numElements, `type`, false, stride, offset).free
    } yield ()

  def drawTriangles(start: Int, end: Int): DSL[Unit] =
    GLDrawElements(GL_TRIANGLES,
                   end / SizeOf[Int].byteSize - start / SizeOf[Int].byteSize,
                   GL_UNSIGNED_INT,
                   start * SizeOf[Int].byteSize).free
}

sealed trait GL[A]

case object GLGetError extends GL[Int]

case class GLGetShaderiv(shader: Int, pname: ShaderParameter) extends GL[Int]
case class GLGetShaderInfoLog(shader: Int, maxLength: Int) extends GL[String]

case class GLGetProgramiv(program: Int, pname: ProgramParameter) extends GL[Int]
case class GLGetProgramInfoLog(program: Int, maxLength: Int) extends GL[String]

case class GLCreateShader(`type`: ShaderType) extends GL[Int]
case class GLShaderSource(shader: Int, sources: List[String]) extends GL[Unit]
case class GLCompileShader(shader: Int) extends GL[Unit]
case object GLCreateProgram extends GL[Int]
case class GLAttachShader(program: Int, shader: Int) extends GL[Unit]
case class GLLinkProgram(program: Int) extends GL[Unit]
case class GLGetAttribLocation(program: Int, name: String) extends GL[Int]

case class GLGenBuffers(number: Int) extends GL[List[Int]]
case class GLBindBuffer(target: BufferTarget, buffer: Int) extends GL[Unit]
case class GLBufferData(
    target: BufferTarget, size: Int, data: Buffer, usage: BufferUsage)
    extends GL[Unit]
case class GLBufferSubData(
    target: BufferTarget, offset: Int, size: Int, data: Buffer)
    extends GL[Unit]
case class GLCopyBufferSubData(read: BufferTarget,
                               write: BufferTarget,
                               readOffset: Int,
                               writeOffset: Int,
                               size: Int)
    extends GL[Unit]

case class GLBindFramebuffer(target: FramebufferTarget, framebuffer: Int)
    extends GL[Unit]
case class GLEnable(capability: Capability) extends GL[Unit]
case class GLDisable(capability: Capability) extends GL[Unit]
case class GLColorMask(
    red: Boolean, green: Boolean, blue: Boolean, alpha: Boolean)
    extends GL[Unit]
case class GLUseProgram(program: Int) extends GL[Unit]
case class GLEnableVertexAttribArray(location: Int) extends GL[Unit]
case class GLVertexAttribPointer(location: Int,
                                 size: Int,
                                 `type`: VertexAttribType,
                                 normalized: Boolean,
                                 stride: Int,
                                 offset: Int)
    extends GL[Unit]
case class GLDrawElements(
    mode: PrimitiveType, count: Int, `type`: IndexType, offset: Int)
    extends GL[Unit]
case class GLClear(bitmask: ChannelBitMask) extends GL[Unit]
