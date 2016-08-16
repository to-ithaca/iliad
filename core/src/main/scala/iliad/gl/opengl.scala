package iliad
package gl

import iliad.kernel.platform.GLES30Library

import iliad.std.int._
import iliad.std.list._
import iliad.syntax.vectord._
import iliad.syntax.matrixd._

import cats._
import cats.data._
import cats.free._, Free._
import cats.implicits._

import java.nio.Buffer

import iliad.CatsExtra._

object OpenGL {

  type DSL[A] = Free[OpenGL, A]
  type Effect[F[_], A] = ReaderT[F, GLES30Library, A]

  type NoEffect[A] = Effect[Id, A]

  type Logger[F[_], A] = WriterT[F, List[String], A]
  type LogEffect[F[_], A] = Effect[Logger[F, ?], A]

  type Debugger[F[_], A] = XorT[F, GLError, A]
  type DebugEffect[F[_], A] = Effect[Debugger[F, ?], A]

  type Interpreter[F[_]] = OpenGL ~> F

  val run: Interpreter[NoEffect] = GLInterpreter
  val log: Interpreter[LogEffect[Id, ?]] = new GLLogInterpreter(run)

  val effectfulLog: Interpreter[NoEffect] = new GLEffectfulLogInterpreter(run)

//  implicit val MW: MonadRec[WriterT[Id, List[String], ?]] = ???

  //val debugLog: Interpreter[DebugEffect[Logger[Id, ?], ?]] =
  // new GLDebugInterpreter(log)

  def interpret[F[_]: RecursiveTailRecM : Monad](f: Interpreter[F]): (DSL ~> F) =
    new (DSL ~> F) {
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
      if (status == GL_TRUE.value) Free.pure(None)
      else
        for {
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
      program: Int,
      attributes: List[String]): DSL[List[(String, Int)]] =
    traverseKeys(attributes)(a => GLGetAttribLocation(program, a).free)

  def getUniformLocations(program: Int,
                          uniforms: List[String]): DSL[List[(String, Int)]] =
    traverseKeys(uniforms)(u => GLGetUniformLocation(program, u).free)

  private val genBuffer: DSL[Int] = GLGenBuffers(1).free.map(_.head)

  private def makeEmptyBuffer(target: BufferTarget, capacity: Int): DSL[Int] =
    for {
      id <- genBuffer
      _ <- GLBindBuffer(target, id).free
      _ <- GLBufferData(target, capacity, null, GL_STATIC_DRAW).free
    } yield id

  private def makeBuffer(target: BufferTarget,
                         data: Buffer,
                         size: Int,
                         capacity: Int): DSL[Int] =
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

  def insertVertices(buffer: Int,
                     offset: Int,
                     size: Int,
                     data: Buffer): DSL[Unit] =
    insertInBuffer(GL_ARRAY_BUFFER, buffer, offset, size, data)

  def insertElements(buffer: Int,
                     offset: Int,
                     size: Int,
                     data: Buffer): DSL[Unit] =
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
    copyToNewBuffer(oldId,
                    GL_ELEMENT_ARRAY_BUFFER,
                    offset,
                    size,
                    data,
                    capacity)

  private def emptyTextureData(t: Texture.Constructor, id: Int): DSL[Unit] =
    for {
      _ <- GLBindTexture(id).free
      _ <- GLTexImage2D(t.format.internal,
                        t.viewport.x,
                        t.viewport.y,
                        t.format.pixel,
                        t.format.pixelType,
                        null).free
    } yield ()

  private def singleTextureData(t: Texture.Constructor,
                                id: Int,
                                data: Buffer): DSL[Unit] =
    for {
      _ <- GLBindTexture(id).free
      _ <- GLTexImage2D(t.format.internal,
                        t.viewport.x,
                        t.viewport.y,
                        t.format.pixel,
                        t.format.pixelType,
                        data).free
    } yield ()

  private def textureData(t: Texture.Constructor,
                          id: Int,
                          rect: Rect[Int],
                          data: Buffer): DSL[Unit] =
    for {
      _ <- GLTexSubImage2D(rect.bottomLeft.x,
                           rect.bottomLeft.y,
                           rect.width,
                           rect.height,
                           t.format.pixel,
                           t.format.pixelType,
                           data).free
    } yield ()

  private def textureData(t: Texture.Constructor,
                          id: Int,
                          data: Texture.GroupData): DSL[Unit] =
    for {
      _ <- emptyTextureData(t, id)
      _ <- data.subData.toList.traverseUnit {
            case (rect, d) => textureData(t, id, rect, d.data)
          }
    } yield ()

  private def textureData(t: Texture.Constructor,
                          data: Texture.Data,
                          id: Int): DSL[Unit] = data match {
    case Texture.Empty => emptyTextureData(t, id)
    case Texture.SingleData(d, _) => singleTextureData(t, id, d)
    case g: Texture.GroupData => textureData(t, id, g)
  }

  def makeSingleTexture(t: Texture.SingleConstructor,
                        data: Texture.Data): DSL[Int] =
    for {
      id <- GLGenTextures(1).free
      _ <- textureData(t, data, id.head)
    } yield id.head

  def makeBufferedTexture(t: Texture.DoubleConstructor,
                          data: Texture.Data): DSL[(Int, Int)] =
    for {
      ids <- GLGenTextures(2).free
      front = ids.head
      back = ids.tail.head
      _ <- textureData(t, data, front)
      _ <- textureData(t, data, back)
    } yield (front, back)

  def makeRenderbuffer(r: Renderbuffer.Constructor): DSL[Int] =
    for {
      ids <- GLGenRenderbuffers(1).free
      id = ids.head
      _ <- GLBindRenderbuffer(id).free
      _ <- GLRenderbufferStorage(r.format, r.viewport.x, r.viewport.y).free
    } yield id

  private def bindAttachment(
      a: FramebufferAttachment,
      l: Framebuffer.AttachmentLoaded)(f: Texture.Loaded => Int): DSL[Unit] =
    l match {
      case Renderbuffer.Loaded(_, id) =>
        GLFramebufferRenderbuffer(a, id).free
      case t: Texture.Loaded => GLFramebufferTexture2D(a, f(t)).free
    }

  private def bindAttachments(
      as: List[(FramebufferAttachment, Framebuffer.AttachmentLoaded)])(
      f: Texture.Loaded => Int): DSL[Unit] =
    as.traverseUnit {
      case (a, l) => bindAttachment(a, l)(f)
    }

  private def makeFramebuffer(
      as: List[(FramebufferAttachment, Framebuffer.AttachmentLoaded)],
      id: Int)(f: Texture.Loaded => Int): DSL[Unit] =
    for {
      _ <- bindFramebuffer(id)
      _ <- bindAttachments(as)(f)
      _ <- GLDrawBuffers(as.map(_._1).filterClass[ColorBuffer]).free
    } yield ()

  def makeSingleFramebuffer(
      as: List[(FramebufferAttachment, Framebuffer.AttachmentLoaded)])
    : DSL[Int] =
    for {
      ids <- GLGenFramebuffers(1).free
      id = ids.head
      _ <- makeFramebuffer(as, id)(_.frontId)
    } yield id

  def makeBufferedFramebuffer(
      as: List[(FramebufferAttachment, Framebuffer.AttachmentLoaded)])
    : DSL[(Int, Int)] =
    for {
      ids <- GLGenFramebuffers(2).free
      front = ids.head
      back = ids.tail.head
      _ <- makeFramebuffer(as, front)(_.frontId)
      _ <- makeFramebuffer(as, back) {
            case t: Texture.SingleLoaded => t.frontId
            case t: Texture.DoubleLoaded => t.backId
          }
    } yield (front, back)

  def makeSampler(s: Sampler.Constructor): DSL[Int] =
    for {
      ids <- GLGenSamplers(1).free
      id = ids.head
      _ <- GLSamplerParameteri(id, GL_TEXTURE_WRAP_S, s.wrapS).free
      _ <- GLSamplerParameteri(id, GL_TEXTURE_WRAP_S, s.wrapT).free
      _ <- GLSamplerParameteri(id, GL_TEXTURE_MAG_FILTER, s.magFilter).free
      _ <- GLSamplerParameteri(id, GL_TEXTURE_MIN_FILTER, s.minFilter).free
    } yield id

  def bindFramebuffer(framebuffer: Int): DSL[Unit] =
    GLBindFramebuffer(GL_FRAMEBUFFER, framebuffer).free
  def enable(c: Capability): DSL[Unit] =
    GLEnable(c).free
  def disable(c: Capability): DSL[Unit] =
    GLDisable(c).free
  def colorMask(red: Boolean,
                green: Boolean,
                blue: Boolean,
                alpha: Boolean): DSL[Unit] =
    GLColorMask(red, green, blue, alpha).free
  def clear(mask: ChannelBitMask): DSL[Unit] = GLClear(mask).free
  def clearColor(red: Float, green: Float, blue: Float, alpha: Float): DSL[Unit] =
    GLClearColor(red, green, blue, alpha).free
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
      _ <- GLVertexAttribPointer(location,
                                 numElements,
                                 `type`,
                                 false,
                                 stride,
                                 offset).free
    } yield ()

  def enableAttributeI(location: Int,
                       numElements: Int,
                       `type`: VertexAttribIType,
                       stride: Int,
                       offset: Int): DSL[Unit] =
    for {
      _ <- GLEnableVertexAttribArray(location).free
      _ <- GLVertexAttribIPointer(location,
                                  numElements,
                                  `type`,
                                  stride,
                                  offset).free
    } yield ()

  def bindUniform1i(location: Int, value: Int): DSL[Unit] =
    GLUniform1i(location, value).free

  def bindUniform1f(location: Int, value: Float): DSL[Unit] =
    GLUniform1f(location, value).free

  def bindUniform2i(location: Int, value: Vec2i): DSL[Unit] =
    GLUniform2i(location, value).free

  def bindUniform2f(location: Int, value: Vec2f): DSL[Unit] =
    GLUniform2f(location, value).free

  def bindUniform3i(location: Int, value: Vec3i): DSL[Unit] =
    GLUniform3i(location, value).free

  def bindUniform3f(location: Int, value: Vec3f): DSL[Unit] =
    GLUniform3f(location, value).free

  def bindUniform4i(location: Int, value: Vec4i): DSL[Unit] =
    GLUniform4i(location, value).free

  def bindUniform4f(location: Int, value: Vec4f): DSL[Unit] =
    GLUniform4f(location, value).free

  def bindUniformMatrix2f(location: Int, value: Mat2f): DSL[Unit] =
    GLUniformMatrix2f(location, value).free

  def bindUniformMatrix3f(location: Int, value: Mat3f): DSL[Unit] =
    GLUniformMatrix3f(location, value).free

  def bindUniformMatrix4f(location: Int, value: Mat4f): DSL[Unit] =
    GLUniformMatrix4f(location, value).free

  def bindTextureUniform(unit: TextureUnit,
                         location: Int,
                         texture: Int,
                         sampler: Int): DSL[Unit] =
    for {
      _ <- GLActiveTexture(unit).free
      _ <- GLBindTexture(texture).free
      _ <- GLBindSampler(unit, sampler).free
      _ <- GLUniform1i(location, Bounded.indexOf(unit)).free
    } yield ()

  def drawTriangles(start: Int, end: Int): DSL[Unit] =
    GLDrawElements(GL_TRIANGLES,
                   end / SizeOf[Int].byteSize - start / SizeOf[Int].byteSize,
                   GL_UNSIGNED_INT,
                   start * SizeOf[Int].byteSize).free
}

sealed trait OpenGL[A]

case object GLGetError extends OpenGL[Int]

case class GLGetShaderiv(shader: Int, pname: ShaderParameter)
    extends OpenGL[Int]
case class GLGetShaderInfoLog(shader: Int, maxLength: Int)
    extends OpenGL[String]

case class GLGetProgramiv(program: Int, pname: ProgramParameter)
    extends OpenGL[Int]
case class GLGetProgramInfoLog(program: Int, maxLength: Int)
    extends OpenGL[String]

case class GLCreateShader(`type`: ShaderType) extends OpenGL[Int]
case class GLShaderSource(shader: Int, sources: List[String])
    extends OpenGL[Unit]
case class GLCompileShader(shader: Int) extends OpenGL[Unit]
case object GLCreateProgram extends OpenGL[Int]
case class GLAttachShader(program: Int, shader: Int) extends OpenGL[Unit]
case class GLLinkProgram(program: Int) extends OpenGL[Unit]
case class GLGetAttribLocation(program: Int, name: String) extends OpenGL[Int]
case class GLGetUniformLocation(program: Int, name: String) extends OpenGL[Int]
case class GLGenBuffers(number: Int) extends OpenGL[Set[Int]]
case class GLBindBuffer(target: BufferTarget, buffer: Int) extends OpenGL[Unit]
case class GLBufferData(target: BufferTarget,
                        size: Int,
                        data: Buffer,
                        usage: BufferUsage)
    extends OpenGL[Unit]
case class GLBufferSubData(target: BufferTarget,
                           offset: Int,
                           size: Int,
                           data: Buffer)
    extends OpenGL[Unit]
case class GLCopyBufferSubData(read: BufferTarget,
                               write: BufferTarget,
                               readOffset: Int,
                               writeOffset: Int,
                               size: Int)
    extends OpenGL[Unit]

case class GLGenTextures(number: Int) extends OpenGL[Set[Int]]
case class GLBindTexture(texture: Int) extends OpenGL[Unit]
case class GLTexImage2D(internalFormat: TextureInternalFormat,
                        width: Int,
                        height: Int,
                        format: TextureFormat,
                        pixelType: TexturePixelType,
                        data: Buffer)
    extends OpenGL[Unit]
case class GLTexSubImage2D(xOffset: Int,
                           yOffset: Int,
                           width: Int,
                           height: Int,
                           format: TextureFormat,
                           pixelType: TexturePixelType,
                           data: Buffer)
    extends OpenGL[Unit]

case class GLGenRenderbuffers(number: Int) extends OpenGL[Set[Int]]
case class GLBindRenderbuffer(renderbuffer: Int) extends OpenGL[Unit]
case class GLRenderbufferStorage(format: RenderbufferInternalFormat,
                                 width: Int,
                                 height: Int)
    extends OpenGL[Unit]

case class GLGenFramebuffers(number: Int) extends OpenGL[Set[Int]]
case class GLBindFramebuffer(target: FramebufferTarget, framebuffer: Int)
    extends OpenGL[Unit]
case class GLFramebufferRenderbuffer(channel: FramebufferAttachment,
                                     renderbuffer: Int)
    extends OpenGL[Unit]
case class GLFramebufferTexture2D(channel: FramebufferAttachment, texture: Int)
    extends OpenGL[Unit]
case class GLDrawBuffers(buffers: List[ColorBuffer]) extends OpenGL[Unit]

case class GLGenSamplers(number: Int) extends OpenGL[Set[Int]]
case class GLSamplerParameteri(sampler: Int,
                               name: SamplerParameter,
                               value: SamplerValue)
    extends OpenGL[Unit]

case class GLEnable(capability: Capability) extends OpenGL[Unit]
case class GLDisable(capability: Capability) extends OpenGL[Unit]
case class GLColorMask(red: Boolean,
                       green: Boolean,
                       blue: Boolean,
                       alpha: Boolean)
    extends OpenGL[Unit]
case class GLUseProgram(program: Int) extends OpenGL[Unit]
case class GLEnableVertexAttribArray(location: Int) extends OpenGL[Unit]
case class GLVertexAttribPointer(location: Int,
                                 size: Int,
                                 `type`: VertexAttribType,
                                 normalized: Boolean,
                                 stride: Int,
                                 offset: Int)
    extends OpenGL[Unit]
case class GLVertexAttribIPointer(location: Int,
                                  size: Int,
                                  `type`: VertexAttribIType,
                                  stride: Int,
                                  offset: Int)
    extends OpenGL[Unit]

case class GLActiveTexture(unit: TextureUnit) extends OpenGL[Unit]
case class GLBindSampler(unit: TextureUnit, sampler: Int) extends OpenGL[Unit]
case class GLUniform1i(location: Int, value: Int) extends OpenGL[Unit]
case class GLUniform1f(location: Int, value: Float) extends OpenGL[Unit]
case class GLUniform2i(location: Int, value: Vec2i) extends OpenGL[Unit]
case class GLUniform2f(location: Int, value: Vec2f) extends OpenGL[Unit]
case class GLUniform3i(location: Int, value: Vec3i) extends OpenGL[Unit]
case class GLUniform3f(location: Int, value: Vec3f) extends OpenGL[Unit]
case class GLUniform4i(location: Int, value: Vec4i) extends OpenGL[Unit]
case class GLUniform4f(location: Int, value: Vec4f) extends OpenGL[Unit]

case class GLUniformMatrix2f(location: Int, value: Mat2f) extends OpenGL[Unit]
case class GLUniformMatrix3f(location: Int, value: Mat3f) extends OpenGL[Unit]
case class GLUniformMatrix4f(location: Int, value: Mat4f) extends OpenGL[Unit]

case class GLDrawElements(mode: PrimitiveType,
                          count: Int,
                          `type`: IndexType,
                          offset: Int)
    extends OpenGL[Unit]
case class GLClear(bitmask: ChannelBitMask) extends OpenGL[Unit]
case class GLClearColor(r: Float, g: Float, b: Float, a: Float) extends OpenGL[Unit]
