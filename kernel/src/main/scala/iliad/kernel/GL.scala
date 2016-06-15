package iliad
package kernel

import iliad.kernel.vectord._
import iliad.kernel.matrixd._
import iliad.kernel.platform._

import java.nio._

import cats._
import cats.data._
import cats.implicits._

import simulacrum.typeclass

import GL._
import GLState.Zoom._

object GL {
  val run: GL[Id] = GLRunner

  def log(logCfg: LoggerConfig): GL[LogEffect[Id, ?]] =
    new GLLogger(logCfg, run)
  def debugAndLog(debugCfg: DebuggerConfig,
                  logCfg: LoggerConfig): GL[DebugEffect[LogEffect[Id, ?], ?]] =
    new GLDebugger[LogEffect[Id, ?]](log(logCfg))

  /** Logger config for specifying which methods to log and verbosity */
  case class LoggerConfig(s: Set[String])

  /** Debugging config for specifying which methods to debug */
  case class DebuggerConfig(filtered: Set[String])

  type IO[F[_], A] = ReaderT[StateT[F, GLState, ?], GLES30Library, A]
  type LogEffect[F[_], A] = WriterT[F, List[String], A]
  type DebugEffect[F[_], A] = XorT[F, String, A]
  type StateEffect[F[_], A] = StateT[F, GLState, A]
}

/**
  Typesafe GL. The type parameter `F` has type constraints to compose operations.
  */
abstract class GL[F[_]: Monad] {

  private def modify[A](io: IO[F, A])(f: A => GLState => GLState): IO[F, A] =
    io.mapF(_.transform { case (s, a) => (f(a)(s), a) })

  private def inspect[A](f: GLState => A): IO[F, A] =
    ReaderT(
        _ => State.inspect(f).transformF(s => Applicative[F].pure(s.value)))

  private def makeIfNotExists[A](opt: IO[F, Option[A]])(
      create: IO[F, A]): IO[F, A] = opt >>= {
    case Some(a) => ReaderT.pure(a)
    case None => create
  }

  private def performNotElse[A](f: GLState => Boolean)(a: A)(io: IO[F, A]) =
    inspect(f) >>= {
      case false => io
      case true => ReaderT.pure(a)
    }

  private def performNot(f: GLState => Boolean)(io: IO[F, Unit]) =
    performNotElse(f)(())(io)

  private def liftXorT[A](io: IO[F, A]): IO[XorT[F, (GLState, String), ?], A] =
    toXorT(io.map(_.right[String]))

  private def toXorT[A](
      io: IO[F, String Xor A]): IO[XorT[F, (GLState, String), ?], A] =
    io.mapF(
        _.transformF(fsa =>
              XorT(fsa.map {
        case (s, xor) => xor.bimap(msg => s -> msg, a => s -> a)
      })))

  private def fromXorT[A](
      io: IO[XorT[F, (GLState, String), ?], A]): IO[F, String Xor A] =
    io.mapF(
        _.transformF(_.value.map { xor =>
      val s = xor.bimap(_._1, _._1).merge
      (s, xor.bimap(_._2, _._2))
    }))

  private def flatMapXor[A, B](ioa: IO[F, String Xor A])(
      f: A => IO[F, String Xor B]): IO[F, String Xor B] = ioa >>= {
    case l: Xor.Left[String] => ReaderT.pure(l)
    case Xor.Right(p) => f(p)
  }

  private def keyTraverse[A, B, G[_]: Traverse](keys: G[A])(
      f: A => IO[F, B]): IO[F, G[(A, B)]] =
    keys.traverse[IO[F, ?], (A, B)](s => f(s).map(s -> _))

  private def _tup1[A](arr: Array[A]): A = arr(0)
  private def _tup2[A](arr: Array[A]): (A, A) = (arr(0), arr(1))
  private def _tup3[A](arr: Array[A]): (A, A, A) = (arr(0), arr(1), arr(2))

  def viewport(rect: Rect[Int]): IO[F, Unit]
  def flush: IO[F, Unit]
  def clear(bitMask: ChannelBitMask): IO[F, Unit]
  def clearColor(
      red: Float, green: Float, blue: Float, alpha: Float): IO[F, Unit]
  def colorMask(
      red: Boolean, green: Boolean, blue: Boolean, alpha: Boolean): IO[F, Unit]
  def colorMask(m: ColorMask): IO[F, Unit] =
    colorMask(m.red, m.green, m.blue, m.alpha)

  def bindColorMask(mask: ColorMask): IO[F, Unit] =
    performNot(GLState.hasColorMask(mask))(modify(colorMask(mask))(_ =>
              GLState.bind(mask)))

  def enable(cap: Capability): IO[F, Unit]

  def bindEnable(cap: Capability): IO[F, Unit] =
    performNot(GLState.hasCapability(cap, true))(modify(enable(cap))(_ =>
              GLState.bind(cap, true)))

  def disable(cap: Capability): IO[F, Unit]
  def bindDisable(cap: Capability): IO[F, Unit] =
    performNot(GLState.hasCapability(cap, false))(modify(disable(cap))(_ =>
              GLState.bind(cap, false))) //we need a different value for this

  def getError: IO[F, Option[Int Xor ErrorCode]]

  /** Shader functions */
  private[kernel] def createShader(`type`: ShaderType): IO[F, Int]

  private[kernel] def shaderSource(
      shader: Int, count: Int, sources: Seq[String]): IO[F, Unit]

  private[kernel] def shaderSource(shader: Int, source: String): IO[F, Unit] =
    shaderSource(shader, 1, Seq(source))

  private[kernel] def compileShader(shader: Int): IO[F, Unit]

  private def makeNewShader(`type`: ShaderType, source: String): IO[F, Int] =
    for {
      id <- createShader(`type`)
      _ <- shaderSource(id, source)
      _ <- compileShader(id)
    } yield id

  private def makeVertexShader(s: VertexShader): IO[F, Loaded[VertexShader]] =
    makeIfNotExists(inspect(GLState.vertexShader(s)))(
        modify(makeNewShader(GL_VERTEX_SHADER, s.source) map (Loaded(_, s)))(
            GLState.addVertexShader))

  private def makeFragmentShader(
      s: FragmentShader): IO[F, Loaded[FragmentShader]] =
    makeIfNotExists(inspect(GLState.fragmentShader(s)))(
        modify(makeNewShader(GL_FRAGMENT_SHADER, s.source) map (Loaded(_, s)))(
            GLState.addFragmentShader))

  private[kernel] def createProgram: IO[F, Int]
  private[kernel] def attachShader(program: Int, shader: Int): IO[F, Unit]
  private[kernel] def linkProgram(program: Int): IO[F, Unit]

  private[kernel] def getAttribLocation(program: Int, name: String): IO[F, Int]

  private def getAttribLocations[G[_]: Traverse](
      program: Int, names: G[(String, AttributeType[_])])
    : IO[F, G[((String, AttributeType[_]), Int)]] =
    keyTraverse(names)(n => getAttribLocation(program, n._1))

  def getUniformLocation(program: Int, name: String): IO[F, Int]

  private def getUniformLocations[G[_]: Traverse, A](
      program: Int, names: G[(String, A)]): IO[F, G[((String, A), Int)]] =
    keyTraverse(names)(n => getUniformLocation(program, n._1))

  private[kernel] def makeNewProgram(p: Program): IO[F, LoadedProgram] =
    for {
      v <- makeVertexShader(p.vertex)
      f <- makeFragmentShader(p.fragment)
      id <- createProgram
      _ <- attachShader(id, v.id)
      _ <- attachShader(id, f.id)
      _ <- linkProgram(id)
      _ <- makeSamplers(p.samplers)
      as <- getAttribLocations(id, p.attributes)
      us <- getUniformLocations(id, p.uniforms)
      ts <- getUniformLocations(id, p.textures)
    } yield LoadedProgram(p, id, v, f, as, us, ts)

  def makeProgram(p: Program): IO[F, LoadedProgram] =
    makeIfNotExists(inspect(GLState.program(p)))(
        modify(makeNewProgram(p))(GLState.addProgram))

  private[kernel] def getShaderiv(
      shader: Int, pname: ShaderParameter): IO[F, Int]

  private def getShaderiv[A <: IntConstant](
      shader: Int, pname: ShaderParameter, expected: Set[A]): IO[F, A] =
    getShaderiv(shader, pname).map(code => expected.find(_.value == code).get)

  def getShaderiv(shader: Int, pname: GL_INFO_LOG_LENGTH.type): IO[F, Int] =
    getShaderiv(shader, pname: ShaderParameter)

  private[kernel] def getShaderInfoLog(
      shader: Int, maxLength: Int): IO[F, String]

  def getShaderInfoLog(shader: Int): IO[F, String] =
    getShaderiv(shader, GL_SHADER_SOURCE_LENGTH) >>=
      (getShaderInfoLog(shader, _))

  def useProgram(program: Int): IO[F, Unit]
  def bindProgram(p: LoadedProgram): IO[F, Unit] =
    performNot(GLState.hasProgram(p))(modify(useProgram(p.id))(_ =>
              GLState.bind(p)))

  def loadedProgram(p: Program): IO[F, String Xor LoadedProgram] =
    inspect(GLState.existingProgram(p))

  private[kernel] def genSamplers(num: Int): IO[F, Array[Int]]

  def genSampler: IO[F, Int] = genSamplers(1) map _tup1

  private[kernel] def samplerParameteri(
      sampler: Int, name: SamplerParameter, value: Int): IO[F, Unit]

  private[kernel] def samplerParameteri(
      sampler: Int, name: SamplerParameter, value: IntConstant): IO[F, Unit] =
    samplerParameteri(sampler, name, value.value)

  private[kernel] def samplerParameter(sampler: Int,
                                       name: GL_TEXTURE_MIN_FILTER.type,
                                       value: TextureMinFilter): IO[F, Unit] =
    samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(sampler: Int,
                                       name: GL_TEXTURE_WRAP_S.type,
                                       value: TextureWrap): IO[F, Unit] =
    samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(sampler: Int,
                                       name: GL_TEXTURE_WRAP_T.type,
                                       value: TextureWrap): IO[F, Unit] =
    samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(sampler: Int,
                                       name: GL_TEXTURE_WRAP_R.type,
                                       value: TextureWrap): IO[F, Unit] =
    samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(sampler: Int,
                                       name: GL_TEXTURE_MAG_FILTER.type,
                                       value: TextureMagFilter): IO[F, Unit] =
    samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(
      sampler: Int, name: GL_TEXTURE_MIN_LOD.type, value: Int): IO[F, Unit] =
    samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(
      sampler: Int, name: GL_TEXTURE_MAX_LOD.type, value: Int): IO[F, Unit] =
    samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(
      sampler: Int,
      name: GL_TEXTURE_COMPARE_MODE.type,
      value: TextureCompareMode): IO[F, Unit] =
    samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(
      sampler: Int,
      name: GL_TEXTURE_COMPARE_FUNC.type,
      value: TextureCompareFunc): IO[F, Unit] =
    samplerParameteri(sampler, name, value)

  private[kernel] def makeNewSampler(
      sampler: Sampler): IO[F, Loaded[Sampler]] =
    for {
      id <- genSampler
      _ <- samplerParameter(id, GL_TEXTURE_MIN_FILTER, sampler.minFilter)
      _ <- samplerParameter(id, GL_TEXTURE_MAG_FILTER, sampler.magFilter)
      _ <- samplerParameter(id, GL_TEXTURE_WRAP_S, sampler.wrapS)
      _ <- samplerParameter(id, GL_TEXTURE_WRAP_T, sampler.wrapT)
    } yield Loaded(id, sampler)

  private[kernel] def makeSampler(s: Sampler): IO[F, Loaded[Sampler]] =
    makeIfNotExists(inspect(GLState.sampler(s)))(
        modify(makeNewSampler(s))(GLState.addSampler))

  def makeSamplers(ss: List[Sampler]): IO[F, List[Loaded[Sampler]]] =
    keyTraverse(ss)(makeSampler) map (_.map(_._2))

  private[kernel] def genRenderbuffers(num: Int): IO[F, Array[Int]]
  private[kernel] def genRenderbuffer: IO[F, Int] =
    genRenderbuffers(1) map _tup1

  private[kernel] def bindRenderbuffer(renderbuffer: Int): IO[F, Unit]

  private[kernel] def renderbufferStorage(
      format: RenderbufferInternalFormat, width: Int, height: Int): IO[F, Unit]

  private[kernel] def makeNewRenderbuffer(
      r: RenderbufferInstance): IO[F, LoadedRenderbuffer] =
    for {
      id <- genRenderbuffer
      _ <- bindRenderbuffer(id)
      _ <- renderbufferStorage(
              r.internalFormat, r.viewport.width, r.viewport.height)
    } yield LoadedRenderbuffer(r, id)

  def makeRenderbuffer(r: RenderbufferInstance): IO[F, LoadedRenderbuffer] =
    makeIfNotExists(inspect(GLState.renderbuffer(r)))(
        modify(makeNewRenderbuffer(r))(GLState.addRenderbuffer))

  private[kernel] def bindTexture(
      target: TextureTarget, texture: Int): IO[F, Unit]

  def bindTexture2D(texture: Int): IO[F, Unit] =
    bindTexture(GL_TEXTURE_2D, texture)

  def bindTexture2DState(id: Int): IO[F, Unit] =
    performNot(GLState.hasTexture(id))(modify(bindTexture2D(id))(_ =>
              GLState.bindTexture(id)))

  private[kernel] def genTextures(num: Int): IO[F, Array[Int]]
  private def genTexture: IO[F, Int] = genTextures(1) map _tup1
  private def genTexture2: IO[F, (Int, Int)] = genTextures(2) map _tup2

  private[kernel] def texImage2D(target: TextureTarget,
                                 level: Int,
                                 internalFormat: TextureInternalFormat,
                                 width: Int,
                                 height: Int,
                                 format: TextureFormat,
                                 `type`: TexturePixelType,
                                 data: Buffer): IO[F, Unit]

  private[kernel] def texImage2D(data: TextureData): IO[F, Unit] =
    texImage2D(GL_TEXTURE_2D,
               0,
               data.texture.template.internalFormat,
               data.texture.template.viewport.width,
               data.texture.template.viewport.height,
               data.texture.template.format,
               data.texture.template.`type`,
               data.data.getOrElse(null))

  private def makeNewPageTexture(id: Int, data: TextureData): IO[F, Int] =
    for {
      _ <- bindTexture2DState(id)
      _ <- texImage2D(data)
    } yield id

  private def makeNewSingleTexture(data: TextureData): IO[F, LoadedTexture] =
    for {
      id <- genTexture
      _ <- makeNewPageTexture(id, data)
    } yield LoadedTexture(data.texture, BufferedId.Single(id))

  private def makeNewDoubleTexture(data: TextureData): IO[F, LoadedTexture] =
    for {
      ids <- genTexture2
      front <- makeNewPageTexture(ids._1, data)
      back <- makeNewPageTexture(ids._2, data)
    } yield LoadedTexture(data.texture, BufferedId.Double(front, back))

  private def makeNewTexture(data: TextureData): IO[F, LoadedTexture] =
    modify(if (data.texture.template.isDouble) makeNewDoubleTexture(data)
        else makeNewSingleTexture(data))(GLState.addTexture)

  private[kernel] def existingTexture(
      t: TextureInstance): IO[F, LoadedTexture] =
    inspect(GLState.texture(t)).map(_.get)

  private[kernel] def genFramebuffers(num: Int): IO[F, Array[Int]]
  def genFramebuffer: IO[F, Int] = genFramebuffers(1) map _tup1
  def genFramebuffer2: IO[F, (Int, Int)] = genFramebuffers(2) map _tup2

  def bindFramebuffer(target: FramebufferTarget, framebuffer: Int): IO[F, Unit]
  def bindFramebuffer(id: Int): IO[F, Unit] =
    bindFramebuffer(GL_FRAMEBUFFER, id)
  def bindFramebufferState(id: Int): IO[F, Unit] =
    performNot(GLState.hasFramebuffer(id))(modify(bindFramebuffer(id))(_ =>
              GLState.bindFramebuffer(id)))

  private[kernel] def framebufferRenderbuffer(
      target: FramebufferTarget,
      attachment: FramebufferAttachment,
      renderbuffer: Int): IO[F, Unit]

  private[kernel] def framebufferRenderbuffer(
      attachment: FramebufferAttachment, renderbuffer: Int): IO[F, Unit] =
    framebufferRenderbuffer(GL_FRAMEBUFFER, attachment, renderbuffer)

  def checkFramebufferStatus(
      target: FramebufferTarget): IO[F, FramebufferStatus]

  private[kernel] def framebufferTexture2D(target: FramebufferTarget,
                                           attachment: FramebufferAttachment,
                                           texTarget: FramebufferTexTarget,
                                           texture: Int,
                                           level: Int): IO[F, Unit]

  private[kernel] def framebufferTexture2D(
      attachment: FramebufferAttachment, texture: Int): IO[F, Unit] =
    framebufferTexture2D(GL_FRAMEBUFFER, attachment, GL_TEXTURE_2D, texture, 0)

  private[kernel] def bindAttachment(
      a: FramebufferAttachment, output: LoadedShaderOutput): IO[F, Unit] =
    output match {
      case LoadedTexturePage(t, id) => framebufferTexture2D(a, id)
      case LoadedRenderbuffer(r, id) => framebufferRenderbuffer(a, id)
    }

  private[kernel] def bindAttachments[G[_]: Traverse](
      as: G[(FramebufferAttachment, LoadedShaderOutput)]) =
    keyTraverse(as) { case (a, o) => bindAttachment(a, o) }

  private[kernel] def drawBuffers(
      num: Int, buffers: Seq[ColorOutputTarget]): IO[F, Unit]
  private[kernel] def drawBuffers(
      buffers: Seq[ColorOutputTarget]): IO[F, Unit] =
    drawBuffers(buffers.size, buffers)

  private[kernel] def loadedOutput(page: LoadedTexture => LoadedTexturePage)(
      o: ShaderOutputInstance): IO[F, LoadedShaderOutput] = o match {
    case t: TextureInstance => existingTexture(t).map(page)
    case r: RenderbufferInstance => makeRenderbuffer(r).map(identity)
  }

  private[kernel] def loadedOutputs(page: LoadedTexture => LoadedTexturePage)(
      os: List[ShaderOutputInstance]): IO[F, List[LoadedShaderOutput]] =
    keyTraverse(os)(loadedOutput(page)) map (_.map(_._2))

  private[kernel] def makeNewPageFramebuffer(
      page: LoadedTexture => LoadedTexturePage,
      f: Framebuffer,
      id: Int): IO[F, Int] =
    for {
      os <- loadedOutputs(page)(f.outputs.map(_._2))
      _ <- bindFramebufferState(id)
      _ <- bindAttachments(f.attachments(os))
      _ <- drawBuffers(f.drawBuffers)
    } yield id

  private def makeNewSingleFramebuffer(
      f: Framebuffer): IO[F, LoadedFramebuffer] =
    for {
      id <- genFramebuffer
      _ <- makeNewPageFramebuffer(_.frontPage, f, id)
    } yield LoadedFramebuffer(f, BufferedId.Single(id))

  private def makeNewDoubleFramebuffer(
      f: Framebuffer): IO[F, LoadedFramebuffer] =
    for {
      ids <- genFramebuffer2
      fid <- makeNewPageFramebuffer(_.frontPage, f, ids._1)
      bid <- makeNewPageFramebuffer(_.backPage, f, ids._2)
    } yield LoadedFramebuffer(f, BufferedId.Double(fid, bid))

  private[kernel] def makeNewFramebuffer(
      f: Framebuffer): IO[F, LoadedFramebuffer] =
    if (f.isDouble) makeNewDoubleFramebuffer(f)
    else makeNewSingleFramebuffer(f)

  def makeFramebuffer(f: Framebuffer): IO[F, LoadedFramebuffer] =
    makeIfNotExists(inspect(GLState.framebuffer(f)))(
        modify(makeNewFramebuffer(f))(GLState.addFramebuffer))

  //TODO: Work out how best to expose N buffers safely
  private[kernel] def genBuffers(num: Int): IO[F, Array[Int]]
  def genBuffer: IO[F, Int] = genBuffers(1) map _tup1
  def genBuffer2: IO[F, (Int, Int)] = genBuffers(2) map _tup2

  def bindBuffer(target: BufferTarget, buffer: Int): IO[F, Unit]
  def bindBufferState(target: BufferTarget, id: Int): IO[F, Unit] =
    performNot(GLState.hasBuffer(target, id))(
        modify(bindBuffer(target, id))(_ => GLState.bindBuffer(target, id)))

  def bufferData(target: BufferTarget,
                 size: Int,
                 data: Buffer,
                 usage: BufferUsage): IO[F, Unit]
  def bufferSubData(
      target: BufferTarget, offset: Int, size: Int, data: Buffer): IO[F, Unit]
  def enableVertexAttribArray(location: Int): IO[F, Unit]

  def vertexAttribPointer(location: Int,
                          size: Int,
                          `type`: VertexAttribType,
                          normalized: Boolean,
                          stride: Int,
                          offset: Int): IO[F, Unit]

  def vertexAttribPointer(location: Int,
                          attribType: AttributeType[_],
                          stride: Int,
                          offset: Int): IO[F, Unit] =
    vertexAttribPointer(location,
                        attribType.elementSize,
                        attribType.baseType,
                        false,
                        stride,
                        offset)

  def copyBufferSubData(read: BufferTarget,
                        write: BufferTarget,
                        readOffset: Int,
                        writeOffset: Int,
                        size: Int): IO[F, Unit]

  private def newBuffer(target: BufferTarget, capacity: Int): IO[F, Int] =
    for {
      id <- genBuffer
      _ <- bindBufferState(target, id)
      _ <- bufferData(target, capacity, null, GL_STATIC_DRAW)
    } yield id

  private def makeNewBuffer(buffer: BufferInstance,
                            target: BufferTarget,
                            data: Buffer,
                            size: Int,
                            capacity: Int): IO[F, LoadedBuffer] =
    modify(
        for {
      id <- newBuffer(target, capacity)
      _ <- bufferSubData(target, 0, size, data)
    } yield (LoadedBuffer(buffer, id, capacity, size, target)))(
        GLState.addBuffer)

  private def insertInBuffer(b: LoadedBuffer,
                             data: Buffer,
                             size: Int): IO[F, LoadedBuffer] =
    modify(
        for {
      _ <- bindBufferState(b.target, b.id)
      _ <- bufferSubData(b.target, b.filled, size, data)
    } yield (b.add(size)))(GLState.replaceBuffer(b))

  private def copyToNewBuffer(old: LoadedBuffer,
                              data: Buffer,
                              size: Int,
                              capacity: Int): IO[F, LoadedBuffer] =
    modify(for {
      id <- newBuffer(old.target, capacity)
      _ <- bindBufferState(GL_COPY_READ_BUFFER, old.id)
      _ <- copyBufferSubData(GL_COPY_READ_BUFFER, old.target, 0, 0, old.filled)
      _ <- bufferSubData(old.target, old.filled, size, data)
    } yield
          (LoadedBuffer(old.instance,
                        id,
                        capacity,
                        old.filled + size,
                        old.target)))(GLState.replaceBuffer(old))

  def makeModel(buffer: BufferInstance,
                target: BufferTarget,
                data: Buffer,
                size: Int,
                capacity: Int): IO[F, (LoadedBuffer, (Int, Int))] =
    inspect(GLState.buffer(buffer, target)) >>= {
      case Some(b) =>
        if (b.hasSpace(size))
          insertInBuffer(b, data, size) map (_ -> (b.filled, b.filled + size))
        else
          copyToNewBuffer(b, data, size, capacity) map (_ -> (b.filled, b.filled +
                  size))
      case None =>
        makeNewBuffer(buffer, target, data, size, capacity) map (_ -> (0, size))
    }

  def makeModel(m: Model,
                vData: Buffer,
                vSize: Int,
                eData: Buffer,
                eSize: Int,
                capacity: Int): IO[F, LoadedModel] =
    modify(
        for {
      vb <- makeModel(m.buffer, GL_ARRAY_BUFFER, vData, vSize, capacity)
      eb <- makeModel(
               m.buffer, GL_ELEMENT_ARRAY_BUFFER, eData, eSize, capacity)
    } yield LoadedModel(m, vb._2, eb._2))(GLState.addModel)

  private[kernel] def activeTexture(texture: Int): IO[F, Unit]
  private def activeTexture(texture: Texture): IO[F, Unit] =
    activeTexture(texture)
  private def bindActiveTexture(t: Texture): IO[F, Unit] =
    performNot(GLState.hasActiveTexture(t))(modify(activeTexture(t))(_ =>
              GLState.bind(t)))

  def bindActiveTexture2D(t: Texture, id: Int): IO[F, Unit] =
    for {
      _ <- bindActiveTexture(t)
      _ <- bindTexture2DState(id)
    } yield ()

  def drawElements(mode: PrimitiveType,
                   count: Int,
                   `type`: IndexType,
                   offset: Int): IO[F, Unit]

  def drawElements(m: LoadedModel): IO[F, Unit] =
    drawElements(m.model.buffer.primitiveType,
                 m.numElements,
                 GL_UNSIGNED_INT,
                 m.elementRange._1)

  private[kernel] def uniform1i(location: Int, arg0: Int): IO[F, Unit]
  private[kernel] def uniform1f(location: Int, arg0: Float): IO[F, Unit]
  private[kernel] def uniform2i(
      location: Int, arg0: Int, arg1: Int): IO[F, Unit]
  private[kernel] def uniform2f(
      location: Int, arg0: Float, arg1: Float): IO[F, Unit]
  private[kernel] def uniform3i(
      location: Int, arg0: Int, arg1: Int, arg2: Int): IO[F, Unit]
  private[kernel] def uniform3f(
      location: Int, arg0: Float, arg1: Float, arg2: Float): IO[F, Unit]
  private[kernel] def uniform4i(
      location: Int, arg0: Int, arg1: Int, arg2: Int, arg3: Int): IO[F, Unit]
  private[kernel] def uniform4f(location: Int,
                                arg0: Float,
                                arg1: Float,
                                arg2: Float,
                                arg3: Float): IO[F, Unit]

  def uniform[A](location: Int, value: A)(
      implicit update: CanUniformUpdate[F, A]): IO[F, Unit] =
    update(this)(location, value)

  private[kernel] def clearBufferiv(
      target: Channel, drawBuffer: Int, value: Array[Int]): IO[F, Unit]
  private[kernel] def clearBufferuiv(
      target: Channel, drawBuffer: Int, value: Array[Int]): IO[F, Unit]
  private[kernel] def clearBufferfv(
      target: Channel, drawBuffer: Int, value: Array[Float]): IO[F, Unit]

  def clearBuffer(target: GL_COLOR.type,
                  drawBuffer: DrawBuffer,
                  red: Float,
                  green: Float,
                  blue: Float,
                  alpha: Float): IO[F, Unit] =
    clearBufferfv(target, drawBuffer.n, Array(red, green, blue, alpha))

  def clearBuffer(target: GL_COLOR.type,
                  drawBuffer: DrawBuffer,
                  red: Int,
                  green: Int,
                  blue: Int,
                  alpha: Int): IO[F, Unit] =
    clearBufferiv(target, drawBuffer.n, Array(red, green, blue, alpha))

  def clearBuffer(target: GL_DEPTH.type, value: Float): IO[F, Unit] =
    clearBufferfv(target, 0, Array(value))
  def clearBuffer(target: GL_STENCIL.type, value: Int): IO[F, Unit] =
    clearBufferiv(target, 0, Array(value))

  // TODO: Need to tag unsigned Ints since they are so common!  Perhaps use spire.math.Natural
  def clearBufferu(target: GL_COLOR.type,
                   drawBuffer: DrawBuffer,
                   red: Int,
                   green: Int,
                   blue: Int,
                   alpha: Int): IO[F, Unit] =
    clearBufferuiv(target, drawBuffer.n, Array(red, green, blue, alpha))

  def drawElementsInstanced(mode: PrimitiveType,
                            count: Int,
                            `type`: IndexType,
                            offset: Int,
                            primCount: Int): IO[F, Unit]

  def existingFramebufferId(f: Framebuffer): IO[F, String Xor Int] =
    inspect(GLState.existingFramebufferId(f))

  private def bindCapability(c: Capability, v: Boolean): IO[F, Unit] =
    if (v) bindEnable(c) else bindDisable(c)
  private def bindCapabilities(cs: Map[Capability, Boolean]): IO[F, Unit] =
    keyTraverse(cs.toList) { case (c, v) => bindCapability(c, v) } map (_ =>
          ())

  def existingBuffer(
      i: BufferInstance, t: BufferTarget): IO[F, String Xor LoadedBuffer] =
    inspect(GLState.existingBuffer(i, t))

  def existingModel(m: Model): IO[F, String Xor LoadedModel] =
    inspect(GLState.existingModel(m))

  private def bindAttribute(a: AttributeLocation) =
    for {
      _ <- enableVertexAttribArray(a.location)
      _ <- vertexAttribPointer(a.location, a.aType, a.stride, a.offset)
    } yield ()

  def bindAttributes(p: LoadedProgram,
                     b: LoadedBuffer,
                     m: LoadedModel): IO[F, String Xor Unit] =
    b.attributeLocations(p.attributeLocations, m.vertexRange._1)
      .map(_.map(bindAttribute).sequence.map(_ => ()))
      .sequence

  def draw(d: Draw): IO[F, String Xor Unit] = fromXorT(
      for {
        fid <- toXorT(existingFramebufferId(d.framebuffer))
        _ <- liftXorT(bindFramebufferState(fid))
        _ <- liftXorT(bindCapabilities(d.capabilities))
        _ <- liftXorT(bindColorMask(d.colorMask))
        p <- toXorT(loadedProgram(d.program))
        _ <- liftXorT(bindProgram(p))
        vb <- toXorT(existingBuffer(d.model.buffer, GL_ARRAY_BUFFER))
        _ <- liftXorT(bindBufferState(GL_ARRAY_BUFFER, vb.id))
        eb <- toXorT(existingBuffer(d.model.buffer, GL_ELEMENT_ARRAY_BUFFER))
        _ <- liftXorT(bindBufferState(GL_ELEMENT_ARRAY_BUFFER, eb.id))
        m <- toXorT(existingModel(d.model))
        _ <- toXorT(bindAttributes(p, vb, m))
        _ <- liftXorT(drawElements(m))
      } yield ()
  )
}

@scala.annotation.implicitNotFound("Cannot find update of type $A")
trait CanUniformUpdate[F[_], A] {
  def apply(gl: GL[F])(at: Int, value: A): IO[F, Unit]
}

object CanUniformUpdate {

  implicit def intCanUniformUpdate[F[_]]: CanUniformUpdate[F, Int] =
    new CanUniformUpdate[F, Int] {
      def apply(gl: GL[F])(at: Int, value: Int): IO[F, Unit] =
        gl.uniform1i(at, value)
    }

  implicit def floatCanUniformUpdate[F[_]]: CanUniformUpdate[F, Float] =
    new CanUniformUpdate[F, Float] {
      def apply(gl: GL[F])(at: Int, value: Float): IO[F, Unit] =
        gl.uniform1f(at, value)
    }

  implicit def vec2iCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec2i] =
    new CanUniformUpdate[F, Vec2i] {
      def apply(gl: GL[F])(at: Int, value: Vec2i): IO[F, Unit] =
        gl.uniform2i(at, value.x, value.y)
    }

  implicit def vec3iCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec3i] =
    new CanUniformUpdate[F, Vec3i] {
      def apply(gl: GL[F])(at: Int, value: Vec3i): IO[F, Unit] =
        gl.uniform3i(at, value.x, value.y, value.z)
    }

  implicit def vec4iCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec4i] =
    new CanUniformUpdate[F, Vec4i] {
      def apply(gl: GL[F])(at: Int, value: Vec4i): IO[F, Unit] =
        gl.uniform4i(at, value.x, value.y, value.z, value.w)
    }
  implicit def vec2fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec2f] =
    new CanUniformUpdate[F, Vec2f] {
      def apply(gl: GL[F])(at: Int, value: Vec2f): IO[F, Unit] =
        gl.uniform2f(at, value.x, value.y)
    }

  implicit def vec3fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec3f] =
    new CanUniformUpdate[F, Vec3f] {
      def apply(gl: GL[F])(at: Int, value: Vec3f): IO[F, Unit] =
        gl.uniform3f(at, value.x, value.y, value.z)
    }

  implicit def vec4fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec4f] =
    new CanUniformUpdate[F, Vec4f] {
      def apply(gl: GL[F])(at: Int, value: Vec4f): IO[F, Unit] =
        gl.uniform4f(at, value.x, value.y, value.z, value.w)
    }
/*  implicit def mat2fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Mat2f] =
    new CanUniformUpdate[F, Mat2f] {
      def apply(gl: GL[F])(at: Int, value: Mat2f): IO[F, Unit] =
        gl.uniformMatrix2fv(at, 1, false, value.toArray)
    }
  implicit def mat3fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Mat3f] =
    new CanUniformUpdate[F, Mat3f] {
      def apply(gl: GL[F])(at: Int, value: Mat3f): IO[F, Unit] =
        gl.uniformMatrix3fv(at, 1, false, value.toArray)
    }
  implicit def mat4fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Mat4f] =
    new CanUniformUpdate[F, Mat4f] {
      def apply(gl: GL[F])(at: Int, value: Mat4f): IO[F, Unit] =
        gl.uniformMatrix4fv(at, 1, false, value.toArray)
    }*/
}
