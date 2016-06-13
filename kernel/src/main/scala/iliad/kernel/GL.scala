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
/**
  Typesafe GL. The type parameter `F` has type constraints to compose operations.
 */
abstract class GL[F[_]: Monad] {
 
  private def modify[A](io: IO[F, A])(f: A => GLState => GLState): IO[F, A] = io.mapF(_.transform { case (s, a) => (f(a)(s), a) })
  private def inspect[A](f: GLState => A): IO[F, A] = ReaderT(_ => State.inspect(f).transformF(s => Applicative[F].pure(s.value)))
  private def createIfNotExists[A](opt: IO[F, Option[A]])(create: IO[F, A]): IO[F, A] = opt >>= {
    case Some(a) => ReaderT.pure(a)
    case None => create
  }

  private def keyTraverse[A, B, G[_] : Traverse](keys: G[A])(f: A => IO[F, B]): IO[F, G[(A, B)]] =  keys.traverse[IO[F, ?], (A, B)](s => f(s).map(s -> _))

  private def _tup1[A](arr: Array[A]): A = arr(0)
  private def _tup2[A](arr: Array[A]): (A, A) = (arr(0), arr(1))
  private def _tup3[A](arr: Array[A]): (A, A, A) = (arr(0), arr(1), arr(2))

  def blitFramebuffer(src: Rect[Int], dest: Rect[Int], bitMask: ChannelBitMask, filter: BlitFilter): IO[F, Unit]
  def viewport(rect: Rect[Int]): IO[F, Unit]
  def flush: IO[F, Unit]
  def clear(bitMask: ChannelBitMask): IO[F, Unit]
  def clearColor(red: Float, green: Float, blue: Float, alpha: Float): IO[F, Unit]
  def colorMask(red: Boolean, green: Boolean, blue: Boolean, alpha: Boolean): IO[F, Unit]
  def enable(cap: Capability): IO[F, Unit]
  def disable(cap: Capability): IO[F, Unit]
  def getError: IO[F, Option[Int Xor ErrorCode]]
  
  private[kernel] def createShader(`type`: ShaderType): IO[F, Int]
  private[kernel] def shaderSource(shader: Int, count: Int, sources: Seq[String]): IO[F, Unit]
  private[kernel] def shaderSource(shader: Int, source: String): IO[F, Unit] = shaderSource(shader, 1, Seq(source))
 
  private[kernel] def compileShader(shader: Int): IO[F, Unit]
  private[kernel] def loadShader(`type`: ShaderType, source: String): IO[F, Int] = createShader(`type`) >>= { id => 
    shaderSource(id, source) >> compileShader(id) map (_ => id)
  }

  private[kernel] def createVertexShader(shader: VertexShader): IO[F, Loaded[VertexShader]] = modify(loadShader(GL_VERTEX_SHADER, shader.source) map (Loaded(_, shader)))(vsh => _vertexShaders.modify(vsh :: _))
  private[kernel] def createFragmentShader(shader: FragmentShader): IO[F, Loaded[FragmentShader]] = modify(loadShader(GL_FRAGMENT_SHADER, shader.source) map (Loaded(_, shader)))(fsh => _fragmentShaders.modify(fsh :: _))

  private[kernel] def existingVertexShader(vsh: VertexShader): IO[F, Option[Loaded[VertexShader]]] = inspect(s => _vertexShaders.get(s).find(_.glObject == vsh))
  private[kernel] def existingFragmentShader(fsh: FragmentShader): IO[F, Option[Loaded[FragmentShader]]] = inspect(s => _fragmentShaders.get(s).find(_.glObject == fsh))

  private[kernel] def vertexShader(shader: VertexShader): IO[F, Loaded[VertexShader]] = createIfNotExists(existingVertexShader(shader))(createVertexShader(shader))
  private[kernel] def fragmentShader(shader: FragmentShader): IO[F, Loaded[FragmentShader]] = createIfNotExists(existingFragmentShader(shader))(createFragmentShader(shader))

  def deleteShader(shader: Int): IO[F, Unit]

  private[kernel] def createProgram: IO[F, Int]
  private[kernel] def attachShader(program: Int, shader: Int): IO[F, Unit]
  private[kernel] def linkProgram(program: Int): IO[F, Unit]

  def getAttribLocation(program: Int, name: String): IO[F, Int]
  def getAttribLocation[G[_] : Traverse](program: Int, names: G[(String, AttributeType[_])]): IO[F,  G[((String, AttributeType[_]), Int)]] = keyTraverse(names)(n => getAttribLocation(program, n._1))
  def getUniformLocation(program: Int, name: String): IO[F, Int]
  def getUniformLocations[G[_] : Traverse, A](program: Int, names: G[(String, A)]): IO[F, G[((String, A), Int)]] = keyTraverse(names)(n => getUniformLocation(program, n._1))

  private[kernel] def createProgram(program: Program): IO[F, LoadedProgram] = modify( for {
    id <- createProgram
    vertex <- vertexShader(program.vertex)
    fragment <- fragmentShader(program.fragment)
    _ <- attachShader(id, vertex.id)
    _ <- attachShader(id, fragment.id)
    _ <- linkProgram(id)
    _ <- samplers(program.samplers)
    p <- getAttribLocation(id, program.attributes) |@| getUniformLocations(id, program.uniforms) |@| getUniformLocations(id, program.textures)  map ( (as, us, ts) => 
      LoadedProgram(program, id, vertex, fragment, as, us, ts))
  } yield p)(p => _programs.modify(p :: _))

  private[kernel] def existingProgram(program: Program): IO[F, Option[LoadedProgram]] = inspect(s => _programs.get(s).find(p => p.program == program))

  def program(program: Program) = createIfNotExists(existingProgram(program))(createProgram(program))

  private[kernel] def getShaderiv(shader: Int, pname: ShaderParameter): IO[F, Int]
  
  private def getShaderiv[A](shader: Int, pname: ShaderParameter, expected: Set[A])(eq: A => Int): IO[F, A] = getShaderiv(shader, pname).map(code => expected.find(eq(_) == code).get)

  def getShaderiv(shader: Int, pname: GL_SHADER_TYPE.type): IO[F, ShaderType] = getShaderiv(shader, pname, SealedEnum.values[ShaderType])(_.value)
  def getShaderiv(shader: Int, pname: GL_DELETE_STATUS.type): IO[F, Boolean] = getShaderiv(shader, pname, SealedEnum.values[TrueFalse])(_.value).map(_ == GL_TRUE)
  def getShaderiv(shader: Int, pname: GL_COMPILE_STATUS.type): IO[F, Boolean] = getShaderiv(shader, pname, SealedEnum.values[TrueFalse])(_.value).map(_ == GL_TRUE)
  def getShaderiv(shader: Int, pname: GL_INFO_LOG_LENGTH.type): IO[F, Int] = getShaderiv(shader, pname: ShaderParameter)
  def getShaderiv(shader: Int, pname: GL_SHADER_SOURCE_LENGTH.type): IO[F, Int] = getShaderiv(shader, pname: ShaderParameter)

  private[kernel] def getShaderInfoLog(shader: Int, maxLength: Int): IO[F, String]
  def getShaderInfoLog(shader: Int): IO[F, String] = getShaderiv(shader, GL_SHADER_SOURCE_LENGTH) >>= (getShaderInfoLog(shader, _))

  def useProgram(program: Int): IO[F, Unit]

  private[kernel] def genSamplers(num: Int): IO[F, Array[Int]]
  def genSampler: IO[F, Int] = genSamplers(1) map _tup1
  def genSampler2: IO[F, (Int, Int)] = genSamplers(2) map _tup2
  def genSampler3: IO[F, (Int, Int, Int)] = genSamplers(3) map _tup3

  private[kernel] def samplerParameteri(sampler: Int, name: SamplerParameter, value: Int): IO[F, Unit]
  private[kernel] def samplerParameteri(sampler: Int, name: SamplerParameter, value: IntConstant): IO[F, Unit] = samplerParameteri(sampler, name, value.value)
  
  private[kernel] def samplerParameter(sampler: Int, name: GL_TEXTURE_MIN_FILTER.type, value: TextureMinFilter): IO[F, Unit] = samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(sampler: Int, name: GL_TEXTURE_WRAP_S.type, value: TextureWrap): IO[F, Unit] = samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(sampler: Int, name: GL_TEXTURE_WRAP_T.type, value: TextureWrap): IO[F, Unit] = samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(sampler: Int, name: GL_TEXTURE_WRAP_R.type, value: TextureWrap): IO[F, Unit] = samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(sampler: Int, name: GL_TEXTURE_MAG_FILTER.type, value: TextureMagFilter): IO[F, Unit] = samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(sampler: Int, name: GL_TEXTURE_MIN_LOD.type, value: Int): IO[F, Unit] = samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(sampler: Int, name: GL_TEXTURE_MAX_LOD.type, value: Int): IO[F, Unit] = samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(sampler: Int, name: GL_TEXTURE_COMPARE_MODE.type, value: TextureCompareMode): IO[F, Unit] = samplerParameteri(sampler, name, value)
  private[kernel] def samplerParameter(sampler: Int, name: GL_TEXTURE_COMPARE_FUNC.type, value: TextureCompareFunc): IO[F, Unit] = samplerParameteri(sampler, name, value)

  private[kernel] def createSampler(sampler: Sampler): IO[F, Loaded[Sampler]] = modify(genSampler >>= { id => 
    samplerParameter(id, GL_TEXTURE_MIN_FILTER, sampler.minFilter) >>
    samplerParameter(id, GL_TEXTURE_MAG_FILTER, sampler.magFilter) >>
    samplerParameter(id, GL_TEXTURE_WRAP_S, sampler.wrapS) >>
    samplerParameter(id, GL_TEXTURE_WRAP_T, sampler.wrapT) map (_ => Loaded(id, sampler))
  })(s =>_samplers.modify(s :: _))

  private[kernel] def existingSampler(s: Sampler): IO[F, Option[Loaded[Sampler]]] = inspect(s => _samplers.get(s).find(_.glObject == s))
  private[kernel] def sampler(s: Sampler): IO[F, Loaded[Sampler]] = createIfNotExists(existingSampler(s))(createSampler(s))

  def samplers(ss: List[Sampler]): IO[F, List[Loaded[Sampler]]] = keyTraverse(ss)(sampler) map (_.map(_._2))

  private[kernel] def genRenderbuffers(num: Int): IO[F, Array[Int]]
  private[kernel] def genRenderbuffer: IO[F, Int] = genRenderbuffers(1) map _tup1
  def genRenderbuffer2: IO[F, (Int, Int)] = genRenderbuffers(2) map _tup2
  def genRenderbuffer3: IO[F, (Int, Int, Int)] = genRenderbuffers(3) map _tup3
  private[kernel] def bindRenderbuffer(renderbuffer: Int): IO[F, Unit]
  private[kernel] def renderbufferStorage(format: RenderbufferInternalFormat, width: Int, height: Int): IO[F, Unit]

  private[kernel] def createRenderbuffer(r: RenderbufferInstance): IO[F, LoadedRenderbuffer] = modify(genRenderbuffer >>= { id => 
    bindRenderbuffer(id) >> renderbufferStorage(r.internalFormat, r.viewport.width, r.viewport.height) map (_ => LoadedRenderbuffer(r, id))
  })(r => _renderbuffers.modify(r :: _))

  private[kernel] def existingRenderbuffer(r: RenderbufferInstance): IO[F, Option[LoadedRenderbuffer]] = inspect(s => _renderbuffers.get(s).find(_.instance == r))
  def renderbuffer(r: RenderbufferInstance): IO[F, LoadedRenderbuffer] = createIfNotExists(existingRenderbuffer(r))(createRenderbuffer(r))

  private[kernel] def bindTexture(target: TextureTarget, texture: Int): IO[F, Unit]
  def bindTexture2D(texture: Int): IO[F, Unit] = bindTexture(GL_TEXTURE_2D, texture)

  private[kernel] def genTextures(num: Int): IO[F, Array[Int]]
  def genTexture: IO[F, Int] = genTextures(1) map _tup1
  def genTexture2: IO[F, (Int, Int)] = genTextures(2) map _tup2
  def genTexture3: IO[F, (Int, Int, Int)] = genTextures(3) map _tup3

  private[kernel] def texParameteri(target: TextureTarget, name: TextureParameter, value: Int): IO[F, Unit]
  private[kernel] def texParameteri(target: TextureTarget, name: TextureParameter, value: IntConstant): IO[F, Unit]
  def texParameter(target: TextureTarget, name: GL_TEXTURE_BASE_LEVEL.type, value: Int): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_COMPARE_FUNC.type, value: TextureCompareFunc): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_COMPARE_MODE.type, value: TextureCompareMode): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_MIN_FILTER.type, value: TextureMinFilter): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_MAG_FILTER.type, value: TextureMagFilter): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_MIN_LOD.type, value: Int): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_MAX_LOD.type, value: Int): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_MAX_LEVEL.type, value: Int): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_SWIZZLE_R.type, value: TextureSwizzle): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_SWIZZLE_G.type, value: TextureSwizzle): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_SWIZZLE_B.type, value: TextureSwizzle): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_SWIZZLE_A.type, value: TextureSwizzle): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_WRAP_S.type, value: TextureWrap): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_WRAP_T.type, value: TextureWrap): IO[F, Unit] = texParameteri(target, name, value)
  def texParameter(target: TextureTarget, name: GL_TEXTURE_WRAP_R.type, value: TextureWrap): IO[F, Unit] = texParameteri(target, name, value)

  private[kernel] def texImage2D(target: TextureTarget, level: Int, internalFormat: TextureInternalFormat, width: Int, height: Int, format: TextureFormat, `type`: TexturePixelType, data: Buffer): IO[F, Unit]
  private [kernel] def texImage2D(data: TextureData): IO[F, Unit] = texImage2D(GL_TEXTURE_2D, 0, data.texture.template.internalFormat, data.texture.template.viewport.width, data.texture.template.viewport.height, data.texture.template.format, data.texture.template.`type`, data.data.getOrElse(null))

  private[kernel] def loadTexture(data: TextureData): IO[F, Int] = genTexture >>= { id => 
    bindTexture2D(id) >> texImage2D(data) map (_ => id)
  }

  private[kernel] def createSingleTexture(data: TextureData): IO[F, LoadedTexture] = loadTexture(data) map (id => LoadedTexture(data.texture, BufferedId.Single(id)))
  private[kernel] def createDoubleTexture(data: TextureData): IO[F, LoadedTexture] = for {
    front <- loadTexture(data)
    back <- loadTexture(data)
  } yield LoadedTexture(data.texture, BufferedId.Double(front, back))


  private[kernel] def texture(data: TextureData): IO[F, LoadedTexture] = modify(if(data.texture.template.isDouble) createDoubleTexture(data) else createSingleTexture(data))(t => _textures.modify(t :: _))

  private[kernel] def existingTexture(texture: TextureInstance): IO[F, Option[LoadedTexture]] = inspect(s => _textures.get(s).find(t => t.instance == texture))
  private[kernel] def getExistingTexture(texture: TextureInstance): IO[F, LoadedTexture] = existingTexture(texture).map(_.get)

  private[kernel] def genFramebuffers(num: Int): IO[F,Array[Int]]
  def genFramebuffer: IO[F, Int] = genFramebuffers(1) map _tup1
  def genFramebuffer2(num: Int): IO[F, (Int, Int)] = genFramebuffers(2) map _tup2
  def genFramebuffer3(num: Int): IO[F, (Int, Int, Int)] = genFramebuffers(3) map _tup3
  def bindFramebuffer(target: FramebufferTarget, framebuffer: Int): IO[F, Unit]
  private[kernel] def framebufferRenderbuffer(target: FramebufferTarget, attachment: FramebufferAttachment, renderbuffer: Int): IO[F, Unit]
  private[kernel] def framebufferRenderbuffer(attachment: FramebufferAttachment, renderbuffer: Int): IO[F, Unit] = framebufferRenderbuffer(GL_FRAMEBUFFER, attachment, renderbuffer)
  def checkFramebufferStatus(target: FramebufferTarget): IO[F, FramebufferStatus]
  private[kernel] def framebufferTexture2D(target: FramebufferTarget, attachment: FramebufferAttachment, texTarget: FramebufferTexTarget, texture: Int, level: Int): IO[F, Unit]
  private[kernel] def framebufferTexture2D(attachment: FramebufferAttachment, texture: Int): IO[F, Unit] = framebufferTexture2D(GL_FRAMEBUFFER, attachment, GL_TEXTURE_2D, texture, 0)
  private[kernel] def bindAttachment(a: FramebufferAttachment, output: LoadedShaderOutput): IO[F, Unit] = output match {
    case LoadedTexturePage(t, id) => framebufferTexture2D(a, id)
    case LoadedRenderbuffer(r, id) => framebufferRenderbuffer(a, id)
  }

  private[kernel] def bindAttachments[G[_]: Traverse](as: G[(FramebufferAttachment, LoadedShaderOutput)]) = keyTraverse(as)(a => bindAttachment(a._1, a._2))
  private[kernel] def drawBuffers(num: Int, buffers: Seq[ColorOutputTarget]): IO[F, Unit]
  private[kernel] def drawBuffers(buffers: Seq[ColorOutputTarget]): IO[F, Unit] = drawBuffers(buffers.size, buffers)

  private[kernel] def createFramebuffer(framebuffer: Framebuffer, outputs: List[LoadedShaderOutput]): IO[F, Int] = genFramebuffer >>= { id => 
    bindFramebuffer(GL_FRAMEBUFFER, id) >>
    bindAttachments(framebuffer.attachments(outputs)) >>
    drawBuffers(framebuffer.drawBuffers) map (_ => id)
  }

  private[kernel] def existingFramebuffer(f: Framebuffer): IO[F, Option[LoadedFramebuffer]] = inspect(s => _framebuffers.get(s).find(ff => ff.framebuffer == f))

  private[kernel] def loadedOutput(page: LoadedTexture => LoadedTexturePage)(o: ShaderOutputInstance): IO[F, LoadedShaderOutput] = o match {
    case t: TextureInstance => getExistingTexture(t).map(page)
    case r: RenderbufferInstance => renderbuffer(r).map(identity)
  }

  private[kernel] def loadedOutputs(page: LoadedTexture => LoadedTexturePage)(os: List[ShaderOutputInstance]): IO[F, List[LoadedShaderOutput]] = keyTraverse(os)(loadedOutput(page)) map (_.map(_._2))
  
  private[kernel] def createFramebuffer(page: LoadedTexture => LoadedTexturePage)(f: Framebuffer): IO[F, Int] = loadedOutputs(page)(f.outputs.map(_._2)) >>= { os =>
    createFramebuffer(f, os)
  }

  private[kernel] def createFramebuffer(f: Framebuffer): IO[F, LoadedFramebuffer] = modify(if(f.isDouble) for {
    front <- createFramebuffer(_.frontPage)(f)
    back <- createFramebuffer(_.backPage)(f)
  } yield LoadedFramebuffer(f, BufferedId.Double(front, back))
  else createFramebuffer(_.frontPage)(f) map(id => LoadedFramebuffer(f, BufferedId.Single(id)))
  )(f => _framebuffers.modify(f :: _))

  def framebuffer(f: Framebuffer): IO[F, LoadedFramebuffer] = createIfNotExists(existingFramebuffer(f))(createFramebuffer(f))


  //TODO: Work out how best to expose N buffers safely
  private[kernel] def genBuffers(num: Int): IO[F, Array[Int]]
  def genBuffer: IO[F, Int] = genBuffers(1) map _tup1
  def genBuffer2: IO[F, (Int, Int)] = genBuffers(2) map _tup2
  def genBuffer3: IO[F, (Int, Int, Int)] = genBuffers(3) map _tup3 
  def bindBuffer(target: BufferTarget, buffer: Int): IO[F , Unit]
  def bufferData(target: BufferTarget, size: Int, data: Buffer, usage: BufferUsage): IO[F, Unit]
  def bufferSubData(target: BufferTarget, offset: Int, size: Int, data: Buffer): IO[F, Unit]
  def enableVertexAttribArray(location: Int): IO[F, Unit]
  def vertexAttribPointer(location: Int, size: Int,`type`: VertexAttribType, normalized: Boolean, stride: Int, offset: Int): IO[F, Unit]

  def copyBufferSubData(read: BufferTarget, write: BufferTarget, readOffset: Int, writeOffset: Int, size: Int): IO[F, Unit]

  private def newBuffer(target: BufferTarget, capacity: Int): IO[F, Int] = genBuffer >>= { id =>
    bindBuffer(target, id) >> bufferData(target, capacity, null, GL_STATIC_DRAW) map (_ => id)
  }

  private def initialBuffer(buffer: BufferInstance, target: BufferTarget, data: Buffer, size: Int, capacity: Int): IO[F, LoadedBuffer] = modify(newBuffer(target, capacity) >>= { id =>
    bufferSubData(target, 0, size, data) map (_ => LoadedBuffer(buffer, id, capacity, size, target))
  })(b => _buffers.modify(b :: _))

  private def insertInBuffer(buffer: LoadedBuffer, data: Buffer, size: Int): IO[F, LoadedBuffer] = modify(bindBuffer(buffer.target, buffer.id) >> 
    bufferSubData(buffer.target, buffer.filled, size, data) map (_ => buffer.add(size)))(b => _buffers.modify(_.map { 
      case bb if (bb == buffer) => b
      case x => x
}))

  private def copyToNew(buffer: LoadedBuffer, data: Buffer, size: Int, capacity: Int): IO[F, LoadedBuffer] = modify(newBuffer(buffer.target, capacity) >>= { id =>
    bindBuffer(GL_COPY_READ_BUFFER, buffer.id) >>
    copyBufferSubData(GL_COPY_READ_BUFFER, buffer.target, 0, 0, buffer.filled) >>
    bufferSubData(buffer.target, buffer.filled, size, data) map (_ => LoadedBuffer(buffer.instance, id, capacity, buffer.filled + size, buffer.target)
  )})(b => _buffers.modify(_.map {
    case bb if (bb == buffer) => b
    case x => x
  }))

  private def existingBuffer(bufferInstance: BufferInstance, target: BufferTarget): IO[F, Option[LoadedBuffer]] = inspect(s =>_buffers.get(s).find(b => b.instance == bufferInstance && b.target == target))
  
  def addModel(buffer: BufferInstance, target: BufferTarget, data: Buffer, size: Int, capacity: Int): IO[F, LoadedBuffer] = existingBuffer(buffer, target) >>= {
    case Some(b) => if(b.hasSpace(size)) insertInBuffer(b, data, size) else copyToNew(b, data, size, capacity)
    case None => initialBuffer(buffer, target, data, size, capacity)
  }
  
  def pixelStorei(name: PixelStoreParameter, value: Int): IO[F, Unit]

  def activeTexture(texture: Int): IO[F, Unit]
  def drawArrays(mode: PrimitiveType, first: Int, count: Int): IO[F, Unit]
  def drawElements(mode: PrimitiveType, count: Int, `type`: IndexType, offset: Int): IO[F, Unit]
  def drawElements(mode: PrimitiveType, count: Int, `type`: IndexType, indices: Buffer): IO[F, Unit]

  private[kernel] def uniform1i(location: Int, arg0: Int): IO[F, Unit]
  private[kernel] def uniform1f(location: Int, arg0: Float): IO[F, Unit]
  private[kernel] def uniform1fv(location: Int, count: Int, arr: Array[Float]): IO[F, Unit]
  private[kernel] def uniform1iv(location: Int, count: Int, arr: Array[Int]): IO[F, Unit]
  private[kernel] def uniform2i(location: Int, arg0: Int, arg1: Int): IO[F, Unit]
  private[kernel] def uniform2f(location: Int, arg0: Float, arg1: Float): IO[F, Unit]
  private[kernel] def uniform2fv(location: Int, count: Int, arr: Array[Float]): IO[F, Unit]
  private[kernel] def uniform2iv(location: Int, count: Int, arr: Array[Int]): IO[F, Unit]
  private[kernel] def uniform3i(location: Int, arg0: Int, arg1: Int, arg2: Int): IO[F, Unit]
  private[kernel] def uniform3f(location: Int, arg0: Float, arg1: Float, arg2: Float): IO[F, Unit]
  private[kernel] def uniform3fv(location: Int, count: Int, arr: Array[Float]): IO[F, Unit]
  private[kernel] def uniform3iv(location: Int, count: Int, arr: Array[Int]): IO[F, Unit]
  private[kernel] def uniform4i(location: Int, arg0: Int, arg1: Int, arg2: Int, arg3: Int): IO[F, Unit]
  private[kernel] def uniform4f(location: Int, arg0: Float, arg1: Float, arg2: Float, arg3: Float): IO[F, Unit]
  private[kernel] def uniform4fv(location: Int, count: Int, arr: Array[Float]): IO[F, Unit]
  private[kernel] def uniform4iv(location: Int, count: Int, arr: Array[Int]): IO[F, Unit]
  private[kernel] def uniformMatrix2fv(location: Int, count: Int, transpose: Boolean, arg0: Array[Float]): IO[F, Unit]
  private[kernel] def uniformMatrix3fv(location: Int, count: Int, transpose: Boolean, arg0: Array[Float]): IO[F, Unit]
  private[kernel] def uniformMatrix4fv(location: Int, count: Int, transpose: Boolean, arg0: Array[Float]): IO[F, Unit]

  def uniform[A](location: Int, value: A)(implicit update: CanUniformUpdate[F, A]): IO[F, Unit] = update(this)(location, value)

  def bindVertexArray(vertexArray: Int): IO[F, Unit]

  private[kernel] def clearBufferfi(target: Channel, drawBuffer: Int, depth: Float, stencil: Int): IO[F, Unit]
  private[kernel] def clearBufferiv(target: Channel, drawBuffer: Int, value: Array[Int]): IO[F, Unit]
  private[kernel] def clearBufferuiv(target: Channel, drawBuffer: Int, value: Array[Int]): IO[F, Unit]
  private[kernel] def clearBufferfv(target: Channel, drawBuffer: Int, value: Array[Float]): IO[F, Unit]

  def clearBuffer(target: GL_COLOR.type, drawBuffer: DrawBuffer, red: Float, green: Float, blue: Float, alpha: Float): IO[F, Unit] = clearBufferfv(target, drawBuffer.n, Array(red, green, blue, alpha))
  def clearBuffer(target: GL_COLOR.type, drawBuffer: DrawBuffer, red: Int, green: Int, blue: Int, alpha: Int): IO[F, Unit] = clearBufferiv(target, drawBuffer.n, Array(red, green, blue, alpha))

  def clearBuffer(target: GL_DEPTH.type, value: Float): IO[F, Unit] = clearBufferfv(target, 0, Array(value))
  def clearBuffer(target: GL_STENCIL.type, value: Int): IO[F, Unit] = clearBufferiv(target, 0, Array(value))
  def clearBuffer(target: GL_DEPTH_STENCIL.type, depth: Float, stencil: Int): IO[F, Unit] = clearBufferfi(target, 0, depth, stencil)
  
  // TODO: Need to tag unsigned Ints since they are so common!  Perhaps use spire.math.Natural
  def clearBufferu(target: GL_COLOR.type, drawBuffer: DrawBuffer, red: Int, green: Int, blue: Int, alpha: Int): IO[F, Unit] = clearBufferuiv(target, drawBuffer.n, Array(red, green, blue, alpha))

  def readBuffer(src: DrawBuffer): IO[F, Unit]
  def drawElementsInstanced(mode: PrimitiveType, count: Int, `type`: IndexType, offset: Int, primCount: Int): IO[F, Unit]
  def drawElementsInstanced(mode: PrimitiveType, count: Int, `type`: IndexType, ptr: Buffer, primCount: Int): IO[F, Unit]
  def bindAttribLocation(program: Int, index: Int, name: String): IO[F, Unit]
  def blendColor(red: Float, green: Float, blue: Float, alpha: Float): IO[F, Unit]
}

@scala.annotation.implicitNotFound("Cannot find update of type $A")
trait CanUniformUpdate[F[_], A] {
  def apply(gl: GL[F])(at: Int, value: A): IO[F, Unit]
}

object CanUniformUpdate {

  implicit def intCanUniformUpdate[F[_]]: CanUniformUpdate[F, Int] = new CanUniformUpdate[F, Int] {
    def apply(gl: GL[F])(at: Int, value: Int): IO[F, Unit] = gl.uniform1i(at, value)
  }

  implicit def floatCanUniformUpdate[F[_]]: CanUniformUpdate[F, Float] = new CanUniformUpdate[F, Float] {
    def apply(gl: GL[F])(at: Int, value: Float): IO[F, Unit] = gl.uniform1f(at, value)
  }

  implicit def vec2iCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec2i] = new CanUniformUpdate[F, Vec2i] {
    def apply(gl: GL[F])(at: Int, value: Vec2i): IO[F, Unit] = gl.uniform2i(at, value.x, value.y)
  }

  implicit def vec3iCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec3i] = new CanUniformUpdate[F, Vec3i] {
    def apply(gl: GL[F])(at: Int, value: Vec3i): IO[F, Unit] = gl.uniform3i(at, value.x, value.y, value.z)
  }

  implicit def vec4iCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec4i] = new CanUniformUpdate[F, Vec4i] {
    def apply(gl: GL[F])(at: Int, value: Vec4i): IO[F, Unit] = gl.uniform4i(at, value.x, value.y, value.z, value.w)
  }
  implicit def vec2fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec2f] = new CanUniformUpdate[F, Vec2f] {
    def apply(gl: GL[F])(at: Int, value: Vec2f): IO[F, Unit] = gl.uniform2f(at, value.x, value.y)
  }

  implicit def vec3fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec3f] = new CanUniformUpdate[F, Vec3f] {
    def apply(gl: GL[F])(at: Int, value: Vec3f): IO[F, Unit] = gl.uniform3f(at, value.x, value.y, value.z)
  }

  implicit def vec4fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Vec4f] = new CanUniformUpdate[F, Vec4f] {
    def apply(gl: GL[F])(at: Int, value: Vec4f): IO[F, Unit] = gl.uniform4f(at, value.x, value.y, value.z, value.w)
  }
  implicit def mat2fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Mat2f] = new CanUniformUpdate[F, Mat2f] {
    def apply(gl: GL[F])(at: Int, value: Mat2f): IO[F, Unit] = gl.uniformMatrix2fv(at, 1, false, value.toArray)
  }
  implicit def mat3fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Mat3f] = new CanUniformUpdate[F, Mat3f] {
    def apply(gl: GL[F])(at: Int, value: Mat3f): IO[F, Unit] = gl.uniformMatrix3fv(at, 1, false, value.toArray)
  }
  implicit def mat4fCanUniformUpdate[F[_]]: CanUniformUpdate[F, Mat4f] = new CanUniformUpdate[F, Mat4f] {
    def apply(gl: GL[F])(at: Int, value: Mat4f): IO[F, Unit] = gl.uniformMatrix4fv(at, 1, false, value.toArray)
  }
}


object MissingCatsImplicits {
  implicit def xorTApplicative[F[_], L](implicit F: Applicative[F]): Applicative[XorT[F, L, ?]] = new Applicative[XorT[F, L, ?]] {
    def pure[A](a: A): XorT[F, L, A] = XorT(F.pure(Xor.right(a)))
    def ap[A, B](ff: XorT[F, L, A => B])(fa: XorT[F, L, A]): XorT[F, L, B] = {
      XorT(F.ap(ff.value.map {
        case l @ Xor.Left(_) => (_: L Xor A) => l
        case Xor.Right(f) => (xa: L Xor A) => xa.map(f)   
      })(fa.value))
    }
  }

  implicit def xorTMonoid[F[_], L, A](implicit F: Applicative[F], M: Monoid[A]): Monoid[XorT[F, L, A]] = new Monoid[XorT[F, L ,A]] {
    def empty = XorT(F.pure(M.empty.right[L]))
    def combine(xx: XorT[F, L ,A], xy: XorT[F, L , A]): XorT[F, L , A] = {
      XorT(F.ap(xx.value.map {
        case l @ Xor.Left(_) => (_: L Xor A) => l
        case Xor.Right(x) => (xy: L Xor A) => xy.map(x |+| _)   
      })(xy.value))
    }}
}

import MissingCatsImplicits._

object GL {
  val run: GL[Id] = GLRunner
  
  def log(logCfg: LoggerConfig): GL[LogEffect[Id, ?]] = new GLLogger(logCfg, run)
  def debugAndLog(debugCfg: DebuggerConfig, logCfg: LoggerConfig): GL[DebugEffect[LogEffect[Id, ?], ?]] = new GLDebugger[LogEffect[Id, ?]](log(logCfg))
 
  /** Logger config for specifying which methods to log and verbosity */
  case class LoggerConfig(s: Set[String])


  /** Debugging config for specifying which methods to debug */
  case class DebuggerConfig(filtered: Set[String])

  type IO[F[_], A] = ReaderT[StateT[F, GLState, ?], GLES30Library, A]
  type LogEffect[F[_], A] = WriterT[F, List[String], A]
  type DebugEffect[F[_], A] = XorT[F, String, A]
  type StateEffect[F[_], A] = StateT[F, GLState, A]
}
