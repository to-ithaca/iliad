package iliad
package gl

import iliad.CatsExtra._

import cats._
import cats.free._, Free._

object Load {
  type DSL[A] = Free[Load, A]

  def parse[F[_]: Monad](i: GL.Interpreter[F]): Load ~> F =
    LoadParser.andThen(GL.interpret(i))

  def apply(s: VertexShader.Source): DSL[VertexShader.Compiled] =
    LoadVertexShader(s).free
  def apply(s: FragmentShader.Source): DSL[FragmentShader.Compiled] =
    LoadFragmentShader(s).free
  def apply(vs: VertexShader.Compiled,
            fs: FragmentShader.Compiled): DSL[Program.Linked] =
    LoadProgram(vs, fs).free

  def create(ref: VertexData.Ref,
             data: VertexData.Data,
             pageSize: Int,
             b: VertexBuffer.Constructor): DSL[VertexBuffer.Update] =
    LoadCreateVertexBuffer(ref, data, pageSize, b).free
  def create(ref: ElementData.Ref,
             data: ElementData.Data,
             pageSize: Int,
             b: ElementBuffer.Constructor): DSL[ElementBuffer.Update] =
    LoadCreateElementBuffer(ref, data, pageSize, b).free
  def insert(ref: VertexData.Ref,
             data: VertexData.Data,
             pageSize: Int,
             b: VertexBuffer.Loaded): DSL[VertexBuffer.Update] =
    LoadInsertVertexBuffer(ref, data, pageSize, b).free
  def insert(ref: ElementData.Ref,
             data: ElementData.Data,
             pageSize: Int,
             b: ElementBuffer.Loaded): DSL[ElementBuffer.Update] =
    LoadInsertElementBuffer(ref, data, pageSize, b).free
  def copy(ref: VertexData.Ref,
           data: VertexData.Data,
           pageSize: Int,
           b: VertexBuffer.Loaded): DSL[VertexBuffer.Update] =
    LoadCopyVertexBuffer(ref, data, pageSize, b).free
  def copy(ref: ElementData.Ref,
           data: ElementData.Data,
           pageSize: Int,
           b: ElementBuffer.Loaded): DSL[ElementBuffer.Update] =
    LoadCopyElementBuffer(ref, data, pageSize, b).free

  def apply(t: Texture.Constructor,
            d: Option[Texture.Data]): DSL[Texture.Loaded] =
    LoadTexture(t, d).free

  def apply(r: Renderbuffer.Constructor): DSL[Renderbuffer.Loaded] =
    LoadRenderbuffer(r).free
  def apply(f: Framebuffer.Constructor,
            as: List[(FramebufferAttachment, Framebuffer.AttachmentLoaded)])
    : DSL[Framebuffer.Loaded] = LoadFramebuffer(f, as).free

  def apply(s: Sampler.Constructor): DSL[Sampler.Loaded] = LoadSampler(s).free
}

sealed trait Load[A]

case class LoadVertexShader(s: VertexShader.Source)
    extends Load[VertexShader.Compiled]
case class LoadFragmentShader(s: FragmentShader.Source)
    extends Load[FragmentShader.Compiled]
case class LoadProgram(vs: VertexShader.Compiled, fs: FragmentShader.Compiled)
    extends Load[Program.Linked]

case class LoadCreateVertexBuffer(ref: VertexData.Ref,
                                  data: VertexData.Data,
                                  pageSize: Int,
                                  b: VertexBuffer.Constructor)
    extends Load[VertexBuffer.Update]
case class LoadCreateElementBuffer(ref: ElementData.Ref,
                                   data: ElementData.Data,
                                   pageSize: Int,
                                   b: ElementBuffer.Constructor)
    extends Load[ElementBuffer.Update]
case class LoadInsertVertexBuffer(ref: VertexData.Ref,
                                  data: VertexData.Data,
                                  pageSize: Int,
                                  b: VertexBuffer.Loaded)
    extends Load[VertexBuffer.Update]
case class LoadInsertElementBuffer(ref: ElementData.Ref,
                                   data: ElementData.Data,
                                   pageSize: Int,
                                   b: ElementBuffer.Loaded)
    extends Load[ElementBuffer.Update]
case class LoadCopyVertexBuffer(ref: VertexData.Ref,
                                data: VertexData.Data,
                                pageSize: Int,
                                b: VertexBuffer.Loaded)
    extends Load[VertexBuffer.Update]
case class LoadCopyElementBuffer(ref: ElementData.Ref,
                                 data: ElementData.Data,
                                 pageSize: Int,
                                 b: ElementBuffer.Loaded)
    extends Load[ElementBuffer.Update]

case class LoadTexture(texture: Texture.Constructor,
                       data: Option[Texture.Data])
    extends Load[Texture.Loaded]

case class LoadRenderbuffer(r: Renderbuffer.Constructor)
    extends Load[Renderbuffer.Loaded]
case class LoadFramebuffer(
    f: Framebuffer.Constructor,
    as: List[(FramebufferAttachment, Framebuffer.AttachmentLoaded)])
    extends Load[Framebuffer.Loaded]
case class LoadSampler(s: Sampler.Constructor) extends Load[Sampler.Loaded]

private object LoadParser extends (Load ~> GL.DSL) {

  private def roundUp(size: Int, baseCapacity: Int): Int =
    Math.ceil(size.toDouble / baseCapacity.toDouble).toInt * baseCapacity

  def apply[A](load: Load[A]): GL.DSL[A] = load match {
    case LoadVertexShader(s) =>
      GL.makeVertexShader(s.text).map(VertexShader.Compiled(_, s))
    case LoadFragmentShader(s) =>
      GL.makeFragmentShader(s.text).map(FragmentShader.Compiled(_, s))
    case LoadProgram(vs, fs) =>
      for {
        id <- GL.makeProgram(vs.id, fs.id)
        _ <- GL.useProgram(id)
        as <- GL.getAttributeLocations(id, vs.source.attributeNames)
        us <- GL.getUniformLocations(id, vs.source.textureNames ++ fs.source.textureNames)
      } yield Program.Linked(id, Program.Unlinked(vs.source, fs.source), as, us)

    case LoadCreateVertexBuffer(r, d, pageSize, b) =>
      val capacity = roundUp(d.size, pageSize)
      GL.makeVertexBuffer(d.data, d.size, capacity)
        .map(
            VertexBuffer.loadNew(_, b, r, d.size, capacity)
        )
    case LoadCreateElementBuffer(r, d, pageSize, b) =>
      val capacity = roundUp(d.size, pageSize)
      GL.makeElementBuffer(d.data, d.size, capacity)
        .map(
            ElementBuffer.loadNew(_, b, r, d.size, capacity)
        )
    case LoadInsertVertexBuffer(r, d, pageSize, b) =>
      GL.insertVertices(b.id, b.filled, d.size, d.data)
        .map(_ => VertexBuffer.insert(b, r, d.size))
    case LoadInsertElementBuffer(r, d, pageSize, b) =>
      GL.insertElements(b.id, b.filled, d.size, d.data)
        .map(_ => ElementBuffer.insert(b, r, d.size))
    case LoadCopyVertexBuffer(r, d, pageSize, b) =>
      val capacity = roundUp(d.size, pageSize)
      GL.copyVertices(b.id, b.filled, d.size, d.data, capacity)
        .map(VertexBuffer.copy(_, b, r, d.size, capacity))
    case LoadCopyElementBuffer(r, d, pageSize, b) =>
      val capacity = roundUp(d.size, pageSize)
      GL.copyElements(b.id, b.filled, d.size, d.data, capacity)
        .map(
            ElementBuffer.copy(_, b, r, d.size, capacity)
        )
    case LoadTexture(t, d) =>
      if (t.isBuffered) GL.makeBufferedTexture(t, d).map {
        case (f, b) => Texture.Loaded(t, f, Some(b))
      } else
        GL.makeSingleTexture(t, d) map (
            (Texture.Loaded(t, _, None))
        )
    case LoadRenderbuffer(r) =>
      GL.makeRenderbuffer(r).map(Renderbuffer.Loaded(r, _))
    case LoadFramebuffer(f, as) =>
      if (f.isBuffered) GL.makeBufferedFramebuffer(as).map {
        case (front, back) => Framebuffer.Loaded(f, front, Some(back))
      } else GL.makeSingleFramebuffer(as).map(Framebuffer.Loaded(f, _, None))
    case LoadSampler(s) => 
      GL.makeSampler(s).map(Sampler.Loaded(s, _))
  }
}
