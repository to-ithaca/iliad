package iliad
package gl

import iliad.CatsExtra._

import cats._
import cats.free._
import cats.implicits._

object Draw {
  type DSL[A] = Free[Draw, A]

  def parse[F[_]: Monad](i: OpenGL.Interpreter[F]): Draw ~> F =
    DrawParser.andThen(OpenGL.interpret(i))

  def bind(f: Framebuffer.Loaded): DSL[Unit] =
    BindFramebuffer(f).free
  def bind(m: ColorMask): DSL[Unit] =
    BindColorMask(m).free
  def clear(m: ChannelBitMask): DSL[Unit] = ClearFrame(m).free
  def use(p: Program.Linked): DSL[Unit] = UseProgram(p).free
  def bind(unit: TextureUnit,
           location: Int,
           t: Texture.Loaded,
           s: Sampler.Loaded): DSL[Unit] =
    BindTextureUniform(unit, location, t, s).free
  def bind(uniform: Uniform.Loaded, value: Uniform.Value): DSL[Unit] =
    BindUniform(uniform, value).free
  def bind(v: VertexBuffer.Loaded): DSL[Unit] = BindVertexBuffer(v).free
  def bind(e: ElementBuffer.Loaded): DSL[Unit] = BindElementBuffer(e).free
  def enable(as: Attribute.LoadedAttributes, baseOffset: Int): DSL[Unit] =
    EnableAttributes(as, baseOffset).free
  def apply(range: DataRange): DSL[Unit] = DrawTriangleModel(range).free
}

sealed trait Draw[A]

case class BindFramebuffer(f: Framebuffer.Loaded) extends Draw[Unit]
case class BindColorMask(c: ColorMask) extends Draw[Unit]
case class Enable(c: Capability) extends Draw[Unit]
case class Disable(c: Capability) extends Draw[Unit]
case class ClearFrame(bitMask: ChannelBitMask) extends Draw[Unit]
case class UseProgram(p: Program.Linked) extends Draw[Unit]
case class BindTextureUniform(unit: TextureUnit,
                              location: Int,
                              t: Texture.Loaded,
                              s: Sampler.Loaded)
    extends Draw[Unit]
case class BindUniform(u: Uniform.Loaded, v: Uniform.Value) extends Draw[Unit]
case class BindVertexBuffer(v: VertexBuffer.Loaded) extends Draw[Unit]
case class BindElementBuffer(e: ElementBuffer.Loaded) extends Draw[Unit]
case class EnableAttributes(as: Attribute.LoadedAttributes, baseOffset: Int)
    extends Draw[Unit]

case class DrawTriangleModel(range: DataRange) extends Draw[Unit]

object DrawParser extends (Draw ~> OpenGL.DSL) {

  private def enableAttribute(stride: Int)(
      a: Attribute.Offset): OpenGL.DSL[Unit] =
    a.loaded.constructor.`type` match {
      case it: VertexAttribIType =>
        OpenGL.enableAttributeI(a.loaded.location,
                                a.loaded.constructor.elementSize,
                                it,
                                stride,
                                a.offset)
      case t: VertexAttribType =>
        OpenGL.enableAttribute(a.loaded.location,
                               a.loaded.constructor.elementSize,
                               a.loaded.constructor.`type`,
                               stride,
                               a.offset)
    }

  def apply[A](draw: Draw[A]): OpenGL.DSL[A] = draw match {
    case BindFramebuffer(f) =>
      OpenGL.bindFramebuffer(f.frontId)
    case BindColorMask(m) =>
      OpenGL.colorMask(m.r, m.g, m.b, m.a)
    case Enable(c) =>
      OpenGL.enable(c)
    case Disable(c) =>
      OpenGL.disable(c)
    case ClearFrame(bitMask) => OpenGL.clear(bitMask)
    case UseProgram(p) => OpenGL.useProgram(p.id)
    case BindTextureUniform(unit, location, t, s) =>
      OpenGL.bindTextureUniform(unit, location, t.frontId, s.id)
    case BindUniform(uniform, value) =>
      value.bind(uniform.location)
    case BindVertexBuffer(v) => OpenGL.bindVertexBuffer(v.id)
    case BindElementBuffer(e) => OpenGL.bindElementBuffer(e.id)
    case EnableAttributes(as, baseOffset) =>
      as.offsets(baseOffset).traverse(enableAttribute(as.stride)).map(_ => ())
    case DrawTriangleModel(range: DataRange) =>
      OpenGL.drawTriangles(range.start, range.end)
  }
}
