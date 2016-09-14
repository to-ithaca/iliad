package iliad
package gl

import iliad.algebra._
import iliad.algebra.syntax.vector._

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
  def bind(m: BlendMode): DSL[Unit] = 
    BindBlendEquation(m).free
  def bind(f: BlendFunction): DSL[Unit] =
    BindBlendFunction(f).free
  def bindViewport(r: Rect[Int]): DSL[Unit] =
    BindViewport(r).free
  def bindClearColour(c: Vec4f): DSL[Unit] =
    BindClearColour(c).free
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
  def enable(as: Attribute.LoadedAttributes, stride: Int): DSL[Unit] =
    EnableAttributes(as, stride).free
  def apply(range: DataRange): DSL[Unit] = DrawTriangleModel(range).free
}

sealed trait Draw[A]

case class BindFramebuffer(f: Framebuffer.Loaded) extends Draw[Unit]
case class BindColorMask(c: ColorMask) extends Draw[Unit]
case class BindClearColour(c: Vec4f) extends Draw[Unit] 
case class Enable(c: Capability) extends Draw[Unit]
case class Disable(c: Capability) extends Draw[Unit]
case class BindBlendEquation(mode: BlendMode) extends Draw[Unit]
case class BindBlendFunction(f: BlendFunction) extends Draw[Unit]
case class BindViewport(rect: Rect[Int]) extends Draw[Unit]
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
case class EnableAttributes(as: Attribute.LoadedAttributes, stride: Int)
    extends Draw[Unit]

case class DrawTriangleModel(range: DataRange) extends Draw[Unit]

object DrawParser extends (Draw ~> OpenGL.DSL) {

  private def enableAttribute(stride: Int)(
      a: Attribute.Loaded): OpenGL.DSL[Unit] =
    a.offset.constructor.`type` match {
      case it: VertexAttribIType =>
        OpenGL.enableAttributeI(a.location,
                                a.offset.constructor.elementSize,
                                it,
                                stride,
                                a.offset.offset)
      case t: VertexAttribType =>
        OpenGL.enableAttribute(a.location,
                               a.offset.constructor.elementSize,
                               a.offset.constructor.`type`,
                               stride,
                               a.offset.offset)
    }

  def apply[A](draw: Draw[A]): OpenGL.DSL[A] = draw match {
    case BindFramebuffer(f) =>
      OpenGL.bindFramebuffer(f.frontId)
    case BindColorMask(m) =>
      OpenGL.colorMask(m.r, m.g, m.b, m.a)
    case BindClearColour(c) =>
      OpenGL.clearColor(c.x, c.y, c.z, c.w)
    case Enable(c) =>
      OpenGL.enable(c)
    case Disable(c) =>
      OpenGL.disable(c)
    case BindBlendEquation(mode) =>
      OpenGL.blendEquation(mode)
    case BindBlendFunction(f) =>
      OpenGL.blendFunc(f.src, f.dest)
    case BindViewport(rect) =>
      OpenGL.viewport(rect)
    case ClearFrame(bitMask) => OpenGL.clear(bitMask)
    case UseProgram(p) => OpenGL.useProgram(p.id)
    case BindTextureUniform(unit, location, t, s) =>
      OpenGL.bindTextureUniform(unit, location, t.frontId, s.id)
    case BindUniform(uniform, value) =>
      value.bind(uniform.location)
    case BindVertexBuffer(v) => OpenGL.bindVertexBuffer(v.id)
    case BindElementBuffer(e) => OpenGL.bindElementBuffer(e.id)
    case EnableAttributes(as, stride) =>
      as.attributes.traverse(enableAttribute(stride)).map(_ => ())
    case DrawTriangleModel(range: DataRange) =>
      OpenGL.drawTriangles(range.start, range.end)
  }
}
