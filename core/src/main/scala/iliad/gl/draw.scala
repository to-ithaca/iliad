package iliad
package gl

import iliad.CatsExtra._

import cats._
import cats.free._
import cats.implicits._

object Draw {
  type DSL[A] = Free[Draw, A]

  def parse[F[_]: Monad](i: GL.Interpreter[F]): Draw ~> F =
    DrawParser.andThen(GL.interpret(i))

  def bindFramebuffer(framebuffer: Int): DSL[Unit] =
    BindFramebuffer(framebuffer).free
  def clear(m: ChannelBitMask): DSL[Unit] = ClearFrame(m).free
  def use(p: Program.Linked): DSL[Unit] = UseProgram(p).free
  def bind(v: VertexBuffer.Loaded): DSL[Unit] = BindVertexBuffer(v).free
  def bind(e: ElementBuffer.Loaded): DSL[Unit] = BindElementBuffer(e).free
  def enable(as: Attribute.LoadedAttributes, baseOffset: Int): DSL[Unit] =
    EnableAttributes(as, baseOffset).free
  def apply(range: DataRange): DSL[Unit] = DrawTriangleModel(range).free
}

sealed trait Draw[A]

case class BindFramebuffer(id: Int) extends Draw[Unit]
case class ClearFrame(bitMask: ChannelBitMask) extends Draw[Unit]
case class UseProgram(p: Program.Linked) extends Draw[Unit]
case class BindVertexBuffer(v: VertexBuffer.Loaded) extends Draw[Unit]
case class BindElementBuffer(e: ElementBuffer.Loaded) extends Draw[Unit]
case class EnableAttributes(as: Attribute.LoadedAttributes, baseOffset: Int)
    extends Draw[Unit]

case class DrawTriangleModel(range: DataRange) extends Draw[Unit]

private object DrawParser extends (Draw ~> GL.DSL) {

  private def enableAttribute(stride: Int)(a: Attribute.Offset): GL.DSL[Unit] =
    GL.enableAttribute(
        a.l.location, a.l.c.elementSize, a.l.c.`type`, stride, a.offset)

  def apply[A](draw: Draw[A]): GL.DSL[A] = draw match {
    case BindFramebuffer(f) => GL.bindFramebuffer(f)
    case ClearFrame(bitMask) => GL.clear(bitMask)
    case UseProgram(p) => GL.useProgram(p.id)
    case BindVertexBuffer(v) => GL.bindVertexBuffer(v.id)
    case BindElementBuffer(e) => GL.bindElementBuffer(e.id)
    case EnableAttributes(as, baseOffset) =>
      as.offsets(baseOffset).traverse(enableAttribute(as.stride)).map(_ => ())
    case DrawTriangleModel(range: DataRange) =>
      GL.drawTriangles(range.start, range.end)
  }
}
