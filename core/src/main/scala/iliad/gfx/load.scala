package iliad
package gfx

import iliad.gl._

import cats._
import cats.data._
import cats.implicits._

object Load {
  type Effect = XorT[GL.DSL, String, Unit]

  private def lift[A](dsl: GL.DSL[A]): Effect =
    XorT.right(dsl.map(_ => ()))

  def parse(pageSize: Int)(a: Load): Effect = a match {
    case PutProgram(p) => lift(GL.load(p))
    case PutVertices(r, d) => lift(GL.load(r, d, pageSize))
    case PutElements(r, d) => lift(GL.load(r, d, pageSize))
    case PutTexture(t, d) =>
      lift(GL.load(GraphTransform.parse(GraphTransform.transform(t)), d))
    case PutRenderbuffer(r) =>
      lift(GL.load(GraphTransform.parse(GraphTransform.transform(r))))
    case PutFramebuffer(f) =>
      XorT(GL.load(GraphTransform.parse(GraphTransform.transform(f))))
  }
}
 
sealed trait Load

case class PutProgram(p: Program.Unlinked) extends Load
case class PutVertices(r: VertexData.Ref, d: VertexData.Data) extends Load
case class PutElements(r: ElementData.Ref, d: ElementData.Data) extends Load
case class PutTexture(t: Texture.Instance, d: Option[gl.Texture.Data])
    extends Load
case class PutRenderbuffer(r: Renderbuffer.Instance) extends Load
case class PutFramebuffer(f: Framebuffer.Instance) extends Load

trait LoadFunctions {

  private def lift(l: Load): Graphics = shapeless.Coproduct[Graphics](l)

  def load(p: Program.Unlinked): Graphics =
    lift(PutProgram(p))

  def load(r: VertexData.Ref, d: VertexData.Data): Graphics =
    lift(PutVertices(r, d))

  def load(r: ElementData.Ref, d: ElementData.Data): Graphics =
    lift(PutElements(r, d))

  def load(t: Texture.Instance, d: Option[gl.Texture.Data]): Graphics =
    lift(PutTexture(t, d))

  def load(r: Renderbuffer.Instance): Graphics =
    lift(PutRenderbuffer(r))

  def load(f: Framebuffer.Instance): Graphics =
    lift(PutFramebuffer(f))
}
