package iliad
package gl

import cats._
import cats.free._
import cats.implicits._

import CatsExtra._

object GraphTransform {
  import iliad.gl.{GraphModel => GM}
  import iliad.{gl => GL}

  sealed trait To[A]
  type DSL[A] = Free[To, A]

  private def transform(t: GM.Texture.Instance): DSL[GL.Texture.Constructor] =
    if (t.constructor.isDouble) DoubleTexture(t).free
    else SingleTexture(t).free

  private def transform(
      i: GM.Output.Instance): DSL[GL.Framebuffer.AttachmentConstructor] =
    i match {
      case t: GM.Texture.Instance => transform(t)
      case r: GM.Renderbuffer.Instance => Renderbuffer(r).free.widen
    }

  private def transform(
      f: GM.Framebuffer.Instance): DSL[GL.Framebuffer.Constructor] =
    f match {
      case GM.Framebuffer.OnScreen => OnScreenFramebuffer.free
      case ff @ GM.Framebuffer.OffScreenInstance(is) =>
        is.traverse { case (c, a) => transform(a).map(c -> _) }.map { as =>
          if (ff.hasDoubleTexture) Framebuffer.DoubleConstructor(as)
          else Framebuffer.SingleConstructor(as)
        }
    }

  private def transform(ts: Map[String, GM.Texture.Uniform])
    : DSL[Map[String, GL.Texture.Constructor]] =
    ts.mapValues {
      case t: GM.Texture.Instance => transform(t)
      case i: GM.Texture.Image => Image(i).free
    }.sequence

  def apply(n: GM.Draw.Instance): DSL[GL.DrawOp] =
    for {
      f <- transform(n.framebuffer)
      us <- transform(n.uniforms)
    } yield
      GL.DrawOp(n.model.model,
                n.constructor.program,
                us,
                f,
                n.constructor.colorMask,
                n.constructor.primitive,
                n.constructor.capabilities,
                n.numInstances)

  def apply(c: GM.Clear.Instance): DSL[GL.ClearOp] =
    for {
      f <- transform(c.framebuffer)
    } yield GL.ClearOp(c.constructor.mask, f)

  def parse[A](dsl: DSL[A]): A = dsl.foldMap(Interpreter)

  case class DoubleTexture(t: GM.Texture.Instance)
      extends To[GL.Texture.Constructor]
  case class SingleTexture(t: GM.Texture.Instance)
      extends To[GL.Texture.Constructor]
  case class Image(i: GM.Texture.Image) extends To[GL.Texture.Constructor]
  case class Renderbuffer(r: GM.Renderbuffer.Instance)
      extends To[GL.Renderbuffer.Constructor]
  case object OnScreenFramebuffer extends To[GL.Framebuffer.Constructor]

  object Interpreter extends (To ~> Id) {
    def apply[A](t: To[A]): Id[A] = t match {
      case DoubleTexture(t) =>
        GL.Texture.DoubleConstructor(s"${t.name}-${t.constructor.name}",
                                     t.constructor.format,
                                     t.constructor.viewport)
      case SingleTexture(t) =>
        GL.Texture.SingleConstructor(s"${t.name}-${t.constructor.name}",
                                     t.constructor.format,
                                     t.constructor.viewport)
      case Image(i) =>
        GL.Texture.SingleConstructor(i.name, i.format, i.viewport)
      case Renderbuffer(r) =>
        GL.Renderbuffer.Constructor(s"${r.name}-${r.constructor.name}",
                                    r.constructor.format,
                                    r.constructor.viewport)
      case OnScreenFramebuffer => GL.Framebuffer.default
    }
  }
}
