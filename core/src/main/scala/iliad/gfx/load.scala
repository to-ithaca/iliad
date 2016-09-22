package iliad
package gfx

import iliad.{gl => GL}

import cats._
import cats.data._
import cats.implicits._

import scodec.bits._

import java.io.{FileInputStream, InputStream}

object Load {
  type Effect = Reader[Graphics.Config, XorT[GL.GL.DSL, GL.GLError, Unit]]

  private def lift[A](dsl: GL.GL.DSL[A]): Effect =
    Kleisli.pure(XorT.right[GL.GL.DSL, GL.GLError, Unit](dsl.map(_ => ())))

  private def lift[A](f: Graphics.Config => GL.GL.DSL[A]): Effect =
    Reader(cfg => XorT.right[GL.GL.DSL, GL.GLError, Unit](f(cfg).map(_ => ())))

  private def liftXor[E <: GL.GLError, A](
      f: Graphics.Config => GL.GL.DSL[E Xor A]): Effect =
    Reader(cfg => XorT(f(cfg)).map(_ => ()).leftWiden[GL.GLError])

  private[gfx] def apply(l: Load): Effect = l match {
    case PutProgram(p) => lift(GL.GL.load(p))
    case PutVertices(r, d) =>
      liftXor(cfg => GL.GL.load(r, d, cfg.pageSize))
    case PutElements(r, d) =>
      liftXor(cfg => GL.GL.load(r, d, cfg.pageSize))
    case PutTexture(t, d) =>
      lift(cfg => GL.GL.load(ToGL.run(ToGL(t)).run(cfg), d))
    case PutImage(i, d) =>
      lift(cfg => GL.GL.load(ToGL.run(ToGL(i)).run(cfg), d))
    case PutRenderbuffer(r) =>
      lift(cfg => GL.GL.load(ToGL.run(ToGL(r)).run(cfg)))
    case PutFramebuffer(f) =>
      liftXor(cfg => GL.GL.load(ToGL.run(ToGL(f)).run(cfg)))
  }
}

sealed trait Load

private case class PutProgram(p: GL.Program.Unlinked) extends Load
private case class PutVertices(r: GL.VertexData.Ref, d: ByteVector)
    extends Load
private case class PutElements(r: GL.ElementData.Ref, d: ByteVector)
    extends Load
private case class PutTexture(t: Texture.Instance, d: GL.Texture.Data)
    extends Load
private case class PutImage(i: Texture.Image, d: GL.Texture.Data) extends Load
private case class PutRenderbuffer(r: Renderbuffer.Instance) extends Load
private case class PutFramebuffer(f: Framebuffer.Instance) extends Load

trait LoadFunctions {

  private def lift(l: Load): GFX =
    shapeless.Coproduct[GFX](l)

  def load(vf: (GL.VertexShader.Source, GL.FragmentShader.Source)): GFX = {
    val (v, f) = vf
    lift(PutProgram(GL.Program.Unlinked(v, f)))
  }

  def load(r: VertexRef, d: ByteVector): GFX =
    lift(PutVertices(r.ref, d))

  def load(r: ElementRef, d: ByteVector): GFX =
    lift(PutElements(r.ref, d))

  def load(t: Texture.Instance, d: GL.Texture.Data): GFX =
    lift(PutTexture(t, d))

  def load[A](name: String, b: Bitmap[A])(implicit format: GL.GLTextureFormat[A]): GFX =
    lift(PutImage(Texture.Image(name, format.format), GL.Texture.SingleData(b.dimensions, b.pixels)))

  def load(r: Renderbuffer.Instance): GFX =
    lift(PutRenderbuffer(r))

  def load(f: Framebuffer.Instance): GFX =
    lift(PutFramebuffer(f))

  def loadFile(name: String): Xor[GraphicsError, BitVector] = 
    ResourceLoader.loadFile(name).leftMap(_ => FileNotFoundError(name))

  def decodePNG[A](bitVector: BitVector)(implicit decoder: PNGDecoder[A]): Xor[GraphicsError, Bitmap[A]] =
    decoder.decode(bitVector).toXor.map(_.value)
      .leftMap(PNGDecodeError).leftWiden[GraphicsError]
}
