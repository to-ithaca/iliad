package iliad
package gfx

import iliad.kernel.platform.GLES30Library
import iliad.gl._

import cats._
import cats.data.{State => CatsState, StateT, Xor, XorT, ReaderT}
import cats.implicits._

import monocle._
import monocle.macros._

import shapeless._

import CatsExtra._
import MonocleExtra._

object Graphics {

  case class State(animation: Animation.State, graph: Graph.Instance)

  type PRG = StateT[Xor[String, ?], State, XorT[GL.DSL, String, Unit]]

  val _graph: monocle.Lens[State, Graph.Instance] = GenLens[State](_.graph)
  val _animation: monocle.Lens[State, Animation.State] =
    GenLens[State](_.animation)

  def empty(gc: Graph.Constructed): State =
    State(Map.empty, gc.instance)

  private def liftG(fa: Action.Effect): PRG =
    fa.applyLens(_graph).map(_ => XorT.pure(()))

  private def liftL(fa: Load.Effect): PRG =
    StateT.pure(fa)
  private def liftA(fa: Animation.Effect): PRG =
    fa.applyLens(_animation)
      .transformF(a => a.value.right[String])
      .map(_ => XorT.pure(()))

  private[iliad] def apply(pageSize: Int, gs: List[Graphics]): PRG =
    gs.foldLeft(
        StateT.pure[Xor[String, ?], State, XorT[GL.DSL, String, Unit]](
            XorT.pure(()))) { (b, fa) =>
      val next = fa match {
        case Inl(a) => liftA(Animation.parse(a))
        case Inr(Inl(l)) => liftL(Load.parse(pageSize)(l))
        case Inr(Inr(Inl(g))) => liftG(Action.parse(g))
        case Inr(Inr(Inr(_))) =>
          sys.error("Impossible case!")
      }

      for {
        x <- b
        xx <- next
      } yield x >> xx
    }
}

trait GraphFunctions {

  def model(name: String): Model.Constructor =
    Model.Constructor(name)

  def vsh(source: String,
          attributes: List[Attribute.Constructor],
          textures: List[(String, Sampler.Constructor)]): VertexShader.Source =
    VertexShader.Source(source, attributes, textures)

  def fsh(
      source: String,
      textures: List[(String, Sampler.Constructor)]): FragmentShader.Source =
    FragmentShader.Source(source, textures)

  def program(v: VertexShader.Source,
              f: FragmentShader.Source): Program.Unlinked =
    Program.Unlinked(v, f)

  def onScreenDraw(name: String,
                   program: Program.Unlinked,
                   model: Model.Constructor,
                   drawType: DrawType,
                   dimension: Dimension): Draw.Constructor =
    Draw.Constructor(
        name,
        program,
        drawType.primitive,
        dimension.capabilities,
        ColorMask.none,
        false,
        model,
        Framebuffer.OnScreen
    )

  def onScreenDraw(cons: Draw.Constructor,
                   uniforms: Map[String, Texture.Uniform],
                   model: Model.Instance): Draw.Instance =
    Draw.Instance(cons, uniforms, model, Framebuffer.OnScreen, 1)

  def offScreenDraw(name: String,
                    program: Program.Unlinked,
                    model: Model.Constructor,
                    drawType: DrawType,
                    dimension: Dimension,
                    outputs: List[(FramebufferAttachment, Output.Constructor)])
    : Draw.Constructor =
    Draw.Constructor(
        name,
        program,
        drawType.primitive,
        dimension.capabilities,
        ColorMask.none,
        false,
        model,
        Framebuffer.OffScreenConstructor(outputs)
    )

  def onScreenClear(name: String): Clear.Constructor =
    Clear.Constructor(
        name,
        ChannelBitMask.BitMask(Set(GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT)),
        Framebuffer.OnScreen
    )

  def onScreenClear(cons: Clear.Constructor): Clear.Instance =
    Clear.Instance(
        cons,
        Framebuffer.OnScreen
    )

  def order(s: Node.Constructor, e: Node.Constructor): Link = Link.Order(s, e)
}
