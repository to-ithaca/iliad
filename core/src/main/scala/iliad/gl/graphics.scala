package iliad
package gl

import iliad.kernel.platform.GLES30Library

import cats._
import cats.data.{State => CatsState, StateT, Xor, XorT, ReaderT}
import cats.implicits._

import monocle._
import monocle.macros._

import shapeless._

import CatsExtra._
import MonocleExtra._

object Graphics {

  type Graphics = Animation :+: LoadCmd :+: GraphCmd :+: CNil

  case class State(animation: Animation.State,
    graph: GraphModel.Graph.Instance)

  type PRG = StateT[Xor[String, ?], State, XorT[CachedGL.DSL, String, Unit]]

  val _graph: monocle.Lens[State, GraphModel.Graph.Instance] = GenLens[State](_.graph)
  val _animation: monocle.Lens[State, Animation.State] = GenLens[State](_.animation)

  def empty(gc: GraphModel.Graph.Constructed): State = 
    State(Map.empty, gc.instance)

  private def liftG(fa: GraphCmd.Effect): PRG =
    fa.applyLens(_graph).map(_ => XorT.pure(()))

  private def liftL(fa: LoadCmd.Effect): PRG =
    StateT.pure(fa)

  private def liftA(fa: Animation.Effect): PRG = 
    fa.applyLens(_animation)
      .transformF(a => a.value.right[String])
      .map(_ => XorT.pure(()))

  def parse(pageSize: Int)(gs: List[Graphics]): PRG = gs.foldLeft(
    StateT.pure[Xor[String, ?], State, XorT[CachedGL.DSL, String, Unit]](XorT.pure(()))){ (b, fa) => 
    val next = fa match {
      case Inl(a) => liftA(Animation.parse(a))
      case Inr(Inl(l)) => liftL(LoadCmd.parse(pageSize)(l))
      case Inr(Inr(Inl(g))) => liftG(GraphCmd.parse(g))
      case Inr(Inr(Inr(_))) =>
        sys.error("Impossible case!")
    }

    for {
      x <- b
      xx <- next
    } yield x >> xx
  }

  private def lift(l: LoadCmd): Graphics = Coproduct[Graphics](l)
  private def lift(g: GraphCmd): Graphics = Coproduct[Graphics](g)

  def load(p: Program.Unlinked): Graphics = 
    lift(PutProgram(p))


  def load(r: VertexData.Ref, d: VertexData.Data): Graphics =
    lift(PutVertices(r, d))

  def load(r: ElementData.Ref, d: ElementData.Data): Graphics =
    lift(PutElements(r, d))

  def load(t: GraphModel.Texture.Instance, d: Option[Texture.Data]): Graphics =
    lift(PutTexture(t, d))

  def load(r: GraphModel.Renderbuffer.Instance): Graphics =
    lift(PutRenderbuffer(r))

  def load(f: GraphModel.Framebuffer.Instance): Graphics =
    lift(PutFramebuffer(f))

  def show(ns: List[GraphModel.Node.Instance]): Graphics =
    lift(Show(ns))

  abstract class DrawType(val primitive: PrimitiveType)
  object DrawType {
    case object Triangles extends DrawType(GL_TRIANGLES)
    case object Points extends DrawType(GL_POINTS)
  }

  abstract class Dimension(val capabilities: Set[Capability])
  object Dimension {
    case object D2 extends Dimension(Set.empty)
    case object D3 extends Dimension(Set(GL_DEPTH_TEST))
  }

  def model(name: String): GraphModel.Model.Constructor =
    GraphModel.Model.Constructor(name)

  def vsh(source: String,
    attributes: List[Attribute.Constructor],
    textures: List[(String, Sampler.Constructor)]
  ): VertexShader.Source
  = VertexShader.Source(source, attributes, textures)

  def fsh(source: String, textures: List[(String, Sampler.Constructor)]):
      FragmentShader.Source =
    FragmentShader.Source(source, textures)

  def program(v: VertexShader.Source, f: FragmentShader.Source): Program.Unlinked =
    Program.Unlinked(v, f)

  def onScreenDraw(name: String,
    program: Program.Unlinked,
    model: GraphModel.Model.Constructor,
    drawType: DrawType,
    dimension: Dimension
  ): GraphModel.Draw.Constructor =
    GraphModel.Draw.Constructor(
      name,
      program,
      drawType.primitive,
      dimension.capabilities,
      ColorMask.none,
      false,
      model,
      GraphModel.Framebuffer.OnScreen
    )

  def onScreenDraw(cons: GraphModel.Draw.Constructor,
    uniforms: Map[String, GraphModel.Texture.Uniform],
    model: GraphModel.Model.Instance): GraphModel.Draw.Instance =
    GraphModel.Draw.Instance(
      cons, uniforms, model,
      GraphModel.Framebuffer.OnScreen,
      1)


  def offScreenDraw(name: String,
    program: Program.Unlinked,
    model: GraphModel.Model.Constructor,
    drawType: DrawType,
    dimension: Dimension,
    outputs: List[(FramebufferAttachment, GraphModel.Output.Constructor)]
  ): GraphModel.Draw.Constructor =
    GraphModel.Draw.Constructor(
      name,
      program,
      drawType.primitive,
      dimension.capabilities,
      ColorMask.none,
      false,
      model,
      GraphModel.Framebuffer.OffScreenConstructor(outputs)
    )

  def onScreenClear(name: String): GraphModel.Clear.Constructor =
    GraphModel.Clear.Constructor(
      name,
      ChannelBitMask.BitMask(Set(GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT)),
      GraphModel.Framebuffer.OnScreen
    )

  def onScreenClear(cons: GraphModel.Clear.Constructor): GraphModel.Clear.Instance =
    GraphModel.Clear.Instance(
      cons,
      GraphModel.Framebuffer.OnScreen
    )

  def order(s: GraphModel.Node.Constructor, e: GraphModel.Node.Constructor): GraphModel.Link
  = GraphModel.Link.Order(s, e)
}

object GraphCmd {
  type Effect = StateT[Xor[String, ?], GraphModel.Graph.Instance, Unit]

  def parse(a: GraphCmd): Effect = a match {
    case Show(ns) => 
      GraphInstantiation.put(ns).transformF(_.leftMap(_.unwrap.mkString("\n")))
  }
}

sealed trait GraphCmd
case class Show(ns: List[GraphModel.Node.Instance]) extends GraphCmd

object LoadCmd {
  type Effect = XorT[CachedGL.DSL, String, Unit]

  private def lift[A](dsl: CachedGL.DSL[A]): Effect = 
    XorT.right(dsl.map(_ => ()))

  def parse(pageSize: Int)(a: LoadCmd): Effect = a match {
    case PutProgram(p) => lift(CachedGL.load(p))
    case PutVertices(r, d) => lift(CachedGL.load(r, d, pageSize))
    case PutElements(r, d) => lift(CachedGL.load(r, d, pageSize))
    case PutTexture(t, d) =>
      lift(CachedGL.load(GraphTransform.parse(GraphTransform.transform(t)), d))
    case PutRenderbuffer(r) =>
      lift(CachedGL.load(GraphTransform.parse(GraphTransform.transform(r))))
    case PutFramebuffer(f) =>
      XorT(CachedGL.load(GraphTransform.parse(GraphTransform.transform(f))))
  }
}

sealed trait LoadCmd

case class PutProgram(p: Program.Unlinked) extends LoadCmd
case class PutVertices(r: VertexData.Ref, d: VertexData.Data) extends LoadCmd
case class PutElements(r: ElementData.Ref, d: ElementData.Data) extends LoadCmd
case class PutTexture(t: GraphModel.Texture.Instance, d: Option[Texture.Data]) extends LoadCmd
case class PutRenderbuffer(r: GraphModel.Renderbuffer.Instance) extends LoadCmd
case class PutFramebuffer(f: GraphModel.Framebuffer.Instance) extends LoadCmd
