package iliad
package gl

import iliad.kernel.platform.GLES30Library

import cats._
import cats.data.{State => CatsState, StateT, Xor, XorT, ReaderT}
import cats.free._
import cats.implicits._

import monocle._
import monocle.macros._

import freek._
import FreekExtra._
import CatsExtra._
import MonocleExtra._

object Graphics {

  type Graphics[A] = (GraphCmd :|: LoadCmd :|: Animation :|: FXNil)#Cop[A]
  type DSL[A] = Free[Graphics, A]

  case class State(animation: Animation.State,
    graph: GraphModel.Graph.Instance,
    gl: (Cached.State, Current.State))

  type PRG[F, A] = ReaderT[StateT[String Xor ?, State, ?], GLES30Library, A]

  val _graph: Lens[State, GraphModel.Graph.Instance] = GenLens[State](_.graph)
  val _gl: Lens[State, (Cached.State, Current.State)] = GenLens[State](_.gl)

  private def liftGraphCmd: GraphCmd.Effect ~> PRG =
    new (GraphCmd.Effect ~> PRG) {
      def apply[A](fa: GraphCmd.Effect[A]): PRG[A] =
        ReaderT.lift(fa.applyLens(_graph))
    }

  private def liftLoadCmd: LoadCmd.Effect ~> PRG =
    new (LoadCmd.Effect ~> PRG) {
      def apply[A](fa: LoadCmd.Effect[A]): PRG[A] =
        fa.map(_.applyLens(_gl))
    }

  //TODO
  private def liftAnimation: Animation.Effect ~> PRG = ???

  def runner(pageSize: Int): Interpreter[Graphics, PRG] =
    GraphParser.andThen(liftGraphCmd) :&:
      (new LoadCmdParser(pageSize)).andThen(liftLoadCmd) :&:
  AnimationParser.andThen(liftAnimation)


  def load(p: Program.Unlinked): DSL[Unit] =
    PutProgram(p).free.freekF[Graphics]

  def load(r: VertexData.Ref, d: VertexData.Data): DSL[Unit] =
    PutVertices(r, d).free.freekF[Graphics]

  def load(r: ElementData.Ref, d: ElementData.Data): DSL[Unit] =
    PutElements(r, d).free.freekF[Graphics]

  def load(t: GraphModel.Texture.Instance, d: Option[Texture.Data]): DSL[Unit] =
    PutTexture(t, d).free.freekF[Graphics]

  def load(r: GraphModel.Renderbuffer.Instance): DSL[Unit] =
    PutRenderbuffer(r).free.freekF[Graphics]

  def load(f: GraphModel.Framebuffer.Instance): DSL[Unit] =
    PutFramebuffer(f).free.freekF[Graphics]

  def show(ns: List[GraphModel.Node.Instance]): DSL[Unit] =
    Show(ns).free.freekF[Graphics]


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
  type Effect[A] = StateT[Xor[String, ?], GraphModel.Graph.Instance, A]
}

sealed trait GraphCmd[A]
case class Show(ns: List[GraphModel.Node.Instance]) extends GraphCmd[Unit]

object GraphParser extends (GraphCmd ~> GraphCmd.Effect) {
  def apply[A](fa: GraphCmd[A]): GraphCmd.Effect[A] = fa match {
    case Show(ns) => GraphInstantiation.put(ns).transformF(_.leftMap(_.unwrap.mkString("\n")))
  }
}


object LoadCmd {
  type Effect[F[_], A] = ReaderT[StateT[F, (Cached.State, Current.State), ?], GLES30Library, A]
}

sealed trait LoadCmd[A]

case class PutProgram(p: Program.Unlinked) extends LoadCmd[Unit]
case class PutVertices(r: VertexData.Ref, d: VertexData.Data) extends LoadCmd[Unit]
case class PutElements(r: ElementData.Ref, d: ElementData.Data) extends LoadCmd[Unit]
case class PutTexture(t: GraphModel.Texture.Instance, d: Option[Texture.Data]) extends LoadCmd[Unit]
case class PutRenderbuffer(r: GraphModel.Renderbuffer.Instance) extends LoadCmd[Unit]
case class PutFramebuffer(f: GraphModel.Framebuffer.Instance) extends LoadCmd[Unit]

//TODO: perhaps this should be a reader of pageSize?
private final class LoadCmdParser(pageSize: Int) extends (LoadCmd ~> XorT[CachedGL.DSL, String, ?]) {

  private def parse[A](fa: GraphTransform.DSL[A]): A = GraphTransform.parse(fa)

  private def lift[A](fa: CachedGL.DSL[A]): XorT[CachedGL.DSL, String, Unit] =
    XorT.right(fa.map(_ => ()))

  def apply[A](fa: LoadCmd[A]): XorT[CachedGL.DSL, String, A] = fa match {
    case PutProgram(p) => lift(CachedGL.load(p))
    case PutVertices(r, d) => lift(CachedGL.load(r, d, pageSize))
    case PutElements(r, d) => lift(CachedGL.load(r, d, pageSize))
    case PutTexture(t, d) =>
      lift(CachedGL.load(parse(GraphTransform.transform(t)), d))
    case PutRenderbuffer(r) =>
      lift(CachedGL.load(parse(GraphTransform.transform(r))))
    case PutFramebuffer(f) =>
      XorT(CachedGL.load(parse(GraphTransform.transform(f))))
  }
}
