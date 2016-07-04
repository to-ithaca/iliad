package iliad
package gl

import cats._
import cats.data._
import cats.free._
import cats.implicits._

import freek._
import FreekExtra._
import CatsExtra._

object Graphics {

  type Graphics[A] = (GraphCmd :|: LoadCmd :|: FXNil)#Cop[A]
  type DSL[A] = Free[Graphics, A]

  type PRG[A] = StateT[XorT[CachedGL.DSL, String, ?], GraphModel.Graph.Instance, A]

  private def liftGraphCmd: GraphCmd.Effect ~> PRG =
    new (GraphCmd.Effect ~> PRG) {
      def apply[A](fa: GraphCmd.Effect[A]): PRG[A] =
        fa.transformF(x => XorT.fromXor(x))
    }

  private def liftLoadCmd: LoadCmd.Effect ~> PRG =
    new (LoadCmd.Effect ~> PRG) {
      def apply[A](fa: LoadCmd.Effect[A]): PRG[A] =
        StateTExtra.lift(fa)
    }

  def runner(pageSize: Int): Interpreter[Graphics, PRG] =
    GraphParser.andThen(liftGraphCmd) :&:
      (new LoadCmdParser(pageSize)).andThen(liftLoadCmd)

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
  type Effect[A] = XorT[CachedGL.DSL, String, A]
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
