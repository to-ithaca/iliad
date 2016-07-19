package iliad
package gfx

import iliad.std.list._
import iliad.std.set._
import iliad.{gl => GL}

import cats._
import cats.data._
import cats.implicits._

import CatsExtra._

trait ConstructFunctions {

  def put(n: Node.Constructor): State[Graph.Constructor, Unit] =
    State.modify(_.addNode(n.lNode))
  def put(l: Link): State[Graph.Constructor, Unit] =
    State.modify(_.addEdge(l.lEdge))

  def model(name: String): Model.Constructor =
    Model.Constructor(name)

  def vsh(source: String,
          attributes: List[GL.Attribute.Constructor],
          textures: List[(String, GL.Sampler.Constructor)])
    : GL.VertexShader.Source =
    GL.VertexShader.Source(source, attributes, textures)

  def fsh(source: String, textures: List[(String, GL.Sampler.Constructor)])
    : GL.FragmentShader.Source =
    GL.FragmentShader.Source(source, textures)

  def program(v: GL.VertexShader.Source,
              f: GL.FragmentShader.Source): GL.Program.Unlinked =
    GL.Program.Unlinked(v, f)

  def onScreenDraw(name: String,
                   program: GL.Program.Unlinked,
                   model: Model.Constructor,
                   drawType: DrawType,
                   dimension: Dimension): Draw.Constructor =
    Draw.Constructor(
        name,
        program,
        drawType.primitive,
        dimension.capabilities,
        GL.ColorMask.none,
        false,
        model,
        Framebuffer.OnScreen
    )

  def draw(cons: Draw.Constructor,
           uniforms: Map[String, Texture.Uniform],
           model: Model.Instance): Draw.Instance =
    Draw.Instance(cons, uniforms, model, Framebuffer.OnScreen, 1)

  def offScreenDraw(
      name: String,
      program: GL.Program.Unlinked,
      model: Model.Constructor,
      drawType: DrawType,
      dimension: Dimension,
      outputs: List[(GL.FramebufferAttachment, Framebuffer.OutputConstructor)])
    : Draw.Constructor =
    Draw.Constructor(
        name,
        program,
        drawType.primitive,
        dimension.capabilities,
        GL.ColorMask.none,
        false,
        model,
        Framebuffer.OffScreenConstructor(outputs)
    )

  def clear(name: String): Clear.Constructor =
    Clear.Constructor(
        name,
        GL.ChannelBitMask.BitMask(
            Set(GL.GL_COLOR_BUFFER_BIT, GL.GL_DEPTH_BUFFER_BIT)),
        Framebuffer.OnScreen
    )

  def order(s: Node.Constructor, e: Node.Constructor): Link = Link.Order(s, e)
}

sealed trait ConstructError extends GraphicsError
case class DuplicateLinkError(duplicates: Set[Set[Link]])
    extends ConstructError
case class NonUniqueNodeError(ns: Set[Set[Node.Constructed]])
    extends ConstructError
case class OffScreenEndNodesError(ns: Set[Node.Constructed])
    extends ConstructError
case class PipeFromScreenError(p: Link.Pipe) extends ConstructError
case class PipeHasUnmatchedTexturesError(p: Link.Pipe,
                                         ts: Set[Texture.Constructor])
    extends ConstructError
case class PipeHasUnmatchedUniformsError(p: Link.Pipe, us: Set[String])
    extends ConstructError

private[iliad] object Construct {

  private def validate[E](f: => Boolean, err: E): ValidatedNel[E, Unit] =
    if (f) ().valid else err.invalidNel

  private val linksUnique: ReaderT[ValidatedNel[DuplicateLinkError, ?],
                                   Graph.Constructed,
                                   Unit] = ReaderT { g =>
    val dupes = g.links.duplicates(l => (l.start, l.end))
    validate(dupes.isEmpty, DuplicateLinkError(dupes))
  }

  private val nodesUnique: ReaderT[ValidatedNel[NonUniqueNodeError, ?],
                                   Graph.Constructed,
                                   Unit] = ReaderT { g =>
    val dupes = g.nodes.duplicates(_.constructor.name)
    validate(dupes.isEmpty, NonUniqueNodeError(dupes))
  }

  private val endNodesOnScreen: ReaderT[
      ValidatedNel[OffScreenEndNodesError, ?],
      Graph.Constructed,
      Unit] = ReaderT { g =>
    val offScreen = g.end.filter(_.constructor.framebuffer match {
      case Framebuffer.OnScreen => false
      case _ => true
    })
    validate(offScreen.isEmpty, OffScreenEndNodesError(offScreen))
  }

  private def pipeTextures: ReaderT[ValidatedNel[GraphicsError, ?],
                                    Graph.Constructed,
                                    Unit] =
    ReaderT(_.links.toList.filterClass[Link.Pipe].traverseUnit { p =>
      p.start.framebuffer match {
        case Framebuffer.OnScreen =>
          PipeFromScreenError(p).invalidNel.widen
        case f: Framebuffer.OffScreenConstructor =>
          val unmatched = p.textures.filterNot(f.textures.contains)
          validate(unmatched.isEmpty,
                   PipeHasUnmatchedTexturesError(p, unmatched)).widen
      }
    })

  private def pipeUniforms: ReaderT[
      ValidatedNel[PipeHasUnmatchedUniformsError, ?],
      Graph.Constructed,
      Unit] =
    ReaderT(_.links.toList.filterClass[Link.Pipe].traverseUnit { p =>
      val unmatched = p.uniformNames.filterNot(p.endTextureNames.contains)
      validate(unmatched.isEmpty, PipeHasUnmatchedUniformsError(p, unmatched))
    })

  private def widen[E <: GraphicsError](
      fa: ReaderT[ValidatedNel[E, ?], Graph.Constructed, Unit])
    : ReaderT[ValidatedNel[GraphicsError, ?], Graph.Constructed, Unit] =
    fa.mapF(_.widen)

  private def validate(
      g: Graph.Constructed): ValidatedNel[GraphicsError, Graph.Constructed] =
    (widen(nodesUnique) *>
          widen(linksUnique) *>
          widen(endNodesOnScreen) *>
          widen(pipeTextures) *>
          widen(pipeUniforms)).apply(g).map(_ => g)

  type DoubleTextures = Map[Texture.Constructor, Texture.Constructed]

  private def constructed(c: Framebuffer.Constructor)
    : Reader[DoubleTextures, Framebuffer.Constructed] = c match {
    case fb: Framebuffer.OffScreenConstructor =>
      Reader { ds =>
        val ts = fb.textures.map(t => ds.getOrElse(t, t.single))
        Framebuffer.OffScreenConstructed(fb, ts)
      }
    case Framebuffer.OnScreen => Kleisli.pure(Framebuffer.OnScreen)
  }

  private def constructed(
      n: Node.Constructor): Reader[DoubleTextures, Node.Constructed] =
    n match {
      case d: Draw.Constructor =>
        constructed(d.framebuffer).map(Draw.Constructed(d, _))
      case c: Clear.Constructor =>
        constructed(c.framebuffer).map(Clear.Constructed(c, _))
    }

  private def doubleTextures(
      g: Graph.Constructor): Map[Texture.Constructor, Texture.Constructed] =
    g.labEdges
      .map(_.label)
      .toSet
      .filterClass[Link.Pipe]
      .flatMap { l =>
        l.end.framebuffer match {
          case Framebuffer.OnScreen => Map.empty
          case c: Framebuffer.OffScreenConstructor =>
            c.textures.filter(l.textures.contains).map(t => t -> t.double)
        }
      }
      .toMap

  private def constructed(c: Graph.Constructor): Graph.Constructed = {
    val ds = doubleTextures(c)
    val g = c.vmap(n => constructed(n).run(ds))
    Graph.Constructed(
        g.nodes.toSet,
        g.labEdges.map(_.label).toSet,
        g.roots.toSet,
        g.leaves.toSet,
        ds
    )
  }

  def validate(s: State[Graph.Constructor, Unit])
    : Xor[NonEmptyList[GraphicsError], Graph.Constructed] =
    validate(constructed(s.runS(Graph.empty).value)).toXor
}
