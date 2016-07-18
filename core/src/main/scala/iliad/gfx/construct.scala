package iliad
package gfx

import iliad.std.list._
import iliad.std.set._
import iliad.{gl => GL}

import cats._
import cats.data._
import cats.implicits._

import CatsExtra._

trait GraphFunctions {

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

  def onScreenDraw(cons: Draw.Constructor,
                   uniforms: Map[String, Texture.Uniform],
                   model: Model.Instance): Draw.Instance =
    Draw.Instance(cons, uniforms, model, Framebuffer.OnScreen, 1)

  def offScreenDraw(
      name: String,
      program: GL.Program.Unlinked,
      model: Model.Constructor,
      drawType: DrawType,
      dimension: Dimension,
      outputs: List[(GL.FramebufferAttachment, Output.Constructor)])
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

  def onScreenClear(name: String): Clear.Constructor =
    Clear.Constructor(
        name,
        GL.ChannelBitMask.BitMask(
            Set(GL.GL_COLOR_BUFFER_BIT, GL.GL_DEPTH_BUFFER_BIT)),
        Framebuffer.OnScreen
    )

  def order(s: Node.Constructor, e: Node.Constructor): Link = Link.Order(s, e)
}

private[iliad] object Construct {

  type GValidated = ReaderT[ValidatedNel[String, ?], Graph.Constructed, Unit]

  private def validate(f: => Boolean,
                       err: String): ValidatedNel[String, Unit] =
    if (f) ().valid else err.invalidNel

  private val linksUnique: GValidated = ReaderT { g =>
    val dupes = g.links.duplicates(l => (l.start, l.end))
    validate(dupes.isEmpty,
             s"The following links are duplicates ${dupes.mkString("\n")}")
  }

  private val nodesUnique: GValidated = ReaderT { g =>
    val dupes = g.nodes.duplicates(_.constructor.name)
    validate(dupes.isEmpty,
             s"The following nodes are non-unique ${dupes.mkString("\n")}")
  }

  private val endNodesOnScreen: GValidated = ReaderT { g =>
    val offScreen = g.end.filter(_.constructor.framebuffer match {
      case Framebuffer.OnScreen => false
      case _ => true
    })
    validate(offScreen.isEmpty,
             s"The following end nodes are off screen $offScreen")
  }

  private def pipeTextures: GValidated =
    ReaderT(_.links.toList.filterClass[Link.Pipe].traverseUnit { p =>
      p.start.framebuffer match {
        case Framebuffer.OnScreen =>
          s"Pipe $p starts with on screen framebuffer".invalidNel
        case f: Framebuffer.OffScreenConstructor =>
          val unmatched = p.textures.filterNot(f.textures.contains)
          validate(
              unmatched.isEmpty,
              s"Pipe $p references textures $unmatched which are not in start node")
      }
    })

  private def pipeUniforms: GValidated =
    ReaderT(_.links.toList.filterClass[Link.Pipe].traverseUnit { p =>
      val unmatched = p.uniformNames.filterNot(p.endTextureNames.contains)
      validate(
          unmatched.isEmpty,
          s"Pipe $p references uniforms $unmatched which are not in end node")
    })

  private def validate(
      g: Graph.Constructed): ValidatedNel[String, Graph.Constructed] =
    (nodesUnique *> linksUnique *>
          endNodesOnScreen *> pipeTextures *> pipeUniforms)
      .apply(g)
      .map(_ => g)

  private def constructed(
      c: Framebuffer.Constructor,
      doubles: Map[Texture.Constructor, Texture.Constructed])
    : Framebuffer.Constructed = c match {
    case fb: Framebuffer.OffScreenConstructor =>
      Framebuffer.OffScreenConstructed(fb, fb.textures.map(t =>
                doubles.getOrElse(t, t.single)))
    case Framebuffer.OnScreen => Framebuffer.OnScreen
  }

  private def constructed(
      n: Node.Constructor,
      doubles: Map[Texture.Constructor, Texture.Constructed])
    : Node.Constructed = {
    n match {
      case d: Draw.Constructor =>
        Draw.Constructed(d, constructed(d.framebuffer, doubles))
      case c: Clear.Constructor =>
        Clear.Constructed(c, constructed(c.framebuffer, doubles))
    }
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
    val g = c.vmap(n => constructed(n, ds))
    Graph.Constructed(
        g.nodes.toSet,
        g.labEdges.map(_.label).toSet,
        g.roots.toSet,
        g.leaves.toSet,
        ds
    )
  }

  def apply(s: State[Graph.Constructor, Unit])
    : ValidatedNel[String, Graph.Constructed] =
    validate(constructed(s.runS(Graph.empty).value))
}
