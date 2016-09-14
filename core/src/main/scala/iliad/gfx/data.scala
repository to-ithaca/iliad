package iliad
package gfx

import iliad.{gl => GL}
import iliad.algebra._
import iliad.algebra.syntax.vector._
import iliad.std.list._
import iliad.std.set._

import cats._
import cats.data._
import cats.implicits._

import quiver.{LNode, LEdge, Decomp}

import monocle._
import monocle.syntax.all._
import monocle.macros._

import com.typesafe.scalalogging._

import scala.{Vector => SVector}

object Graph extends LazyLogging {
  type Constructor = quiver.Graph[Node.Constructor, String, Link]
  type QInstance = quiver.Graph[Node.Instance, String, Unit]

  private[gfx] val empty: Constructor =
    quiver.empty[Node.Constructor, String, Link]

  case class Constructed(
      nodes: SVector[Node.Constructed],
      links: Set[Link],
      start: Set[Node.Constructed],
      end: Set[Node.Constructed],
      doubleTextures: Map[Texture.Constructor, Texture.Constructed]) {
    private[gfx] def instance: Instance =
      Instance(this, quiver.empty[Node.Instance, String, Unit])
    private[gfx] val orderedLinks: Set[Link.Order] = links.filterClass[Link.Order]
    private[gfx] val pipes: Set[Link.Pipe] = links.filterClass[Link.Pipe]
  }

  object Instance {
    private val _graph: Lens[Instance, QInstance] = GenLens[Instance](_.graph)

    private[gfx] def addNode[F[_]: Applicative](n: Node.Instance): StateT[F, Instance, Unit] =
      StateT.modify[F, Instance](_graph modify(_.addNode(n.lNode)))

    private[gfx] def addEdges[F[_]: Applicative](ls: List[Link.Instance]): StateT[F, Instance, Unit] =
      StateT.modify[F, Instance](_graph modify(qg => ls.foldLeft(qg)((next, l) => next.addEdge(l.lEdge))))
  }

  case class Instance(constructed: Constructed, graph: QInstance) {

    private[gfx] def remove(ns: List[Node.Instance]): Instance = {
      val next = graph.nfilter(n => !ns.exists(_.name == n.name))
      copy(graph = next)
    }

    private[gfx] def nodes(scopes: UniformCache.Values)
      : Reader[GraphTraversal, GraphicsError Xor SVector[Node.Drawable]] =
      Reader[GraphTraversal, GraphicsError Xor SVector[Node.Drawable]] { f =>
        val ops = f(this)
        ops.traverse {
          case c: Clear.Instance => c.right
          case d: Draw.Instance =>
            d.scopes.traverse { p =>
                for {
                  us <- scopes
                         .get(p.scope)
                         .toRightXor(UnsetScopeError(p.scope, scopes.keySet))
                  v <- us.get(p.name).toRightXor(UnsetUniformError(p.name, p.scope))
                } yield v
            }.map(Draw.Drawable(d, _))
        }
      }
  }
}

sealed trait Node
object Node {
  sealed trait Constructor {
    def name: String
    def framebuffer: Framebuffer.Constructor
    private[gfx] def lNode: LNode[Constructor, String] = LNode(this, name)
  }

  sealed trait Constructed {
    def constructor: Constructor
  }

  sealed trait Instance {
    def name: String
    def constructor: Constructor
    private[gfx] def lNode: LNode[Instance, String] = LNode(this, name)
  }
  sealed trait Drawable
}

object Draw {
  case class Constructor(
      name: String,
      program: GL.Program.Unlinked,
      primitive: GL.PrimitiveType,
      capabilities: Map[GL.Capability, Boolean],
      colorMask: GL.ColorMask,
      blend: Option[GL.Blend],
      isInstanced: Boolean,
      framebuffer: Framebuffer.Constructor
  ) extends Node.Constructor

  case class Constructed(
      constructor: Constructor,
      framebuffer: Framebuffer.Constructed
  ) extends Node.Constructed

  case class Instance(
      constructor: Constructor,
      textureUniforms: Map[String, Texture.Uniform],
      scopes: List[ScopeProperty],
      model: Model,
      framebuffer: Framebuffer.Instance,
      numInstances: Int,
      viewport: Viewport
  ) extends Node.Instance {
    def name: String = s"${constructor.name}-${scopes}-instance"
    private[gfx] def vertexAttribs: List[GL.Attribute.Constructor] =
      constructor.program.vertex.attributes
    private[gfx] def modelAttribs: List[GL.Attribute.Constructor] =
      model.vertex.ref.buffer.attributes
  }

  case class Drawable(
      instance: Instance,
      uniforms: List[GL.Uniform.Value]
  ) extends Node.Drawable
}

object Clear {
  case class Constructor(
      name: String,
      mask: GL.ChannelBitMask,
      colour: Vec4f,
      framebuffer: Framebuffer.Constructor
  ) extends Node.Constructor

  case class Constructed(constructor: Constructor,
                         framebuffer: Framebuffer.Constructed)
      extends Node.Constructed

  case class Instance(
      constructor: Constructor,
      framebuffer: Framebuffer.Instance,
      viewport: Viewport
  ) extends Node.Instance
      with Node.Drawable {
    def name: String = toString
  }
}

sealed trait Link {
  def start: Node.Constructor
  def end: Node.Constructor
  private[gfx] def lEdge: LEdge[Node.Constructor, Link] =
    LEdge(start, end, this)
}

object Link {

  case class Pipe(start: Draw.Constructor,
                  end: Draw.Constructor,
                  uniforms: Map[String, Texture.Constructor])
      extends Link {
    private[gfx] def textures: Set[Texture.Constructor] = uniforms.values.toSet
    private[gfx] def uniformNames: Set[String] = uniforms.keySet
    private[gfx] def endTextureNames: List[String] = end.program.textureNames
  }

  case class Order(start: Node.Constructor, end: Node.Constructor) extends Link

  case class Instance(start: Node.Instance, end: Node.Instance) {
    private[gfx] def lEdge: LEdge[Node.Instance, Unit] = LEdge(start, end, ())
  }
}

object Texture {
  sealed trait Uniform
  case class Constructor(name: String,
                         format: GL.Texture.Format)
      extends Framebuffer.OutputConstructor {
    private[gfx] def single: Constructed = Constructed(this, false)
    private[gfx] def double: Constructed = Constructed(this, true)
  }

  case class Constructed(constructor: Constructor, isDouble: Boolean)

  case class Instance(name: String, constructor: Constructor)
      extends Uniform
      with Framebuffer.OutputInstance

  case class Image(name: String, format: GL.Texture.Format)
      extends Uniform
}

object Renderbuffer {
  case class Constructor(name: String,
                         format: GL.RenderbufferInternalFormat,
                         viewport: Vec2i)
      extends Framebuffer.OutputConstructor
  case class Instance(name: String, constructor: Constructor)
      extends Framebuffer.OutputInstance
}

object Framebuffer {
  sealed trait Constructor
  sealed trait Constructed
  sealed trait Instance

  sealed trait OutputConstructor
  sealed trait OutputInstance

  case object OnScreen extends Constructor with Constructed with Instance

  case class OffScreenConstructor(
      buffers: List[(GL.FramebufferAttachment, Framebuffer.OutputConstructor)])
      extends Constructor {
    private[gfx] def textures: List[Texture.Constructor] =
      buffers.map(_._2).filterClass[Texture.Constructor]
  }

  case class OffScreenConstructed(
      constructor: OffScreenConstructor,
      textures: List[Texture.Constructed]
  ) extends Constructed

  case class OffScreenInstance(
      instances: List[(GL.FramebufferAttachment, Framebuffer.OutputInstance)])
      extends Instance
}

case class ScopeProperty(name: String, scope: String) 

sealed trait Viewport

object Viewport {
  case class Fraction(rect: Rect[Double]) extends Viewport
  case class Exact(rect: Rect[Int]) extends Viewport
  val full: Viewport = Fraction(Rect(v"0.0 0.0", v"1.0 1.0"))
}

//TODO: find out what to do with this
//case class Valve(start: Node.Draw, links: List[Link.Pipe])
