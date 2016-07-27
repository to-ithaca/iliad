package iliad
package gfx

import iliad.{gl => GL}
import iliad.syntax.all._
import iliad.std.list._
import iliad.std.set._

import cats._
import cats.data._
import cats.implicits._

import quiver.{LNode, LEdge, Decomp}
import QuiverExtra._

object Graph {
  type Constructor = quiver.Graph[Node.Constructor, String, Link]
  type QInstance = quiver.Graph[Node.Instance, String, Unit]

  private[gfx] val empty: Constructor =
    quiver.empty[Node.Constructor, String, Link]

  case class Constructed(
      nodes: Set[Node.Constructed],
      links: Set[Link],
      start: Set[Node.Constructed],
      end: Set[Node.Constructed],
      doubleTextures: Map[Texture.Constructor, Texture.Constructed]) {
    private[gfx] def instance: Instance =
      Instance(this, quiver.empty[Node.Instance, String, Unit])
  }

  case class Instance(constructed: Constructed, graph: QInstance) {

    private def addNodes(ns: List[Node.Instance]): State[QInstance, Unit] =
      State.modify(qg => ns.foldLeft(qg)((next, n) => next.addNode(n.lNode)))

    private def addEdges(ls: List[Link.Instance]): State[QInstance, Unit] =
      State.modify(qg => ls.foldLeft(qg)((next, l) => next.addEdge(l.lEdge)))

    private[gfx] def put(ns: List[Node.Instance],
                         ls: List[Link.Instance]): Instance = {
      val next = (addNodes(ns) >> addEdges(ls)).run(graph).value._1
      copy(graph = next)
    }

    private[gfx] def nodes(us: Map[Draw.Instance, List[GL.Uniform]])
      : Reader[GraphTraversal, UnsetUniformError Xor Vector[Node.Drawable]] =
      Reader[GraphTraversal, UnsetUniformError Xor Vector[Node.Drawable]](
          _.apply(graph).traverse {
        case c: Clear.Instance => c.right
        case d: Draw.Instance =>
          us.get(d).map(Draw.Drawable(d, _)).toRightXor(UnsetUniformError(d))
      })
  }
}

object GraphTraversal {
  val ordered: GraphTraversal = g => g.ordered
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
      capabilities: Set[GL.Capability],
      colorMask: GL.ColorMask,
      isInstanced: Boolean,
      model: Model.Constructor,
      framebuffer: Framebuffer.Constructor
  ) extends Node.Constructor

  case class Constructed(
      constructor: Constructor,
      framebuffer: Framebuffer.Constructed
  ) extends Node.Constructed

  case class Instance(
      constructor: Constructor,
      uniforms: Map[String, Texture.Uniform],
      model: Model.Instance,
      framebuffer: Framebuffer.Instance,
      numInstances: Int
  ) extends Node.Instance {
    def name: String = toString
    private[gfx] def vertexAttribs: List[GL.Attribute.Constructor] =
      constructor.program.vertex.attributes
    private[gfx] def modelAttribs: List[GL.Attribute.Constructor] =
      model.model.vertex.ref.buffer.attributes
  }

  case class Drawable(
      instance: Instance,
      uniforms: List[GL.Uniform]
  ) extends Node.Drawable
}

object Clear {
  case class Constructor(
      name: String,
      mask: GL.ChannelBitMask,
      framebuffer: Framebuffer.Constructor
  ) extends Node.Constructor

  case class Constructed(constructor: Constructor,
                         framebuffer: Framebuffer.Constructed)
      extends Node.Constructed

  case class Instance(
      constructor: Constructor,
      framebuffer: Framebuffer.Instance
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
                         format: GL.Texture.Format,
                         viewport: Vec2i)
      extends Framebuffer.OutputConstructor {
    private[gfx] def single: Constructed = Constructed(this, false)
    private[gfx] def double: Constructed = Constructed(this, true)
  }

  case class Constructed(constructor: Constructor, isDouble: Boolean)

  case class Instance(name: String, constructor: Constructor)
      extends Uniform
      with Framebuffer.OutputInstance

  case class Image(name: String, format: GL.Texture.Format, viewport: Vec2i)
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

object Model {
  case class Constructor(name: String)
  case class Instance(name: String, constructor: Constructor, model: GL.Model)
}

//TODO: find out what to do with this
//case class Valve(start: Node.Draw, links: List[Link.Pipe])
