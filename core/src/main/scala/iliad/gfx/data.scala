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

object Output {
  sealed trait Constructor
  sealed trait Instance
}

object Texture {
  sealed trait Uniform
  case class Constructor(name: String,
                         format: GL.Texture.Format,
                         viewport: Vec2i)
      extends Output.Constructor {
    private[gfx] def single: Constructed = Constructed(this, false)
    private[gfx] def double: Constructed = Constructed(this, true)
  }

  private[gfx] case class Constructed(constructor: Constructor,
                                      isDouble: Boolean)

  case class Instance(name: String, constructor: Constructor)
      extends Uniform
      with Output.Instance

  case class Image(name: String, format: GL.Texture.Format, viewport: Vec2i)
      extends Uniform
}

object Renderbuffer {
  case class Constructor(name: String,
                         format: GL.RenderbufferInternalFormat,
                         viewport: Vec2i)
      extends Output.Constructor
  case class Instance(name: String, constructor: Constructor)
      extends Output.Instance
}

object Framebuffer {
  sealed trait Constructor
  private[gfx] sealed trait Constructed
  sealed trait Instance

  case object OnScreen extends Constructor with Constructed with Instance

  case class OffScreenConstructor(
      buffers: List[(GL.FramebufferAttachment, Output.Constructor)])
      extends Constructor {
    private[gfx] def textures: List[Texture.Constructor] =
      buffers.map(_._2).filterClass[Texture.Constructor]
  }

  private[gfx] case class OffScreenConstructed(
      constructor: OffScreenConstructor,
      textures: List[Texture.Constructed]
  ) extends Constructed

  case class OffScreenInstance(
      instances: List[(GL.FramebufferAttachment, Output.Instance)])
      extends Instance
}

object Model {
  case class Constructor(name: String)
  case class Instance(name: String, constructor: Constructor, model: GL.Model)
}

sealed trait Node
object Node {
  sealed trait Constructor {
    def name: String
    def framebuffer: Framebuffer.Constructor
    private[gfx] def lNode: LNode[Constructor, String] = LNode(this, name)
  }

  private[gfx] sealed trait Constructed {
    def constructor: Constructor
  }

  sealed trait Instance {
    def name: String
    def constructor: Constructor
    private[gfx] def lNode: LNode[Instance, String] = LNode(this, name)
  }
  private[iliad] sealed trait Drawable
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

  private[gfx] case class Constructed(
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

  private[iliad] case class Drawable(
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

//TODO: find out what to do with this
//case class Valve(start: Node.Draw, links: List[Link.Pipe])

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

  type Traversal = QInstance => Vector[Node.Instance]

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
      : Reader[Algorithm, AnimationError Xor Vector[Node.Drawable]] =
      Reader[Algorithm, AnimationError Xor Vector[Node.Drawable]](
          _.apply(graph).traverse {
        case c: Clear.Instance => c.right
        case d: Draw.Instance =>
          us.get(d)
            .map(Draw.Drawable(d, _))
            .toRightXor(AnimationError(s"Uniforms for node $d do not exist"))
      })
  }
}

object Algorithm {
  val ordered: Algorithm = g => g.ordered
}

abstract class DrawType(val primitive: GL.PrimitiveType)
object DrawType {
  case object Triangles extends DrawType(GL.GL_TRIANGLES)
  case object Points extends DrawType(GL.GL_POINTS)
}

abstract class Dimension(val capabilities: Set[GL.Capability])
object Dimension {
  case object _2D extends Dimension(Set.empty)
  case object _3D extends Dimension(Set(GL.GL_DEPTH_TEST))
}

sealed trait GraphicsError extends IliadError

case class AnimationError(msg: String) extends GraphicsError
case class GraphMatchError(msg: String) extends GraphicsError
case class GraphLinkError(msg: String) extends GraphicsError
