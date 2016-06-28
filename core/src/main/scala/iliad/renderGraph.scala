package iliad

import iliad.syntax.all._
import iliad.gl._

import cats._
import cats.data._
import cats.implicits._

import iliad.CatsExtra._

object RenderModel {

  object Texture {
    sealed trait Uniform
    case class Constructor(name: String,
                           format: gl.Texture.Format,
                           viewport: Vec2i)
        extends Output.Constructor
    case class Instance(name: String, constructor: Constructor)
        extends Uniform
        with Output.Instance

    case class Image(name: String, format: gl.Texture.Format, viewport: Vec2i)
        extends Uniform
  }

  object Renderbuffer {
    case class Constructor(name: String,
                           format: RenderbufferInternalFormat,
                           viewport: Vec2i)
        extends Output.Constructor
    case class Instance(name: String, constructor: Constructor)
        extends Output.Instance
  }

  object Output {
    sealed trait Constructor
    sealed trait Instance
  }

  object Framebuffer {
    sealed trait Constructor
    sealed trait Instance

    case object OnScreen extends Constructor with Instance
    case class OffScreenConstructor(
        buffers: List[(FramebufferAttachment, Output.Constructor)])
        extends Constructor

    case class OffScreenInstance(
        instances: List[(FramebufferAttachment, Output.Instance)])
        extends Instance
  }

  object Model {
    case class Constructor(name: String)
    case class Instance(name: String,
                        constructor: Constructor,
                        model: gl.Model)
  }

  sealed trait Node
  object Node {
    sealed trait Constructor {
      def name: String
      def framebuffer: Framebuffer.Constructor
    }
    sealed trait Instance {
      def constructor: Constructor
    }
  }

  object Draw {
    case class Constructor(
        name: String,
        program: Program.Unlinked,
        primitive: PrimitiveType,
        capabilities: List[Capability],
        colorMask: ColorMask,
        isInstanced: Boolean,
        model: Model.Constructor,
        framebuffer: Framebuffer.Constructor
    ) extends Node.Constructor

    case class Piped(
        constructor: Constructor,
        uniforms: List[(String, Texture.Constructor)]
    ) {
      def textureNames: List[String] = uniforms.map(_._1)
    }

    case class Instance(
        piped: Piped,
        uniforms: Map[String, Texture.Uniform],
        model: Model.Instance,
        framebuffer: Framebuffer.Instance,
        numInstances: Int
    ) extends Node.Instance {
      def constructor: Constructor = piped.constructor
      def imageNames =
        constructor.program.textureNames.filterNot(piped.textureNames.toSet)
    }
  }

  object Clear {
    case class Constructor(
        name: String,
        mask: ChannelBitMask,
        framebuffer: Framebuffer.Constructor
    ) extends Node.Constructor

    case class Instance(
        constructor: Constructor,
        framebuffer: Framebuffer.Instance
    ) extends Node.Instance
  }

  sealed trait Link {
    def start: Node.Constructor
    def end: Node.Constructor
  }
  object Link {

    case class Pipe(start: Draw.Constructor,
                    end: Draw.Constructor,
                    uniforms: Map[String, Texture.Constructor])
        extends Link
    case class Order(start: Node.Constructor, end: Node.Constructor)
        extends Link
  }

  //TODO: find out what to do with this
  //case class Valve(start: Node.Draw, links: List[Link.Pipe])

  object Graph {

    case class Constructor(nodes: List[Node.Constructor], links: List[Link]) {
      def put(n: Node.Constructor): Constructor = ???
      def put(l: Link): Constructor = ???
      def constructed: Constructed = Constructed(nodes.toSet, links.toSet)
    }

    object Constructor {
      val empty: Constructor = Constructor(Nil, Nil)
    }

    case class Constructed(nodes: Set[Node.Constructor], links: Set[Link]) {

      def start: Set[Node.Constructor] =
        nodes.filter(n => !links.exists(_.end == n))

      def next(ns: Set[Node.Constructor]): Set[Node.Constructor] =
        links.filter(l => ns.contains(l.start)).map(_.end)

      def end: Set[Node.Constructor] =
        nodes.filter(n => !links.exists(_.start == n))

      def instance: Instance = Instance(this, Nil)
    }

    case class Instance(constructed: Constructed, nodes: List[Node.Instance])
  }
}

object GraphConstruction {
  import RenderModel._

  def put(n: Node.Constructor): State[Graph.Constructor, Unit] =
    State.modify(_.put(n))
  def put(l: Link): State[Graph.Constructor, Unit] =
    State.modify(_.put(l))
}

object GraphValidation {
  import RenderModel._

  type Validate[A] = ReaderT[ValidatedNel[String, ?], Graph.Constructed, A]

  private val nodesConnected: Validate[Unit] = ReaderT { g =>
    def go(start: Set[Node.Constructor],
           connected: Set[Node.Constructor],
           remaining: Set[Node.Constructor]): ValidatedNel[String, Unit] = {
      if (remaining.isEmpty) ().valid
      else {
        val next = g.next(start)
        if (next.isEmpty)
          s"The following group is disconnected ${remaining}".invalidNel
        else go(next, connected ++ next, remaining -- next)
      }
    }

    go(g.start, Set.empty, g.nodes)
  }

  private val linksUnique: Validate[Unit] = ReaderT { g =>
    val dupes = g.links
      .groupBy(l => (l.start, l.end))
      .filter {
        case (_, ls) => ls.size > 1
      }
      .map(_._2)
    if (dupes.nonEmpty)
      s"The following links are duplicates ${dupes.mkString("\n")}".invalidNel
    else ().valid
  }

  private val nodesUnique: Validate[Unit] = ReaderT { g =>
    val dupes = g.nodes
      .groupBy(_.name)
      .filter {
        case (_, ns) => ns.size > 1
      }
      .map(_._2)
    if (dupes.nonEmpty)
      s"The following nodes are non-unique ${dupes.mkString("\n")}".invalidNel
    else ().valid
  }
  private val endNodesOnScreen: Validate[Unit] = ReaderT { g =>
    val offScreen = g.end.filter(_.framebuffer match {
      case Framebuffer.OnScreen => false
      case _ => true
    })
    if (offScreen.nonEmpty)
      s"The following end nodes are off screen ${offScreen}".invalidNel
    else ().valid
  }
  //TODO: check that pipes have valid textures / uniforms

  private val checks: Validate[Unit] =
    nodesUnique *> linksUnique *> nodesConnected *> endNodesOnScreen

  def validate(g: Graph.Constructed): ValidatedNel[String, Unit] =
    checks.run(g)
}

object GraphInstantiation {
  import RenderModel._

  type RValidated[A] = ReaderT[ValidatedNel[String, ?], Graph.Instance, A]

  def hasAllInputTextueres(n: Draw.Instance): ValidatedNel[String, Unit] =
    n.piped.uniforms.traverseUnit {
      case (name, cons) =>
        n.uniforms.get(name) match {
          case Some(u: Texture.Instance) =>
            if (u.constructor == cons) ().valid
            else
              s"Uniform texture $name has inconsistent texture instance $u for node $n".invalidNel
          case Some(u: Texture.Image) =>
            s"Uniform texture $name is image $u instead of texture instance $cons for $n".invalidNel
          case None =>
            s"No texture provided for uniform $name of node $n".invalidNel
        }
    }

  def hasAllImageTextures(n: Draw.Instance): ValidatedNel[String, Unit] =
    n.imageNames.traverseUnit { name =>
      n.uniforms.get(name) match {
        case Some(u: Texture.Image) => ().valid
        case Some(u: Texture.Instance) =>
          s"Uniform image $name is texture instead of image for node $n".invalidNel
        case None =>
          s"No image provided for uniform $name of node $n".invalidNel
      }
    }

  def hasAllAttributes(n: Draw.Instance): ValidatedNel[String, Unit] =
    n.constructor.program.vertex.attributes.traverseUnit { a =>
      if (n.model.model.vertex.ref.buffer.attributes.contains(a)) ().valid
      else s"Attribute $a is not present for node $n".invalidNel
    }

  def hasAllNodes(ns: List[Node.Instance]): RValidated[Unit] = ReaderT { g =>
    val unfilled = g.constructed.links.flatMap { l =>
      if (ns.exists(_.constructor == l.start) && !ns.exists(
              _.constructor == l.end)) {
        Some(l.end)
      } else if (ns.exists(_.constructor == l.end) && !ns.exists(
                     _.constructor == l.start)) {
        Some(l.start)
      } else None
    }
    if (unfilled.nonEmpty)
      s"instance has unfilled nodes ${unfilled}".invalidNel
    else ().valid
  }

  def put(nodes: List[Node.Instance])
    : StateT[ValidatedNel[String, ?], Graph.Instance, Unit] = ???
}

object GraphRunner {
  import RenderModel._
  def run(graph: Graph.Instance): List[DrawOp] = ???
}
/*
object TestGraph {

  val cons = RenderGraph.Constructor.empty

  val vsh = VertexShader.Source("text", List(GLAttribute[Vec2f].attribute("position")), Nil)
  val fsh = FragmentShader.Source("text", List("image" -> Sampler.Constructor.image))
  val program = Program.Unlinked(vsh, fsh)
  val model = RenderGraph.Model.Constructor("model")
  val draw = RenderGraph.Draw.Constructor("basic", program, 
    GL_TRIANGLES,
    Capabilities.depth,
    ColorMask.none,
    false,
    model,
    RenderGraph.Framebuffer.OnScreen
  )
  val instance = cons.put(draw).instance
}
 */
/* We need a state to build up a graph
   We run the state to get the final graph instance
   We validate the instance
   We add a node instance to the graph instance
   We take the graph instance and convert it into a list of draw ops
   We run the draw ops
   We get the load commands out

   Q: Where is the validation?
 **/
/*
object RenderMap {

  /** Switches between multiple links.  All links have the same start node */
  case class Valve(links: List[Link])
}

/** A group of nodes and links */
case class RenderMap(nodes: List[RenderMap.Node],
                     links: List[RenderMap.Link],
                     valves: List[RenderMap.Valve])
 *
 * Any other difficult questions....
 *
 * Q: Once we have a map, how do we add an instance?
 *
 * Any instance must flow from a start node to the end node
 * Certain things are shared across instances
 *
 * Scenario: load and draw the restaurant.
 * The most basic:  We have a single draw drawing untextured vertices to the screen.
 * We add each instance. We need to add vertexData and uniform data only.
 * We can add another instance by adding vertex data
 *
 * More advanced: We want to add shadows to certain objects.
 * For a shadow, we draw all casting objects to the same texture.
 * We then use that texture when drawing all objects to the screen.
 * => we add an instance of a shadow casting object, which means we draw it to the texture
 *
 * Even mode advanced: We want to add lights to certain objects.
 * We draw them with a light source.
 *
 * We want to add bumpmapping to certain objects.
 * We draw them with a normal map.
 *
 * So if we want a shadow casting, bumpmapped, lit object, we go through all the steps.
 *
 * Consider we have multilpe light sources.
 */

/*
trait Loadable {
  def loadCommands[F[_]]: GL.IO[F, Unit]
}

object Basic {

  //this should be 
  val vsh = VertexShader(s"""
#version 300 es

in vec2 position;

void main() {
  gl_Position = vec4(position, 0.0, 1.0);
}

""",
                         List("position" -> ???),
                         Nil,
                         Nil)

  val fsh = FragmentShader(s"""
#version 300 es

out vec4 color;

void main() {
  color = vec4(0.0, 1.0, 0.0, 1.0);
}
""",
                           Nil,
                           Nil,
                           Map.empty)

  val program = Program(vsh, fsh)

  val node = RenderMap.ProgramNode(
      name = "basic",
      program = program,
      primitive = GL_TRIANGLES,
      capabilities = List(GL_DEPTH_TEST),
      isInstanced = false,
      channelBitMask = ChannelBitMask.Empty,
      colorMask = ColorMask(true, true, true, true),
      isOnScreen = true
  )

  //now specific to table
  val bufferInstance = BufferInstance(
      List("position" -> AttributeType[VectorD[nat._2, Float]]),
      GL_TRIANGLES
  )

  val vertexData = Buffer(-1f, -1f, 0f, 1f, 1f, -1f) //size 2 * 3 * 4 = 24
  val elementData = Buffer(0, 1, 2) //size 12

  def loadCommands[F[_]: Monad](gl: GL[F]) =
    for {
      _ <- gl.makeProgram(program)
      _ <- gl.makeModel(
              bufferInstance, GL_ELEMENT_ARRAY_BUFFER, elementData, 12, 2048)
      _ <- gl.makeModel(bufferInstance, GL_ARRAY_BUFFER, vertexData, 24, 2048)
    } yield ()

  //draw commands are part of the node in particular.
  //In a more abstract sense, there is loading the node and loading the model
  //There is drawing the model with the node
  //
  /**
    val instance = node.asGraph.instantiate
    instance.addInstance(node -> tableData)
    -- what does this mean?  tableData consists of a vertexBuffer, elementBuffer reference in a buffer
    -- should be a key e.g. ModelKey
    val tableData = ModelKey("table", vdata, edata)
   load(tableData) andThen instance.addInstance tableData at 30ms
   i.e. load the table data, then add the table instance to the draw at a given time
   we know that the table data has been loaded, because this is done in a stream based system.

   Q: how do we know what instance info to provide?  It would be good to be compile time safe, but not at the moment.
   we assume that we load as soon as we know we are going to draw.
   
   val table = Model("table")
   val chair = Model("chair")

   load(table, chair) andThen addInstance(node -> table) and addInstance(node -> chair)
   //will validate that all info is provided
 */
}

//How do we link buffers? Should we do so?

/**
Imagine we have two nodes, both with the same buffer.
For a given model (table), we want to add it

  addInstance table => {
    node -> table.data1
    node1 -> table.data1, texture1
  }
  so for each node, we specify the data given a model
  we then have all of the instance data.
  and we have a node instance, which contains instances of the textures etc.  
 */

/**
We have a template and an instance
TextureTemplate
RenderbufferTemplate
are used in the graph (instances can be singletons)

when an instance is added, all of the non-singleton variables must be specified

load table andThen 
graph add table {
 bumpMapNode -> Data(table.vertices, table.bumpMapTexture)
 lightDrawNode -> ???
  lightDrawValve -> ???
}
//ERROR: bumpMapNode feeds into lightDrawNode, which is unspecified.
//ERROR: lightDrawNode has valve into draw1, draw2, draw3, which is unspecified
so we start with an entry point, and add things from there

What we have missed:
 - constraining buffers
 - constraining animations
 - pixel draws
 - by default, point buffers are treated like any other
 **/

//constraints can be added in afterwards
 */
