package iliad
package gfx

import iliad.std.list._
import iliad.std.set._

import cats._
import cats.data._
import cats.implicits._

import CatsExtra._

object Expose {
  def put(n: Node.Constructor): State[Graph.Constructor, Unit] =
    State.modify(_.addNode(n.lNode))
  def put(l: Link): State[Graph.Constructor, Unit] =
    State.modify(_.addEdge(l.lEdge))
}

object Construct {

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
    val dupes = g.nodes.duplicates(_.name)
    validate(dupes.isEmpty,
             s"The following nodes are non-unique ${dupes.mkString("\n")}")
  }

  private val endNodesOnScreen: GValidated = ReaderT { g =>
    val offScreen = g.end.filter(_.framebuffer match {
      case Framebuffer.OnScreen => false
      case _ => true
    })
    validate(offScreen.isEmpty,
             s"The following end nodes are off screen $offScreen")
  }

  private def pipeTextures: GValidated = ReaderT(
    _.links.toList.filterClass[Link.Pipe].traverseUnit { p =>
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

  private def pipeUniforms: GValidated = ReaderT(
    _.links.toList.filterClass[Link.Pipe].traverseUnit { p =>
      val unmatched = p.uniformNames.filterNot(p.endTextureNames.contains)
      validate(
          unmatched.isEmpty,
          s"Pipe $p references uniforms $unmatched which are not in end node")
    })

  private def validate(g: Graph.Constructed): ValidatedNel[String, Graph.Constructed] =
    (nodesUnique *> linksUnique *>
          endNodesOnScreen *> pipeTextures *> pipeUniforms).apply(g).map(_ => g)

  def apply(s: State[Graph.Constructor, Unit]): ValidatedNel[String, Graph.Constructed] =
    validate(Graph.Constructed(s.runS(Graph.empty).value))
}

