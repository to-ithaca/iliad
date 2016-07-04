package iliad
package gl

import iliad.std.list._
import iliad.std.set._

import cats._
import cats.data._
import cats.implicits._

import CatsExtra._

object GraphConstruction {
  import GraphModel._

  def put(n: Node.Constructor): State[Graph.Constructor, Unit] =
    State.modify(_.addNode(n.lNode))
  def put(l: Link): State[Graph.Constructor, Unit] =
    State.modify(_.addEdge(l.lEdge))

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

  private def pipeTextures: GValidated = ReaderT { g =>
    g.links.toList.filterClass[Link.Pipe].traverseUnit { p =>
      p.start.framebuffer match {
        case Framebuffer.OnScreen =>
          s"Pipe $p starts with on screen framebuffer".invalidNel
        case f: Framebuffer.OffScreenConstructor =>
          val unmatched = p.textures.filterNot(f.textures.contains)
          validate(
              unmatched.isEmpty,
              s"Pipe $p references textures $unmatched which are not in start node")
      }
    }
  }

  private def pipeUniforms: GValidated = ReaderT { g =>
    g.links.toList.filterClass[Link.Pipe].traverseUnit { p =>
      val unmatched = p.uniformNames.filterNot(p.endTextureNames.contains)
      validate(
          unmatched.isEmpty,
          s"Pipe $p references uniforms $unmatched which are not in end node")
    }
  }

  def validate(g: Graph.Constructed): ValidatedNel[String, Unit] =
    (nodesUnique *> linksUnique *>
      endNodesOnScreen *> pipeTextures *> pipeUniforms).apply(g)

  def construct(s: State[Graph.Constructor, Unit]): ValidatedNel[String, Graph.Constructed] = {
    val g = Graph.Constructed.fromConstructor(s.runS(Graph.emptyConstructor).value)
    validate(g).map(_ => g)
  }
}

object GraphInstantiation {
  import GraphModel._

  type GValidated[A] = ReaderT[ValidatedNel[String, ?], Graph.Instance, A]

  private def textures(n: Draw.Instance): ValidatedNel[String, Unit] =
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

  private def images(n: Draw.Instance): ValidatedNel[String, Unit] =
    n.imageNames.traverseUnit { name =>
      n.uniforms.get(name) match {
        case Some(u: Texture.Image) => ().valid
        case Some(u: Texture.Instance) =>
          s"Uniform image $name is texture instead of image for node $n".invalidNel
        case None =>
          s"No image provided for uniform $name of node $n".invalidNel
      }
    }

  private def attributes(n: Draw.Instance): ValidatedNel[String, Unit] =
    n.vertexAttribs.traverseUnit { a =>
      if (n.modelAttribs.contains(a)) ().valid
      else s"Attribute $a is not present for node $n".invalidNel
    }

  private def links(ns: List[Node.Instance]): GValidated[List[Link.Instance]] =
    ReaderT { g =>
      g.constructed.links.flatMap { l =>
        val sOpt = ns.find(_.constructor == l.start)
        val eOpt = ns.find(_.constructor == l.end)
        (sOpt, eOpt) match {
          case (Some(s), Some(e)) =>
            Some(Link.Instance(s, e).valid)
          case (Some(s), None) =>
            Some(
                s"Unable to find end node for link $l with start $s".invalidNel)
          case (None, Some(e)) =>
            Some(s"Unable to find end node for link $l with end $e".invalidNel)
          case _ => None
        }
      }.toList.sequence
    }

  private def lift(v: ValidatedNel[String, Unit]): GValidated[Unit] =
    KleisliExtra.lift(v)

  private def checks(
      ns: List[Node.Instance]): ReaderT[Xor[NonEmptyList[String], ?],
                                        Graph.Instance,
                                        List[Link.Instance]] = {
    val vs = lift(ns.filterClass[Draw.Instance].traverseUnit(attributes)) *>
        lift(ns.filterClass[Draw.Instance].traverseUnit(images)) *>
        lift(ns.filterClass[Draw.Instance].traverseUnit(textures)) *>
        links(ns)

    vs.transform(
        new (ValidatedNel[String, ?] ~> Xor[NonEmptyList[String], ?]) {
      def apply[A](v: ValidatedNel[String, A]): NonEmptyList[String] Xor A =
        v.toXor
    })
  }

  def put(ns: List[Node.Instance])
    : StateT[Xor[NonEmptyList[String], ?], Graph.Instance, Unit] =
    for {
      ls <- StateTExtra.inspect(checks(ns).run)
      _ <- State
            .modify[Graph.Instance](_.put(ns, ls))
            .transformF[Xor[NonEmptyList[String], ?], Unit](_.value.right)
    } yield ()
}
