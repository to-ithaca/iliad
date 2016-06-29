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
    State.modify(_.put(n))
  def put(l: Link): State[Graph.Constructor, Unit] =
    State.modify(_.put(l))

  type GValidated = ReaderT[ValidatedNel[String, ?], Graph.Constructed, Unit]

  private val nodesConnected: GValidated = ReaderT { g =>
    def go(start: Set[Node.Constructor],
           connected: Set[Node.Constructor],
           remaining: Set[Node.Constructor]): ValidatedNel[String, Unit] = {
      if (remaining.isEmpty) ().valid
      else {
        val next = g.next(start)
        if (next.isEmpty)
          s"The following group is disconnected $remaining".invalidNel
        else go(next, connected ++ next, remaining -- next)
      }
    }

    go(g.start, Set.empty, g.nodes)
  }

  private val linksUnique: GValidated = ReaderT { g =>
    val dupes = g.links.duplicates(l => (l.start, l.end))
    if (dupes.nonEmpty)
      s"The following links are duplicates ${dupes.mkString("\n")}".invalidNel
    else ().valid
  }

  private val nodesUnique: GValidated = ReaderT { g =>
    val dupes = g.nodes.duplicates(_.name)
    if (dupes.nonEmpty)
      s"The following nodes are non-unique ${dupes.mkString("\n")}".invalidNel
    else ().valid
  }

  private val endNodesOnScreen: GValidated = ReaderT { g =>
    val offScreen = g.end.filter(_.framebuffer match {
      case Framebuffer.OnScreen => false
      case _ => true
    })
    if (offScreen.nonEmpty)
      s"The following end nodes are off screen $offScreen".invalidNel
    else ().valid
  }

  private def pipeTextures: GValidated = ReaderT { g =>
    g.links.toList.filterClass[Link.Pipe].traverseUnit { p =>
      p.start.framebuffer match {
        case Framebuffer.OnScreen =>
          s"Pipe $p starts with on screen framebuffer".invalidNel
        case f: Framebuffer.OffScreenConstructor =>
          val ts = f.buffers.map(_._2).filterClass[Texture.Constructor]
          val unmatched = p.uniforms.values.filterNot(ts.contains)
          if (unmatched.nonEmpty)
            s"Pipe $p references textures $unmatched which are not in start node".invalidNel
          else ().valid
      }
    }
  }
  //TODO: check that pipes have valid textures / uniforms

  def validate(g: Graph.Constructed): ValidatedNel[String, Unit] =
    (nodesUnique *> linksUnique *> nodesConnected *> endNodesOnScreen).apply(g)
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
    n.constructor.program.vertex.attributes.traverseUnit { a =>
      if (n.model.model.vertex.ref.buffer.attributes.contains(a)) ().valid
      else s"Attribute $a is not present for node $n".invalidNel
    }

  private def filledNodes(ns: List[Node.Instance]): GValidated[Unit] =
    ReaderT { g =>
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
        s"instance has unfilled nodes $unfilled".invalidNel
      else ().valid
    }

  private def lift(v: ValidatedNel[String, Unit]): GValidated[Unit] =
    KleisliExtra.lift(v)

  private def checks(ns: List[Node.Instance])
    : ReaderT[Xor[NonEmptyList[String], ?], Graph.Instance, Unit] = {
    val vs = lift(ns.filterClass[Draw.Instance].traverseUnit(attributes)) *>
        lift(ns.filterClass[Draw.Instance].traverseUnit(images)) *>
        lift(ns.filterClass[Draw.Instance].traverseUnit(textures)) *>
        filledNodes(ns)

    vs.transform(
        new (ValidatedNel[String, ?] ~> Xor[NonEmptyList[String], ?]) {
      def apply[A](v: ValidatedNel[String, A]): NonEmptyList[String] Xor A =
        v.toXor
    })
  }

  def put(ns: List[Node.Instance])
    : StateT[Xor[NonEmptyList[String], ?], Graph.Instance, Unit] =
    StateTExtra.modifyT(checks(ns).run)
}
