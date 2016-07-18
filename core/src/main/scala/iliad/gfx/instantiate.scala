package iliad
package gfx

import iliad.std.list._
import iliad.gl._

import cats._
import cats.data._
import cats.implicits._

import CatsExtra._

private[iliad] object Instantiate {

  private def framebufferOutput(
      c: Output.Constructor,
      o: Output.Instance): ValidatedNel[GraphMatchError, Unit] =
    (c, o) match {
      case (rc: Renderbuffer.Constructor, ri: Renderbuffer.Instance) =>
        if (ri.constructor == rc) ().valid
        else
          GraphMatchError(
              s"Renderbuffer instance does not match constructor $ri $rc").invalidNel
      case (tc: Texture.Constructor, ti: Texture.Instance) =>
        if (ti.constructor == tc) ().valid
        else
          GraphMatchError(
              s"Texture instance does not match constructor $ti $tc").invalidNel
      case other =>
        GraphMatchError(s"Invalid texture / renderbuffer combination $other").invalidNel
    }

  private def framebuffer(
      n: Draw.Instance): ValidatedNel[GraphMatchError, Unit] = {
    (n.constructor.framebuffer, n.framebuffer) match {
      case (Framebuffer.OnScreen, Framebuffer.OnScreen) =>
        ().valid
      case (Framebuffer.OnScreen, i) =>
        GraphMatchError(
            s"Offscreen framebuffer instance $i provided for node $n").invalidNel
      case (c, Framebuffer.OnScreen) =>
        GraphMatchError(
            s"Onscreen framebuffer instance provided when $c expected for node $n").invalidNel
      case (c: Framebuffer.OffScreenConstructor,
            i: Framebuffer.OffScreenInstance) =>
        c.buffers.traverseUnit {
          case (a, c) =>
            i.instances.toMap.get(a) match {
              case None =>
                GraphMatchError(
                    s"Framebuffer attachment $a missing for draw $n").invalidNel
              case Some(o) => framebufferOutput(c, o)
            }
        }
    }
  }

  private def instanced(
      n: Draw.Instance): ValidatedNel[GraphMatchError, Unit] =
    if (!n.constructor.isInstanced && n.numInstances != 1)
      GraphMatchError(
          s"A non-instanced draw cannot have ${n.numInstances} instances for node $n").invalidNel
    else if (n.numInstances == 0)
      GraphMatchError(s"A draw must have at least one instance for node $n").invalidNel
    else ().valid

  private def textures(n: Draw.Instance): ValidatedNel[GraphMatchError, Unit] =
    n.constructor.program.textureNames.traverseUnit { name =>
      n.uniforms.get(name) match {
        case Some(_) => ().valid
        case None =>
          GraphMatchError(s"No texture provided for uniform $name of node $n").invalidNel
      }
    }

  private def attributes(
      n: Draw.Instance): ValidatedNel[GraphMatchError, Unit] =
    n.vertexAttribs.traverseUnit { a =>
      if (n.modelAttribs.contains(a)) ().valid
      else
        GraphMatchError(s"Attribute $a is not present for node $n").invalidNel
    }

  private def links(
      ns: List[Node.Instance]): ReaderT[ValidatedNel[GraphLinkError, ?],
                                        Graph.Instance,
                                        List[Link.Instance]] =
    ReaderT(_.constructed.links.flatMap { l =>
      val sOpt = ns.find(_.constructor == l.start)
      val eOpt = ns.find(_.constructor == l.end)
      (sOpt, eOpt) match {
        case (Some(s), Some(e)) =>
          Some(Link.Instance(s, e).valid)
        case (Some(s), None) =>
          Some(
              GraphLinkError(
                  s"Unable to find end node for link $l with start $s").invalidNel)
        case (None, Some(e)) =>
          Some(
              GraphLinkError(
                  s"Unable to find end node for link $l with end $e").invalidNel)
        case _ => None
      }
    }.toList.sequence)

  private def lift[E <: GraphicsError](v: ValidatedNel[E, Unit])
    : ReaderT[ValidatedNel[GraphicsError, ?], Graph.Instance, Unit] =
    KleisliExtra.lift(v.leftMap(_.map(identity)))

  private def checks(
      ns: List[Node.Instance]): ReaderT[Xor[NonEmptyList[GraphicsError], ?],
                                        Graph.Instance,
                                        List[Link.Instance]] = {
    val vs = lift(
          ns.filterClass[Draw.Instance]
            .traverseUnit(attributes)
            .leftMap(identity)) *>
        lift(ns.filterClass[Draw.Instance].traverseUnit(textures)) *>
        lift(ns.filterClass[Draw.Instance].traverseUnit(instanced)) *>
        lift(ns.filterClass[Draw.Instance].traverseUnit(framebuffer)) *>
        links(ns).mapF(_.leftMap(_.map(identity)))
    vs.mapF(_.toXor)
  }

  def apply(ns: List[Node.Instance])
    : StateT[Xor[NonEmptyList[GraphicsError], ?], Graph.Instance, Unit] =
    for {
      ls <- StateTExtra.inspect(checks(ns).run)
      _ <- State
            .modify[Graph.Instance](_.put(ns, ls))
            .transformF[Xor[NonEmptyList[GraphicsError], ?], Unit](
                _.value.right)
    } yield ()
}
