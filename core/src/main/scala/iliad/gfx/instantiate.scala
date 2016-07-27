package iliad
package gfx

import iliad.{gl => GL}
import iliad.implicits._

import cats._
import cats.data._
import cats.implicits._

import CatsExtra._

trait InstantiateFunctions {

  def vref(name: String, attributes: Attribute*): GL.VertexData.Ref = {
    val vb = GL.VertexBuffer.Constructor(attributes.toList.map(_.attribute))
    GL.VertexData.Ref(name, vb)
  }

  def vd(vr: GL.VertexData.Ref, range: (Int, Int)): GL.Model.VertexRef = {
    val (s, e) = range
    GL.Model.VertexRef(vr, GL.DataRange(s, e))
  }

  def eref(name: String, bufferName: String): GL.ElementData.Ref =
    GL.ElementData.Ref(name, GL.ElementBuffer.Constructor(bufferName))

  def ed(er: GL.ElementData.Ref, range: (Int, Int)): GL.Model.ElementRef = {
    val (s, e) = range
    GL.Model.ElementRef(er, GL.DataRange(s, e))
  }

  def model(name: String,
            constructorName: String,
            vdata: GL.Model.VertexRef,
            edata: GL.Model.ElementRef): Model.Instance = {
    val m = GL.Model(vdata, edata)
    Model.Instance(name, Model.Constructor(constructorName), m)
  }

  def png(name: String, size: Vec2i): Texture.Image =
    Texture.Image(name, TextureFormat.rgba, size)

  def drawInstance(model: Model.Instance,
                   cons: Draw.Constructor,
                   uniforms: (String, Texture.Uniform)*): Draw.Instance =
    Draw.Instance(cons, uniforms.toMap, model, Framebuffer.OnScreen, 1)

  def clearScreen(c: Clear.Constructor): Clear.Instance =
    Clear.Instance(c, Framebuffer.OnScreen)
}

object Instantiate {

  private def framebufferOutput(
      c: Framebuffer.OutputConstructor,
      o: Framebuffer.OutputInstance): ValidatedNel[InstantiationError, Unit] =
    (c, o) match {
      case (rc: Renderbuffer.Constructor, ri: Renderbuffer.Instance) =>
        if (ri.constructor == rc) ().valid
        else RenderbufferMatchError(rc, ri).invalidNel.widen
      case (tc: Texture.Constructor, ti: Texture.Instance) =>
        if (ti.constructor == tc) ().valid
        else TextureMatchError(tc, ti).invalidNel.widen
      case (tc: Texture.Constructor, ri: Renderbuffer.Instance) =>
        TextureRenderbufferMatchError(tc, ri).invalidNel.widen
      case (rc: Renderbuffer.Constructor, ti: Texture.Instance) =>
        RenderbufferTextureMatchError(rc, ti).invalidNel.widen
    }

  private def framebuffer(
      n: Draw.Instance): ValidatedNel[InstantiationError, Unit] = {
    (n.constructor.framebuffer, n.framebuffer) match {
      case (Framebuffer.OnScreen, Framebuffer.OnScreen) =>
        ().valid
      case (Framebuffer.OnScreen, i: Framebuffer.OffScreenInstance) =>
        OffScreenOnScreenMatchError(i).invalidNel.widen
      case (c: Framebuffer.OffScreenConstructor, Framebuffer.OnScreen) =>
        OnScreenOffScreenMatchError(c).invalidNel.widen
      case (c: Framebuffer.OffScreenConstructor,
            i: Framebuffer.OffScreenInstance) =>
        c.buffers.toList.traverseUnit {
          case (a, c) =>
            i.instances.toMap.get(a) match {
              case None => AttachmentMissingError(a).invalidNel.widen
              case Some(o) => framebufferOutput(c, o)
            }
        }
    }
  }

  private def instanced(
      n: Draw.Instance): ValidatedNel[NumInstanceError, Unit] =
    if (!n.constructor.isInstanced && n.numInstances != 1)
      NumInstanceError(n.constructor.isInstanced, n.numInstances).invalidNel
    else if (n.numInstances == 0) //TODO: have a non-zero integer
      NumInstanceError(n.constructor.isInstanced, n.numInstances).invalidNel
    else ().valid

  private def textures(
      n: Draw.Instance): ValidatedNel[TextureUniformMissingError, Unit] =
    n.constructor.program.textureNames.traverseUnit { name =>
      n.uniforms.get(name) match {
        case Some(_) => ().valid
        case None => TextureUniformMissingError(name).invalidNel
      }
    }

  private def attributes(
      n: Draw.Instance): ValidatedNel[AttributeMissingError, Unit] =
    n.vertexAttribs.traverseUnit { a =>
      if (n.modelAttribs.contains(a)) ().valid
      else AttributeMissingError(a).invalidNel
    }

  private def links(
      ns: List[Node.Instance]): ReaderT[ValidatedNel[InstantiationError, ?],
                                        Graph.Instance,
                                        List[Link.Instance]] =
    ReaderT(_.constructed.links.flatMap { l =>
      val sOpt = ns.find(_.constructor == l.start)
      val eOpt = ns.find(_.constructor == l.end)
      (sOpt, eOpt) match {
        case (Some(s), Some(e)) =>
          Some(Link.Instance(s, e).valid)
        case (Some(s), None) =>
          Some(EndNodeMissingError(l, s).invalidNel.widen[InstantiationError])
        case (None, Some(e)) =>
          Some(
              StartNodeMissingError(l, e).invalidNel.widen[InstantiationError])
        case _ => None
      }
    }.toList.sequence)

  private def validate(
      d: Draw.Instance): ValidatedNel[InstantiationError, Unit] = {
    val v = framebuffer(d) *>
        instanced(d).widen *>
        textures(d).widen *>
        attributes(d).widen
    v.leftMap(_.map(e => NodeInstantiationError(d, e)))
  }

  private def lift(v: ValidatedNel[InstantiationError, Unit])
    : ReaderT[ValidatedNel[InstantiationError, ?], Graph.Instance, Unit] =
    KleisliExtra.lift(v)

  private def checks(ns: List[Node.Instance])
    : ReaderT[Xor[NonEmptyList[InstantiationError], ?],
              Graph.Instance,
              List[Link.Instance]] = {
    val vs =
      lift((ns.filterClass[Draw.Instance].traverseUnit(validate))) *>
        links(ns)
    vs.mapF(_.toXor)
  }

  def apply(ns: List[Node.Instance])
    : StateT[Xor[NonEmptyList[InstantiationError], ?], Graph.Instance, Unit] =
    for {
      ls <- StateTExtra.inspect(checks(ns).run)
      _ <- State
            .modify[Graph.Instance](_.put(ns, ls))
            .transformF[Xor[NonEmptyList[InstantiationError], ?], Unit](
                _.value.right)
    } yield ()
}
