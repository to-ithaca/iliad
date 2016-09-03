package iliad
package gfx

import iliad.{gl => GL}
import iliad.implicits._

import cats._
import cats.data._
import cats.implicits._

trait InstantiateFunctions {

  def vDataRef(name: String, attributes: Attribute*): GL.VertexData.Ref = {
    val vb = GL.VertexBuffer.Constructor(attributes.toList.map(_.attribute))
    GL.VertexData.Ref(name, vb)
  }

  def vModelRef(vr: GL.VertexData.Ref, range: (Int, Int)): GL.Model.VertexRef = {
    val (s, e) = range
    GL.Model.VertexRef(vr, GL.DataRange(s, e))
  }

  def eDataRef(name: String, bufferName: String): GL.ElementData.Ref =
    GL.ElementData.Ref(name, GL.ElementBuffer.Constructor(bufferName))

  def eModelRef(er: GL.ElementData.Ref,
                range: (Int, Int)): GL.Model.ElementRef = {
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

  def image[A](name: String)(implicit f: GL.GLTextureFormat[A]): Texture.Image =
    Texture.Image(name, f.format)

  def drawInstance(
      model: Model.Instance,
      cons: Draw.Constructor,
      uniforms: Map[String, UniformScope],
      textureUniforms: (String, Texture.Uniform)*): Draw.Instance =
    Draw.Instance(cons,
                  textureUniforms.toMap,
                  uniforms,
                  model,
                  Framebuffer.OnScreen,
                  1)

  def clearScreen(c: Clear.Constructor): Clear.Instance =
    Clear.Instance(c, Framebuffer.OnScreen)
}

object ValidateNodeInstance {

  type ME[F[_]] = ApplicativeError[F, NonEmptyList[InstantiationError]]

  private def framebufferOutput[F[_]](
      c: Framebuffer.OutputConstructor,
      o: Framebuffer.OutputInstance)(implicit M: ME[F]): F[Unit] =
    (c, o) match {
      case (rc: Renderbuffer.Constructor, ri: Renderbuffer.Instance) =>
        if (ri.constructor == rc) M.pure(())
        else  M.raiseError(NonEmptyList(RenderbufferMatchError(rc, ri)))
      case (tc: Texture.Constructor, ti: Texture.Instance) =>
        if (ti.constructor == tc) M.pure(())
        else M.raiseError(NonEmptyList(TextureMatchError(tc, ti)))
      case (tc: Texture.Constructor, ri: Renderbuffer.Instance) =>
        M.raiseError(NonEmptyList(TextureRenderbufferMatchError(tc, ri)))
      case (rc: Renderbuffer.Constructor, ti: Texture.Instance) =>
        M.raiseError(NonEmptyList(RenderbufferTextureMatchError(rc, ti)))
    }

  private def framebuffer[F[_]](
      n: Draw.Instance)(implicit M: ME[F]): F[Unit] = {
    (n.constructor.framebuffer, n.framebuffer) match {
      case (Framebuffer.OnScreen, Framebuffer.OnScreen) =>
        M.pure(())
      case (Framebuffer.OnScreen, i: Framebuffer.OffScreenInstance) =>
        M.raiseError(NonEmptyList(OffScreenOnScreenMatchError(i)))
      case (c: Framebuffer.OffScreenConstructor, Framebuffer.OnScreen) =>
        M.raiseError(NonEmptyList(OnScreenOffScreenMatchError(c)))
      case (c: Framebuffer.OffScreenConstructor,
            i: Framebuffer.OffScreenInstance) =>
        c.buffers.toList.traverseUnit {
          case (a, c) =>
            i.instances.toMap.get(a) match {
              case None => M.raiseError[Unit](NonEmptyList(AttachmentMissingError(a)))
              case Some(o) => framebufferOutput(c, o)
            }
        }
    }
  }

  private def instanced[F[_]](
      n: Draw.Instance)(implicit M: ME[F]): F[Unit] =
    if (!n.constructor.isInstanced && n.numInstances != 1)
      M.raiseError(NonEmptyList(NumInstanceError(n.constructor.isInstanced, n.numInstances)))
    else if (n.numInstances == 0)
      M.raiseError(NonEmptyList(NumInstanceError(n.constructor.isInstanced, n.numInstances)))
    else M.pure(())

  private def textures[F[_]](
      n: Draw.Instance)(implicit M: ME[F]): F[Unit] =
    n.constructor.program.textureNames.traverseUnit { name =>
      n.textureUniforms.get(name) match {
        case Some(_) => M.pure(())
        case None => M.raiseError[Unit](NonEmptyList(TextureUniformMissingError(name)))
      }
    }

  private def attributes[F[_]](
      n: Draw.Instance)(implicit M: ME[F]): F[Unit] =
    n.vertexAttribs.traverseUnit { a =>
      if (n.modelAttribs.contains(a)) M.pure(())
      else M.raiseError[Unit](NonEmptyList(AttributeMissingError(a)))
    }

  private def pipes[F[_]](n: Draw.Instance, sources: List[Node.Instance], g: Graph.Instance)(implicit E: ME[F]): F[List[Link.Instance]] =
    g.constructed.pipes.toList.filter(_.end == n.constructor).traverse { l =>
        sources.find(_.constructor == l.start) match {
          case Some(start) => E.pure(Link.Instance(start, n))
          case None => E.raiseError[Link.Instance](NonEmptyList(StartNodeMissingError(l, n)))
        }
      }

  def validate[F[_]](d: Draw.Instance, sources: List[Draw.Instance], g: Graph.Instance)(implicit E: ME[F]): F[List[Link.Instance]] = {
    val v = framebuffer(d) *>
        instanced(d) *>
        textures(d) *>
        attributes(d) *>
        pipes(d, sources, g)
    v.handleErrorWith(err => 
      E.raiseError(err.map(e => NodeInstantiationError(d, e))))
  }
}

object Instantiate {

  type XorNel[A] = Xor[NonEmptyList[InstantiationError], A]
  type Effect[A] = StateT[Xor[NonEmptyList[InstantiationError], ?], Graph.Instance, A] 

  private def links(n: Draw.Instance, sources: List[Draw.Instance]): Effect[List[Link.Instance]] = StateT.inspectF { s =>
    val validated = ValidateNodeInstance.validate[ValidatedNel[InstantiationError, ?]](n, sources, s)
    validated.toXor
  }

  def apply(n: Draw.Instance, sources: List[Draw.Instance]): Effect[Unit] = for {
    links <- links(n, sources)
        _ <- Graph.Instance.addNode[XorNel](n)
        _ <- Graph.Instance.addEdges[XorNel](links)
  } yield ()

  def apply(n: Clear.Instance): Effect[Unit] = Graph.Instance.addNode[XorNel](n)
}

object HideInstantiate {

  //TODO: we need to check all the links before removing
  def apply(ns: List[Node.Instance])
    : StateT[Xor[NonEmptyList[InstantiationError], ?], Graph.Instance, Unit] =
    State
      .modify[Graph.Instance](_.remove(ns))
      .transformF[Xor[NonEmptyList[InstantiationError], ?], Unit](
          _.value.right)
}
