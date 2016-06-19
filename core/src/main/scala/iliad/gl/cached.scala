package iliad
package gl

import iliad.CatsExtra._

import cats._
import cats.data._
import cats.free._
import cats.implicits._

import monocle._
import monocle.macros._
import monocle.function.all._
import monocle.syntax.all._
import monocle.std.map._

object Cached {
  type DSL[A] = Free[Cached, A]
  type Effect[A] = State[CachedState, A]

  def get(vs: VertexShader.Source): DSL[Option[VertexShader.Compiled]] =
    VertexShaderGet(vs).free
  def put(vs: VertexShader.Compiled): DSL[Unit] = VertexShaderPut(vs).free

  def get(fs: FragmentShader.Source): DSL[Option[FragmentShader.Compiled]] =
    FragmentShaderGet(fs).free
  def put(fs: FragmentShader.Compiled): DSL[Unit] =
    FragmentShaderPut(fs).free

  def get(p: Program.Unlinked): DSL[Option[Program.Linked]] =
    ProgramGet(p).free
  def put(p: Program.Linked): DSL[Unit] = ProgramPut(p).free

  def get(v: VertexData.Ref): DSL[Option[VertexData.Loaded]] =
    VertexDataGet(v).free
  def get(b: VertexBuffer.Constructor): DSL[Option[VertexBuffer.Loaded]] =
    VertexBufferGet(b).free
  def put(b: VertexBuffer.Update): DSL[Unit] =
    VertexBufferPut(b.buffer).free >> VertexDataPut(b.data).free

  def get(e: ElementData.Ref): DSL[Option[ElementData.Loaded]] =
    ElementDataGet(e).free
  def get(b: ElementBuffer.Constructor): DSL[Option[ElementBuffer.Loaded]] =
    ElementBufferGet(b).free
  def put(b: ElementBuffer.Update): DSL[Unit] =
    ElementBufferPut(b.buffer).free >> ElementDataPut(b.data).free

  def ensure[A, L](dsl: DSL[Option[A]], l: => L): DSL[L Xor A] =
    dsl.map(_.toRightXor(l))

  case class CachedState(
      vertexShaders: Map[VertexShader.Source, VertexShader.Compiled],
      fragmentShaders: Map[FragmentShader.Source, FragmentShader.Compiled],
      programs: Map[Program.Unlinked, Program.Linked],
      vertexData: Map[VertexData.Ref, VertexData.Loaded],
      elementData: Map[ElementData.Ref, ElementData.Loaded],
      vertexBuffers: Map[VertexBuffer.Constructor, VertexBuffer.Loaded],
      elementBuffers: Map[ElementBuffer.Constructor, ElementBuffer.Loaded])
}

sealed trait Cached[A]

case class VertexShaderGet(vs: VertexShader.Source)
    extends Cached[Option[VertexShader.Compiled]]
case class VertexShaderPut(vs: VertexShader.Compiled) extends Cached[Unit]
case class FragmentShaderGet(fs: FragmentShader.Source)
    extends Cached[Option[FragmentShader.Compiled]]
case class FragmentShaderPut(fs: FragmentShader.Compiled) extends Cached[Unit]
case class ProgramGet(p: Program.Unlinked)
    extends Cached[Option[Program.Linked]]
case class ProgramPut(p: Program.Linked) extends Cached[Unit]
case class VertexDataGet(ref: VertexData.Ref)
    extends Cached[Option[VertexData.Loaded]]
case class VertexDataPut(v: VertexData.Loaded) extends Cached[Unit]
case class VertexBufferGet(c: VertexBuffer.Constructor)
    extends Cached[Option[VertexBuffer.Loaded]]
case class VertexBufferPut(v: VertexBuffer.Loaded) extends Cached[Unit]

case class ElementDataGet(ref: ElementData.Ref)
    extends Cached[Option[ElementData.Loaded]]
case class ElementDataPut(e: ElementData.Loaded) extends Cached[Unit]
case class ElementBufferGet(c: ElementBuffer.Constructor)
    extends Cached[Option[ElementBuffer.Loaded]]
case class ElementBufferPut(e: ElementBuffer.Loaded) extends Cached[Unit]

private object CachedParser extends (Cached ~> Cached.Effect) {
  private val _vertexShaders: Lens[
      Cached.CachedState, Map[VertexShader.Source, VertexShader.Compiled]] =
    GenLens[Cached.CachedState](_.vertexShaders)
  private val _fragmentShaders: Lens[
      Cached.CachedState,
      Map[FragmentShader.Source, FragmentShader.Compiled]] =
    GenLens[Cached.CachedState](_.fragmentShaders)
  private val _programs: Lens[
      Cached.CachedState, Map[Program.Unlinked, Program.Linked]] =
    GenLens[Cached.CachedState](_.programs)
  private val _vertexData: Lens[
      Cached.CachedState, Map[VertexData.Ref, VertexData.Loaded]] =
    GenLens[Cached.CachedState](_.vertexData)
  private val _vertexBuffers: Lens[
      Cached.CachedState, Map[VertexBuffer.Constructor, VertexBuffer.Loaded]] =
    GenLens[Cached.CachedState](_.vertexBuffers)
  private val _elementData: Lens[
      Cached.CachedState, Map[ElementData.Ref, ElementData.Loaded]] =
    GenLens[Cached.CachedState](_.elementData)
  private val _elementBuffers: Lens[
      Cached.CachedState,
      Map[ElementBuffer.Constructor, ElementBuffer.Loaded]] =
    GenLens[Cached.CachedState](_.elementBuffers)

  def apply[A](cached: Cached[A]): Cached.Effect[A] =
    cached match {
      case VertexShaderGet(vs) =>
        State.inspect(_ &|-> _vertexShaders ^|-> at(vs) get)
      case VertexShaderPut(vs) =>
        State.modify(_ &|-> _vertexShaders ^|-> at(vs.source) set Some(vs))
      case FragmentShaderGet(fs) =>
        State.inspect(_ &|-> _fragmentShaders ^|-> at(fs) get)
      case FragmentShaderPut(fs) =>
        State.modify(_ &|-> _fragmentShaders ^|-> at(fs.source) set Some(fs))
      case ProgramGet(p) => State.inspect(_ &|-> _programs ^|-> at(p) get)
      case ProgramPut(p) =>
        State.modify(_ &|-> _programs ^|-> at(p.unlinked) set Some(p))
      case VertexDataGet(d) => State.inspect(_ &|-> _vertexData ^|-> at(d) get)
      case VertexDataPut(d) =>
        State.modify(_ &|-> _vertexData ^|-> at(d.ref) set Some(d))
      case VertexBufferGet(b) =>
        State.inspect(_ &|-> _vertexBuffers ^|-> at(b) get)
      case VertexBufferPut(b) =>
        State.modify(_ &|-> _vertexBuffers ^|-> at(b.constructor) set Some(b))
      case ElementDataGet(d) =>
        State.inspect(_ &|-> _elementData ^|-> at(d) get)
      case ElementDataPut(d) =>
        State.modify(_ &|-> _elementData ^|-> at(d.ref) set Some(d))
      case ElementBufferGet(b) =>
        State.inspect(_ &|-> _elementBuffers ^|-> at(b) get)
      case ElementBufferPut(b) =>
        State.modify(_ &|-> _elementBuffers ^|-> at(b.constructor) set Some(b))
    }
}
