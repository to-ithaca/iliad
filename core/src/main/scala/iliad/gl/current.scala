package iliad
package gl

import iliad.algebra._
import iliad.algebra.syntax.vector._

import cats._
import cats.data.{State => CatsState}
import cats.free._

import monocle._
import monocle.macros._
import monocle.syntax.all._
import monocle.std.map._
import monocle.function.all._

import spire.implicits.IntAlgebra

object Current {
  type DSL[A] = Free[Current, A]
  type Effect[A] = CatsState[State, A]

  private def getContains[A](a: A)(get: Current[Option[A]]): DSL[Boolean] =
    get.free map (_.contains(a))

  def contains(p: Program.Linked): DSL[Boolean] =
    getContains(p)(CurrentProgramGet)
  def contains(f: Framebuffer.Loaded): DSL[Boolean] =
    getContains(f)(CurrentFramebufferGet)
  def contains(v: VertexBuffer.Loaded): DSL[Boolean] =
    getContains(v)(CurrentVertexBufferGet)
  def contains(e: ElementBuffer.Loaded): DSL[Boolean] =
    getContains(e)(CurrentElementBufferGet)
  def contains(m: ColorMask): DSL[Boolean] =
    getContains(m)(CurrentColorMaskGet)
  def containsClearColour(c: Vec4f): DSL[Boolean] =
    getContains(c)(CurrentClearColourGet)
  def contains(m: BlendMode): DSL[Boolean] =
    getContains(m)(CurrentBlendModeGet)
  def contains(f: BlendFunction): DSL[Boolean] =
    getContains(f)(CurrentBlendFunctionGet)

  def contains(viewport: Rect[Int]): DSL[Boolean] =
    CurrentViewportGet.free.map(_ === viewport)

  def get(c: Capability): DSL[Option[Boolean]] =
    CurrentCapabilityGet(c).free

  def set(p: Program.Linked): DSL[Unit] = CurrentProgramSet(p).free
  def set(f: Framebuffer.Loaded): DSL[Unit] =
    CurrentFramebufferSet(f).free
  def set(v: VertexBuffer.Loaded): DSL[Unit] = CurrentVertexBufferSet(v).free
  def set(e: ElementBuffer.Loaded): DSL[Unit] = CurrentElementBufferSet(e).free
  def set(m: ColorMask): DSL[Unit] = CurrentColorMaskSet(m).free
  def setClearColour(c: Vec4f): DSL[Unit] = CurrentClearColourSet(c).free
  def set(m: BlendMode): DSL[Unit] = CurrentBlendModeSet(m).free
  def set(f: BlendFunction): DSL[Unit] = CurrentBlendFunctionSet(f).free
  def set(viewport: Rect[Int]): DSL[Unit] = CurrentViewportSet(viewport).free
  def enable(c: Capability): DSL[Unit] = CurrentCapabilitySet(c, true).free
  def disable(c: Capability): DSL[Unit] = CurrentCapabilitySet(c, false).free

  case class State(framebuffer: Option[Framebuffer.Loaded],
                   program: Option[Program.Linked],
                   vertexBuffer: Option[VertexBuffer.Loaded],
                   elementBuffer: Option[ElementBuffer.Loaded],
                   colorMask: Option[ColorMask],
                   clearColour: Option[Vec4f],
                   blendMode: Option[BlendMode],
                   blendFunction: Option[BlendFunction],
                   capabilities: Map[Capability, Boolean],
                   viewport: Rect[Int])

  object State {
    def empty(screenDimensions: Vec2i): State = State(None, None, None, None, None, None, None, None, 
      Map.empty,
      Rect(v"0 0", screenDimensions))
  }
}

sealed trait Current[A]

case object CurrentProgramGet extends Current[Option[Program.Linked]]
case object CurrentFramebufferGet extends Current[Option[Framebuffer.Loaded]]
case object CurrentVertexBufferGet extends Current[Option[VertexBuffer.Loaded]]
case object CurrentElementBufferGet
    extends Current[Option[ElementBuffer.Loaded]]
case object CurrentColorMaskGet extends Current[Option[ColorMask]]
case class CurrentCapabilityGet(c: Capability) extends Current[Option[Boolean]]
case object CurrentClearColourGet extends Current[Option[Vec4f]]
case object CurrentBlendModeGet extends Current[Option[BlendMode]]
case object CurrentBlendFunctionGet extends Current[Option[BlendFunction]]
case object CurrentViewportGet extends Current[Rect[Int]]

case class CurrentProgramSet(p: Program.Linked) extends Current[Unit]
case class CurrentFramebufferSet(f: Framebuffer.Loaded) extends Current[Unit]
case class CurrentVertexBufferSet(v: VertexBuffer.Loaded) extends Current[Unit]
case class CurrentElementBufferSet(e: ElementBuffer.Loaded)
    extends Current[Unit]
case class CurrentColorMaskSet(m: ColorMask) extends Current[Unit]
case class CurrentCapabilitySet(c: Capability, value: Boolean)
    extends Current[Unit]
case class CurrentClearColourSet(c: Vec4f) extends Current[Unit]
case class CurrentBlendModeSet(m: BlendMode) extends Current[Unit]
case class CurrentBlendFunctionSet(f: BlendFunction) extends Current[Unit]
case class CurrentViewportSet(r: Rect[Int]) extends Current[Unit]

object CurrentParser extends (Current ~> Current.Effect) {

  private val _program: Lens[Current.State, Option[Program.Linked]] =
    GenLens[Current.State](_.program)

  private val _framebuffer: Lens[Current.State, Option[Framebuffer.Loaded]] =
    GenLens[Current.State](_.framebuffer)

  private val _vertexBuffer: Lens[Current.State, Option[VertexBuffer.Loaded]] =
    GenLens[Current.State](_.vertexBuffer)

  private val _elementBuffer: Lens[Current.State, Option[ElementBuffer.Loaded]] =
    GenLens[Current.State](_.elementBuffer)

  private val _colorMask: Lens[Current.State, Option[ColorMask]] =
    GenLens[Current.State](_.colorMask)

  private val _capabilities: Lens[Current.State, Map[Capability, Boolean]] =
    GenLens[Current.State](_.capabilities)

  private val _clearColour: Lens[Current.State, Option[Vec4f]] =
    GenLens[Current.State](_.clearColour)

  private val _blendMode: Lens[Current.State, Option[BlendMode]] =
    GenLens[Current.State](_.blendMode)

  private val _blendFunction: Lens[Current.State, Option[BlendFunction]] =
    GenLens[Current.State](_.blendFunction)

  private val _viewport: Lens[Current.State, Rect[Int]] =
    GenLens[Current.State](_.viewport)

  def apply[A](current: Current[A]): Current.Effect[A] = current match {
    case CurrentProgramGet => CatsState.inspect(_ &|-> _program get)
    case CurrentProgramSet(p) => CatsState.modify(_ &|-> _program set Some(p))
    case CurrentFramebufferGet => CatsState.inspect(_ &|-> _framebuffer get)
    case CurrentFramebufferSet(f) =>
      CatsState.modify(_ &|-> _framebuffer set Some(f))
    case CurrentVertexBufferGet => CatsState.inspect(_ &|-> _vertexBuffer get)
    case CurrentVertexBufferSet(b) =>
      CatsState.modify(_ &|-> _vertexBuffer set Some(b))
    case CurrentElementBufferGet =>
      CatsState.inspect(_ &|-> _elementBuffer get)
    case CurrentElementBufferSet(b) =>
      CatsState.modify(_ &|-> _elementBuffer set Some(b))
    case CurrentColorMaskGet => CatsState.inspect(_ &|-> _colorMask get)
    case CurrentColorMaskSet(m) =>
      CatsState.modify(_ &|-> _colorMask set Some(m))
    case CurrentCapabilityGet(c) =>
      CatsState.inspect(_ &|-> _capabilities ^|-> at(c) get)
    case CurrentCapabilitySet(c, v) =>
      CatsState.modify(_ &|-> _capabilities ^|-> at(c) set Some(v))
    case CurrentClearColourGet => CatsState.inspect(_ &|-> _clearColour get)
    case CurrentClearColourSet(c) =>
      CatsState.modify(_ &|-> _clearColour set Some(c))
    case CurrentBlendModeGet => CatsState.inspect(_ &|-> _blendMode get)
    case CurrentBlendModeSet(m) =>
      CatsState.modify(_ &|-> _blendMode set Some(m))
    case CurrentBlendFunctionGet => CatsState.inspect(_ &|-> _blendFunction get)
    case CurrentBlendFunctionSet(f) =>
      CatsState.modify(_ &|-> _blendFunction set Some(f))
    case CurrentViewportGet => CatsState.inspect(_ &|-> _viewport get)
    case CurrentViewportSet(r) => 
      CatsState.modify(_ &|-> _viewport set r)
  }
}
