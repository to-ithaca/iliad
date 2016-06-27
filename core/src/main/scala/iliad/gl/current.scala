package iliad
package gl

import iliad.CatsExtra._

import cats._
import cats.data.{State => CatsState}
import cats.free._

import monocle._
import monocle.macros._
import monocle.syntax.all._

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

  def set(p: Program.Linked): DSL[Unit] = CurrentProgramSet(p).free
  def set(f: Framebuffer.Loaded): DSL[Unit] =
    CurrentFramebufferSet(f).free
  def set(v: VertexBuffer.Loaded): DSL[Unit] = CurrentVertexBufferSet(v).free
  def set(e: ElementBuffer.Loaded): DSL[Unit] = CurrentElementBufferSet(e).free

  case class State(framebuffer: Option[Framebuffer.Loaded],
                   program: Option[Program.Linked],
                   vertexBuffer: Option[VertexBuffer.Loaded],
                   elementBuffer: Option[ElementBuffer.Loaded])

  object State {
    val empty: State = State(None, None, None, None)
  }
}

sealed trait Current[A]

case object CurrentProgramGet extends Current[Option[Program.Linked]]
case object CurrentFramebufferGet extends Current[Option[Framebuffer.Loaded]]
case object CurrentVertexBufferGet extends Current[Option[VertexBuffer.Loaded]]
case object CurrentElementBufferGet
    extends Current[Option[ElementBuffer.Loaded]]

case class CurrentProgramSet(p: Program.Linked) extends Current[Unit]
case class CurrentFramebufferSet(f: Framebuffer.Loaded) extends Current[Unit]
case class CurrentVertexBufferSet(v: VertexBuffer.Loaded) extends Current[Unit]
case class CurrentElementBufferSet(e: ElementBuffer.Loaded)
    extends Current[Unit]

private object CurrentParser extends (Current ~> Current.Effect) {

  private val _program: Lens[Current.State, Option[Program.Linked]] =
    GenLens[Current.State](_.program)

  private val _framebuffer: Lens[Current.State, Option[Framebuffer.Loaded]] =
    GenLens[Current.State](_.framebuffer)

  private val _vertexBuffer: Lens[Current.State, Option[VertexBuffer.Loaded]] =
    GenLens[Current.State](_.vertexBuffer)

  private val _elementBuffer: Lens[Current.State, Option[ElementBuffer.Loaded]] =
    GenLens[Current.State](_.elementBuffer)

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
  }
}
