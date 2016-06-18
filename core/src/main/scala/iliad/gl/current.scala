package iliad
package gl

import iliad.CatsExtra._

import cats._
import cats.data._
import cats.free._

import monocle._
import monocle.macros._
import monocle.function.all._
import monocle.syntax.all._
import monocle.std.option._

object Current {
  type DSL[A] = Free[Current, A]
  type Effect[A] = State[CurrentState, A]

  private def getContains[A](a: A)(get: Current[Option[A]]): DSL[Boolean] =
    get.free map (_.contains(a))

  def contains(p: Program.Linked): DSL[Boolean] =
    getContains(p)(CurrentProgramGet)
  def contains(framebuffer: Int): DSL[Boolean] =
    getContains(framebuffer)(CurrentFramebufferGet)
  def contains(v: VertexBuffer.Loaded): DSL[Boolean] =
    getContains(v)(CurrentVertexBufferGet)
  def contains(e: ElementBuffer.Loaded): DSL[Boolean] =
    getContains(e)(CurrentElementBufferGet)

  def set(p: Program.Linked): DSL[Unit] = CurrentProgramSet(p).free
  def set(framebuffer: Int): DSL[Unit] =
    CurrentFramebufferSet(framebuffer).free
  def set(v: VertexBuffer.Loaded): DSL[Unit] = CurrentVertexBufferSet(v).free
  def set(e: ElementBuffer.Loaded): DSL[Unit] = CurrentElementBufferSet(e).free

  case class CurrentState(framebuffer: Option[Int],
                          program: Option[Program.Linked],
                          vertexBuffer: Option[VertexBuffer.Loaded],
                          elementBuffer: Option[ElementBuffer.Loaded])
}

sealed trait Current[A]

case object CurrentProgramGet extends Current[Option[Program.Linked]]
case object CurrentFramebufferGet extends Current[Option[Int]]
case object CurrentVertexBufferGet extends Current[Option[VertexBuffer.Loaded]]
case object CurrentElementBufferGet
    extends Current[Option[ElementBuffer.Loaded]]

case class CurrentProgramSet(p: Program.Linked) extends Current[Unit]
case class CurrentFramebufferSet(framebuffer: Int) extends Current[Unit]
case class CurrentVertexBufferSet(v: VertexBuffer.Loaded) extends Current[Unit]
case class CurrentElementBufferSet(e: ElementBuffer.Loaded)
    extends Current[Unit]

private object CurrentParser extends (Current ~> Current.Effect) {

  private val _program: Lens[Current.CurrentState, Option[Program.Linked]] =
    GenLens[Current.CurrentState](_.program)

  private val _framebuffer: Lens[Current.CurrentState, Option[Int]] =
    GenLens[Current.CurrentState](_.framebuffer)

  private val _vertexBuffer: Lens[
      Current.CurrentState, Option[VertexBuffer.Loaded]] =
    GenLens[Current.CurrentState](_.vertexBuffer)

  private val _elementBuffer: Lens[
      Current.CurrentState, Option[ElementBuffer.Loaded]] =
    GenLens[Current.CurrentState](_.elementBuffer)

  def apply[A](current: Current[A]): Current.Effect[A] = current match {
    case CurrentProgramGet => State.inspect(_ &|-> _program get)
    case CurrentProgramSet(p) => State.modify(_ &|-> _program set Some(p))
    case CurrentFramebufferGet => State.inspect(_ &|-> _framebuffer get)
    case CurrentFramebufferSet(f) =>
      State.modify(_ &|-> _framebuffer set Some(f))
    case CurrentVertexBufferGet => State.inspect(_ &|-> _vertexBuffer get)
    case CurrentVertexBufferSet(b) =>
      State.modify(_ &|-> _vertexBuffer set Some(b))
    case CurrentElementBufferGet => State.inspect(_ &|-> _elementBuffer get)
    case CurrentElementBufferSet(b) =>
      State.modify(_ &|-> _elementBuffer set Some(b))
  }
}
