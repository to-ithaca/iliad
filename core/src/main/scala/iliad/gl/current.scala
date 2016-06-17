package iliad
package gl

import iliad.CatsExtra._

import cats._
import cats.data._
import cats.free._

object Current {
  type DSL[A] = Free[Current, A]

  private def getContains[A](a: A)(get: Current[Option[A]]): DSL[Boolean] = get.free map (_.contains(a))

  def contains(p: Program.Linked): DSL[Boolean] = getContains(p)(CurrentProgramGet)
  def contains(framebuffer: Int): DSL[Boolean] = getContains(framebuffer)(CurrentFramebufferGet)
  def contains(v: VertexBuffer.Loaded): DSL[Boolean] = getContains(v)(CurrentVertexBufferGet)
  def contains(e: ElementBuffer.Loaded): DSL[Boolean] = getContains(e)(CurrentElementBufferGet)

  def set(p: Program.Linked): DSL[Unit] = CurrentProgramSet(p).free
  def set(framebuffer: Int): DSL[Unit] = CurrentFramebufferSet(framebuffer).free
  def set(v: VertexBuffer.Loaded): DSL[Unit] = CurrentVertexBufferSet(v).free
  def set(e: ElementBuffer.Loaded): DSL[Unit] = CurrentElementBufferSet(e).free

  type CurrentState = String
}

sealed trait Current[A]

case object CurrentProgramGet extends Current[Option[Program.Linked]]
case object CurrentFramebufferGet extends Current[Option[Int]]
case object CurrentVertexBufferGet extends Current[Option[VertexBuffer.Loaded]]
case object CurrentElementBufferGet extends Current[Option[ElementBuffer.Loaded]]

case class CurrentProgramSet(p: Program.Linked) extends Current[Unit]
case class CurrentFramebufferSet(framebuffer: Int) extends Current[Unit]
case class CurrentVertexBufferSet(v: VertexBuffer.Loaded) extends Current[Unit]
case class CurrentElementBufferSet(e: ElementBuffer.Loaded) extends Current[Unit]

private object CurrentParser extends (Current ~> State[Current.CurrentState, ?]) {
  def apply[A](current: Current[A]): State[Current.CurrentState, A] = ???
}
