package iliad
package gl

import cats._
import cats.arrow._
import cats.data._
import cats.free._, Free._
import cats.implicits._

object Cached {
  type DSL[A] = Free[Cached, A]

  def get(vs: VertexShader.Source): DSL[Option[VertexShader.Compiled]] =
    liftF(VertexShaderGet(vs))
  def put(vs: VertexShader.Compiled): DSL[Unit] = liftF(VertexShaderPut(vs))
  def get(fs: FragmentShader.Source): DSL[Option[FragmentShader.Compiled]] =
    liftF(FragmentShaderGet(fs))
  def put(fs: FragmentShader.Compiled): DSL[Unit] =
    liftF(FragmentShaderPut(fs))
  def get(p: Program.Unlinked): DSL[Option[Program.Linked]] =
    liftF(ProgramGet(p))
  def put(p: Program.Linked): DSL[Unit] = liftF(ProgramPut(p))

  type CachedState = String

  def parse: Cached ~> State[CachedState, ?] = ???
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

private object CachedParser extends (Cached ~> State[Cached.CachedState, ?]) {
  def apply[A](cached: Cached[A]): State[Cached.CachedState, A] = ???
}
