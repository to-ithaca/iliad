package iliad
package gfx

import iliad.{gl => GL}

import cats._
import cats.free._
import cats.data.{State => CatsState, ReaderT, Xor}
import cats.implicits._

import fs2._
import fs2.util._

import CatsExtra._

sealed trait AnimationF {
  def name: String
  def apply(at: Long): GL.Uniform.Value
}

object AnimationF {
  case class Constant(name: String, value: GL.Uniform.Value)
      extends AnimationF {
    def apply(at: Long): GL.Uniform.Value = value
  }

  case class TimeF(name: String, f: Long => GL.Uniform.Value)
      extends AnimationF {
    def apply(at: Long): GL.Uniform.Value = f(at)
  }

  //TODO: what about interpolation between points? Need an interpolation strategy
}

trait AnimationFunctions {
  def animation[A](name: String, value: A)(
      implicit G: GL.GLUniform[A]): AnimationF =
    AnimationF.Constant(name, G.uniform(name, value))

  def animation[A](name: String, f: Long => A)(implicit G: GL.GLUniform[A]): 
      AnimationF = AnimationF.TimeF(name, f.andThen(v => G.uniform(name, v)))
}

object UniformCache {
  type State = Map[Draw.Instance, Map[String, AnimationF]]
  type Values = Map[Draw.Instance, List[GL.Uniform.Value]]
  type Effect = CatsState[State, Unit]

  private[gfx] def apply(a: UniformCache): Effect = a match {
    case AnimationPut(n, fs) => CatsState.modify(_ + (n -> fs))
  }

  private[iliad] def values(at: Long)(
      tup: (Draw.Instance, Map[String, AnimationF]))
    : (Draw.Instance, List[GL.Uniform.Value]) = {
    val (n, fs) = tup
    (n, fs.values.toList.map(_.apply(at)))
  }
}

sealed trait UniformCache

private case class AnimationPut(n: Draw.Instance, fs: Map[String, AnimationF])
    extends UniformCache

trait UniformCacheFunctions {

  private def lift(a: UniformCache): Graphics.Graphics =
    shapeless.Coproduct[Graphics.Graphics](a)

  def animate(n: Draw.Instance, fs: AnimationF*): Graphics.Graphics =
    lift(AnimationPut(n, fs.toList.map(a => (a.name, a)).toMap))
}
