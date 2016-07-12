package iliad
package gl

import cats._
import cats.free._
import cats.data.{State => CatsState, ReaderT, Xor}
import cats.implicits._

import CatsExtra._

sealed trait AnimationF {
  def apply(at: Long): Uniform
}

object AnimationF {
  case class Constant(value: Uniform) extends AnimationF {
    def apply(at: Long): Uniform = value
  }

  case class TimeF(f: Long => Uniform) extends AnimationF {
    def apply(at: Long): Uniform = f(at)
  }

  //TODO: what about interpolation between points? Need an interpolation strategy
}


object Animation {
  type State = Map[GraphModel.Draw.Instance, Map[String, AnimationF]]
  type Effect[A] = CatsState[State, A]
  type DSL[A] = Free[Animation, A]

  //TODO: should I change this to iterable?
  case class Draw(node: GraphModel.Draw.Instance, uniforms: List[Uniform])

  def put(n: GraphModel.Draw.Instance, fs: Map[String, AnimationF]): DSL[Unit] =
    AnimationPut(n, fs).free

  def get(n: GraphModel.Draw.Instance, at: Long): DSL[Option[Animation.Draw]] =
    AnimationGet(n, at).free

  def ensure[A](dsl: DSL[Option[A]], msg: String): DSL[String Xor A] =
    dsl.map(_.toRightXor(msg))
}

sealed trait Animation[A]

case class AnimationPut(n: GraphModel.Draw.Instance, fs: Map[String, AnimationF]) extends Animation[Unit]
case class AnimationGet(n: GraphModel.Draw.Instance, at: Long) extends Animation[Option[Animation.Draw]]

object AnimationParser extends (Animation ~> Animation.Effect) {
  def apply[A](fa: Animation[A]): Animation.Effect[A] = fa match {
    case AnimationPut(n, fs) => CatsState.modify(_ + (n -> fs))
    case AnimationGet(n, at) => CatsState.inspect(_.get(n).map { fs =>
      val us = fs.values.map(_.apply(at)).toList
      Animation.Draw(n, us)
    })
  }
}
