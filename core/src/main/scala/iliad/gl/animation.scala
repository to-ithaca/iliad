package iliad
package gl

import cats._
import cats.free._
import cats.data.{State => CatsState, ReaderT, Xor}
import cats.implicits._

import fs2._
import fs2.util._

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
  type Calculated = Map[GraphModel.Draw.Instance, List[Uniform]]
  type Effect = CatsState[State, Unit]

  //TODO: should I change this to iterable?
  case class Draw(node: GraphModel.Draw.Instance, uniforms: List[Uniform])

  def put(n: GraphModel.Draw.Instance, fs: Map[String, AnimationF]): Animation =
    AnimationPut(n, fs)

  def parse(a: Animation): Effect = a match {
    case AnimationPut(n, fs) => CatsState.modify(_ + (n -> fs))
  }

  implicit def S: Strategy = 
    Strategy.fromFixedDaemonPool(8, "animations")

  def calculate(at: Long, fs: Map[String, AnimationF]): List[Uniform] = 
    fs.values.toList.map(_.apply(at))

  def calculate(at: Long, as: State): Error Xor Calculated = ???
}

sealed trait Animation

case class AnimationPut(n: GraphModel.Draw.Instance, fs: Map[String, AnimationF]) extends Animation
