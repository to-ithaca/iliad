package iliad
package gfx

import iliad.gl._

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
  type State = Map[Draw.Instance, Map[String, AnimationF]]
  type Values = Map[Draw.Instance, List[Uniform]]
  type Effect = CatsState[State, Unit]

  private[gfx] def apply(a: Animation): Effect = a match {
    case AnimationPut(n, fs) => CatsState.modify(_ + (n -> fs))
  }

  private[iliad] def calculate(at: Long,
                               fs: Map[String, AnimationF]): List[Uniform] =
    fs.values.toList.map(_.apply(at))
}

sealed trait Animation

private case class AnimationPut(n: Draw.Instance, fs: Map[String, AnimationF])
    extends Animation

trait AnimationFunctions {

  private def lift(a: Animation): Graphics = shapeless.Coproduct[Graphics](a)

  def put(n: Draw.Instance, fs: Map[String, AnimationF]): Graphics =
    lift(AnimationPut(n, fs))
}
