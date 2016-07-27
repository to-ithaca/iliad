package iliad
package gfx

import iliad.{gl => GL}

import cats._
import cats.free._
import cats.data.{State => CatsState, ReaderT, Xor, StateT}
import cats.implicits._

import fs2._
import fs2.util._

import monocle._
import monocle.function.all._
import monocle.syntax.all._
import monocle.std.map._

import scala.util.Try

import CatsExtra._

sealed trait CachedFunction

object CachedFunction {
  sealed trait UniformFunction[A] extends CachedFunction {
    def apply(at: Long): A
    def apply(at: Long, name: String): GL.Uniform.Value
  }

  case class Constant[A](a: A, G: GL.GLUniform[A])
      extends UniformFunction[A]
      with CachedFunction {
    def apply(at: Long): A = a
    def apply(at: Long, name: String): GL.Uniform.Value = G.uniform(name, a)
  }

  case class Time[A](f: Long => A, G: GL.GLUniform[A])
      extends UniformFunction[A]
      with CachedFunction {
    def apply(at: Long): A = f(at)
    def apply(at: Long, name: String): GL.Uniform.Value =
      G.uniform(name, f(at))
  }

  case class Fold[A](prevf: UniformFunction[A],
                     f: (Long, A) => UniformFunction[A])
      extends CachedFunction {
    def apply(at: Long): UniformFunction[A] = {
      val prevA = prevf(at)
      f(at, prevA)
    }
  }
}

trait AnimationFunctions {
  def animation[A](value: A)(
      implicit G: GL.GLUniform[A]): CachedFunction.UniformFunction[A] =
    CachedFunction.Constant(value, G)

  def animation[A](f: Long => A)(
      implicit G: GL.GLUniform[A]): CachedFunction.UniformFunction[A] =
    CachedFunction.Time(f, G)
}

object UniformCache {
  type State = Map[Draw.Instance, Map[String, CachedFunction]]
  type Values = Map[Draw.Instance, List[GL.Uniform.Value]]
  type Effect = StateT[GraphicsError Xor ?, State, Unit]

  private val root = Iso.id[State]

  private[gfx] def apply(a: UniformCache): Effect = a match {
    case UniformPut(n, fs) => StateTExtra.modify(s => (s + (n -> fs)).right)
    case UniformFold(n, name, f) =>
      StateTExtra.modify { (fs: State) =>
        for {
          us <- fs.get(n).toRightXor(UnsetUniformsError(n))
          u <- us.get(name).toRightXor(UnsetUniformError(n, name))
          cf <- u match {
                 case _: CachedFunction.Fold[_] =>
                   DoubleUniformFoldError(n, name).left
                 case prev: CachedFunction.UniformFunction[_] =>
                   //nasty, but we need to get around type erasure somehow
                   Xor
                     .fromTry(Try {
                       val cast =
                         prev.asInstanceOf[CachedFunction.UniformFunction[Any]]
                       //calling toString to throw a ClassCastException here as opposed to later
                       cast.toString
                       cast
                     })
                     .leftMap(e => UniformTypeMatchError(n, name, e))
                     .map(p => CachedFunction.Fold(p, f))
               }
        } yield (root ^|-? index(n) ^|-> at(name) set Some(cf))(fs)
      }
  }

  private[iliad] def uniformValues(at: Long)
    : CatsState[Map[String, CachedFunction], List[GL.Uniform.Value]] =
    CatsState { fs =>
      val both: Map[String, (GL.Uniform.Value, CachedFunction)] = fs.map {
        case (name, u: CachedFunction.UniformFunction[_]) =>
          name -> (u(at, name), u)
        case (name, f: CachedFunction.Fold[_]) =>
          val uf = f(at)
          name -> (uf(at, name), uf)
      }.toMap
      val out = both.values.map(_._1).toList
      val nextS = both.mapValues(_._2)
      (nextS, out)
    }

  private[iliad] def values(at: Long)(
      tup: (Draw.Instance, Map[String, CachedFunction]))
    : (Draw.Instance, (List[GL.Uniform.Value], Map[String, CachedFunction])) = {
    val (d, fs) = tup
    val (nextfs, vs) = uniformValues(at).run(fs).value
    (d, (vs, nextfs))
  }
}

sealed trait UniformCache

private case class UniformPut(n: Draw.Instance,
                              fs: Map[String, CachedFunction])
    extends UniformCache
private case class UniformFold[A](n: Draw.Instance,
                                  name: String,
                                  f: (Long,
                                      A) => CachedFunction.UniformFunction[A])
    extends UniformCache

trait UniformCacheFunctions {

  private def lift(a: UniformCache): Graphics.Graphics =
    shapeless.Coproduct[Graphics.Graphics](a)

  def animate(n: Draw.Instance,
              fs: (String, CachedFunction)*): Graphics.Graphics =
    lift(UniformPut(n, fs.toMap))

  def fold[A](n: Draw.Instance,
              name: String,
              f: (Long,
                  A) => CachedFunction.UniformFunction[A]): Graphics.Graphics =
    lift(UniformFold(n, name, f))
}
