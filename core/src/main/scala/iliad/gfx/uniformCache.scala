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
  type State = Map[UniformScope, Map[String, CachedFunction]]
  type Values = Map[UniformScope, Map[String, GL.Uniform.Value]]
  type Effect = StateT[GraphicsError Xor ?, State, Unit]

  private val root = Iso.id[State]

  private[gfx] def apply(a: UniformCache): Effect = a match {
    case UniformPut(scope, fs) =>
      StateT.modifyF(s => (s + (scope -> fs)).right)
    case UniformUpdate(scope, name, f) =>
      StateT.modifyF(
          s =>
            s.get(scope)
              .toRightXor(UnsetScopeError(scope, s.keySet))
              .map(_ => (root ^|-? index(scope) ^|-> at(name) set Some(f))(s)))
    case UniformFold(scope, name, f) =>
      StateT.modifyF { (fs: State) =>
        for {
          us <- fs.get(scope).toRightXor(UnsetScopeError(scope, fs.keySet))
          u <- us.get(name).toRightXor(UnsetUniformError(name, scope))
          cf <- (foldOver(f)(u)).right
        } yield (root ^|-? index(scope) ^|-> at(name) set Some(cf))(fs)
      }
  }

  private[iliad] def foldOver(f: (Long,
                                  Any) => CachedFunction.UniformFunction[Any])(
      u: CachedFunction): CachedFunction.Fold[Any] = u match {
    case prev: CachedFunction.Fold[_] =>
      foldOver(f)(prev.prevf)
    case prev: CachedFunction.UniformFunction[_] =>
      val cast = prev.asInstanceOf[CachedFunction.UniformFunction[Any]]
      CachedFunction.Fold(cast, f)
  }

  private[iliad] def uniformValues(at: Long)
    : CatsState[Map[String, CachedFunction], Map[String, GL.Uniform.Value]] =
    CatsState { fs =>
      val both: Map[String, (GL.Uniform.Value, CachedFunction)] = fs.map {
        case (name, u: CachedFunction.UniformFunction[_]) =>
          name -> (u(at, name), u)
        case (name, f: CachedFunction.Fold[_]) =>
          val uf = f(at)
          name -> (uf(at, name), uf)
      }.toMap
      val next = both.map { case (k, (_, f)) => k -> f }
      val out = both.map { case (k, (value, _)) => k -> value }
      (next, out)
    }

  private[iliad] def values(at: Long): CatsState[State, Values] =
    CatsState { state =>
      val both = state.map {
        case (k, fs) =>
          k -> uniformValues(at).run(fs).value
      }
      val next = both.map { case (k, (fs, _)) => k -> fs }
      val out = both.map { case (k, (_, vs)) => k -> vs }
      (next, out)
    }
}

sealed trait UniformCache

private case class UniformPut(s: UniformScope, fs: Map[String, CachedFunction])
    extends UniformCache
private case class UniformFold[A](s: UniformScope,
                                  name: String,
                                  f: (Long,
                                      A) => CachedFunction.UniformFunction[A])
    extends UniformCache

private case class UniformUpdate(s: UniformScope,
                                 name: String,
                                 f: CachedFunction)
    extends UniformCache

trait UniformCacheFunctions {

  private def lift(a: UniformCache): GFX =
    shapeless.Coproduct[GFX](a)

  def putScope(s: UniformScope, fs: (String, CachedFunction)*): GFX =
    lift(UniformPut(s, fs.toMap))

  def foldUniform[A](s: UniformScope,
                     name: String,
                     f: (Long, A) => CachedFunction.UniformFunction[A]): GFX =
    lift(UniformFold(s, name, f))

  def updateUniform(s: UniformScope, name: String, f: CachedFunction): GFX =
    lift(UniformUpdate(s, name, f))
}
