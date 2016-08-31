package spire

import spire.math._
import spire.algebra._

import cats.Functor

trait SpireInstances {
  implicit def toFunctorOps[F[_], A](fa: F[A]): FunctorOps[F, A] = new FunctorOps(fa)
  implicit def catsEq[A](implicit E: spire.algebra.Eq[A]): cats.Eq[A] = new CatsEq[A] {
    val F = E
  }
}

final class FunctorOps[F[_], A](val fa: F[A]) {
  def cmap[B](implicit M: Functor[F], CT: ConvertableTo[B], CF: ConvertableFrom[A]): F[B] = 
    M.map(fa)(a => CT.fromType[A](a))
}

sealed trait CatsEq[A] extends cats.Eq[A] {
  def F: spire.algebra.Eq[A]
  def eqv(x: A, y: A): Boolean = F.eqv(x, y)
}
