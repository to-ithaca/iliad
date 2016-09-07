package iliad
package gfx

import simulacrum.typeclass

import cats._
import cats.implicits._

@typeclass
trait Render[A] {
  def show(a: A): List[GFX]
  def hide(a: A): List[GFX]

  def showFirst(a: A)(implicit U: Update[A]): List[GFX] = U.update(a) ++ show(a)
}

object Render {
  implicit def foldableRender[F[_], A](implicit R: Render[A], F: Foldable[F], FF: Functor[F]): Render[F[A]] =
    new Render[F[A]] {
      def show(fa: F[A]): List[GFX] = fa.map(_.show).fold
      def hide(fa: F[A]): List[GFX] = fa.map(_.hide).fold
    }
}
