package iliad
package gfx

import simulacrum.typeclass

import iliad._
import iliad.{gl => GL}

import cats._
import cats.implicits._

@typeclass
trait Update[A] {
  def update(a: A): List[GFX]
}

object Update {
  implicit def scopePropertyUpdate[A](implicit G: GL.GLUniform[A]): Update[(ScopeProperty, A)] = 
    new Update[(ScopeProperty, A)] {
      def update(a: (ScopeProperty, A)): List[GFX] = {
        val (prop, value) = a
        List(gfx.putUniform(prop, value))
      }
    }

  implicit def scopePropertyFuncUpdate[A](implicit G: GL.GLUniform[A]): Update[(ScopeProperty, Long => A)] =
    new Update[(ScopeProperty, Long => A)] {
      def update(a: (ScopeProperty, Long => A)): List[GFX] = {
        val (prop, f) = a
        List(gfx.putUniform(prop, f))
      }
    }

  implicit def scopePropertyFuncModify[A](implicit G: GL.GLUniform[A]): 
      Update[(ScopeProperty, (Long, A) => Long => A)] =
    new Update[(ScopeProperty, (Long, A) => Long => A)] {
      def update(a: (ScopeProperty, (Long, A) => Long => A)): List[GFX] = {
        val (prop, f) = a
        List(gfx.modifyUniformFunc[A](prop, f))
      }
    }

  implicit def scopePropertyConstModify[A](implicit G: GL.GLUniform[A]):
      Update[(ScopeProperty, (Long, A) => A)] =
    new Update[(ScopeProperty, (Long, A) => A)] {
      def update(a: (ScopeProperty, (Long, A) => A)): List[GFX] = {
        val (prop, f) = a
        List(gfx.modifyUniformConst[A](prop, f))
      }
    }

  implicit def foldableUpdate[F[_], A](implicit U: Update[A], F: Foldable[F], FF: Functor[F]): Update[F[A]] = 
    new Update[F[A]] {
      def update(fa: F[A]): List[GFX] = fa.map(_.update).fold
    }
}
