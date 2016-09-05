package iliad
package gfx

import simulacrum.typeclass

@typeclass
trait Render[A] {
  def show(a: A): List[GFX]
  def hide(a: A): List[GFX]

  def showFirst(a: A)(implicit U: Update[A]): List[GFX] = U.update(a) ++ show(a)
}
