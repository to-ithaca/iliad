package iliad

import simulacrum.typeclass

@typeclass
trait Bounded[A] {
  def MinValue: A
  def MaxValue: A
}

object Bounded {
  def element[A: Bounded: Enum](n: Int): A = {
    @annotation.tailrec
    def go(a: A, n: Int): A = if (n <= 0) a else go(Enum[A].succ(a), n - 1)
    go(Bounded[A].MinValue, n)
  }

  def indexOf[A: Bounded: Enum](a: A): Int = {
    @annotation.tailrec
    def go(a: A, aa: A, n: Int): Int =
      if (aa == a) n else go(a, Enum[A].succ(aa), n + 1)
    go(a, Bounded[A].MinValue, 0)
  }
}
