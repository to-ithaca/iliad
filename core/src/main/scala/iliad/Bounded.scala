package iliad

import simulacrum.typeclass

@typeclass
trait Bounded[A] {
  def MinValue: A
  def MaxValue: A
}

object Bounded {
def index[A](n: Int)(implicit B: Bounded[A], E: Enum[A]) = {
    @annotation.tailrec
    def go(a: A, n: Int): A = if(n <= 0) a else go(Enum[A].succ(a), n - 1)
    go(Bounded[A].MinValue, n)

  }
}
