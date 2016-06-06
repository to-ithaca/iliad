package iliad

trait Bounded[A] {
  def MinValue: A
  def MaxValue: A
}

object Bounded {
  def apply[A](implicit B: Bounded[A]): Bounded[A] = B
}
