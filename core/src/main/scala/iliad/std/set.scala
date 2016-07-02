package iliad
package std

trait SetInstances {
  implicit def toSetOps[A](s: Set[A]): SetOps[A] = new SetOps[A](s)
}

final class SetOps[A](s: Set[A]) {
  def duplicates[B](f: A => B): Set[Set[A]] =
    s.groupBy(f)
      .filter {
        case (_, group) => group.size > 1
      }
      .map(_._2)
      .toSet
}
