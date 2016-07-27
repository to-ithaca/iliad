package iliad
package std

import scala.reflect._

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

  def filterClass[B](implicit ct: ClassTag[B]): Set[B] = s.flatMap {
    case b: B => Some(b)
    case _ => None
  }
}
