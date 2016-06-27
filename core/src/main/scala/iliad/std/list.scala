package iliad
package std

import scala.reflect._

trait ListInstances {
  implicit def listOps[A](l: List[A]): ListOps[A] = new ListOps(l)
}

final class ListOps[A](l: List[A]) {
  def filterClass[B](implicit ct: ClassTag[B]): List[B] = l.flatMap {
    case b: B => Some(b)
    case _ => None
  }
}
