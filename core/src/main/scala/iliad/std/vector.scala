package iliad
package std

trait VectorInstances {
  implicit def vectorOps[A](v: Vector[A]): VectorOps[A] = new VectorOps(v)
}

final class VectorOps[A](v: Vector[A]) {
  def get(i: Int): Option[A] = if (v.size > i) Some(v(i)) else None
}
