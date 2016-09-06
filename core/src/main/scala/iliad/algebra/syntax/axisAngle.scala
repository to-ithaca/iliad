package iliad
package algebra
package syntax

trait AxisAngleSyntax {
  implicit def toAxisAngleOps[A](a: (A, Vec3[A])): AxisAngleOps[A] =
    new AxisAngleOps(a)
}
