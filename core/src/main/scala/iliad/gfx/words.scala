package iliad
package gfx

import iliad.{gl => GL}

abstract class DrawType(val primitive: GL.PrimitiveType)
object DrawType {
  case object Triangles extends DrawType(GL.GL_TRIANGLES)
  case object Points extends DrawType(GL.GL_POINTS)
}

abstract class Dimension(val capabilities: Map[GL.Capability, Boolean])
object Dimension {
  case object _2D extends Dimension(Map(GL.GL_DEPTH_TEST -> false))
  case object _3D extends Dimension(Map(GL.GL_DEPTH_TEST -> true))
}

sealed trait VshParameter
sealed trait FshParameter

case class Attribute(attribute: GL.Attribute.Constructor) extends VshParameter
object Attribute {
  def apply[A](name: String)(implicit G: GL.GLAttribute[A]): Attribute =
    Attribute(GL.GLAttribute[A].attribute(name))
}

case class Uniform(uniform: GL.Uniform.Constructor)
    extends VshParameter
    with FshParameter
object Uniform {
  def apply[A](name: String)(implicit G: GL.GLUniform[A]): Uniform =
    Uniform(GL.Uniform.Constructor(name))
}

case class Sampler(name: String, constructor: GL.Sampler.Constructor)
    extends VshParameter
    with FshParameter
object Sampler {
  def image(name: String): Sampler =
    Sampler(name, GL.Sampler.Constructor.image)
}
