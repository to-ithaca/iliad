package iliad
package gl

import iliad.algebra._
import iliad.implicits._

import org.scalatest._
import org.scalatest.prop._

class GLUniformTests extends FunSuite {

  GLUniform[Int]
  GLUniform[Vec2i]
  GLUniform[Vec3i]
  GLUniform[Vec4i]

  GLUniform[Float]
  GLUniform[Vec2f]
  GLUniform[Vec3f]
  GLUniform[Vec4f]

  GLUniform[Double]
  GLUniform[Vec2d]
  GLUniform[Vec3d]
  GLUniform[Vec4d]

  GLUniform[Mat2f]
  GLUniform[Mat3f]
  GLUniform[Mat4f]

  GLUniform[Mat2d]
  GLUniform[Mat3d]
  GLUniform[Mat4d]
}
