package iliad
package gl

//should be for creating blocks
/**
Thoughs: we should be able to create an individual node
We shold be able to reference textures
We should be able to reference programs
...
Same as before, maybe we need no syntax, and just validation?
  */

import cats._
import cats.data._
import cats.implicits._

object NodeValidation {
  //TODO: Placeholder
  def validate(d: GraphModel.Draw.Constructor): ValidatedNel[String, Unit] =
    ().valid
}

/*
object OcclusionTest {
  import GraphModel._
  import iliad.implicits._

  val program = Program.Unlinked(
    VertexShader.Source("vertex",
      List(GLAttribute[Vec2f].attribute("position")),
      Nil),
    FragmentShader.Source("fragment",
      Nil))

  val model = Model.Constructor("triangle")
  val framebuffer = Framebuffer.OnScreen

  val draw = Draw.Constructor("triangle",
    program,
    GL_TRIANGLES,
    Set(GL_DEPTH_TEST),
    ColorMask.none,
    false,
    model,
    Framebuffer.OnScreen)

  val clear = Clear.Constructor("screen-clear",
    ChannelBitMask.BitMask(Set(COLOR_BUFFER_BIT, DEPTH_BUFFER_BIT)),
    Framebuffer.OnScreen
  )
}
 */
