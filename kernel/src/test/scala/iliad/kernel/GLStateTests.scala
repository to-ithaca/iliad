package iliad
package kernel

import iliad.kernel.platform.GLES30Library

import org.scalatest._
import org.scalamock.scalatest._

import cats._
import cats.data._
import cats.implicits._

class GLStateTests extends FunSuite with MockFactory {

  //al tracker: GLTracker[Id] = new GLTracker(GL.run)
  val lib: GLES30Library = mock[GLES30Library]
  val state: GLState = GLState.empty

  test("GL commands should be executed in the order they are sent") {
    val vSource = "vertex shader"
    val vsh = VertexShader(vSource, Nil, Nil, Nil) 

    inSequence {
      (lib.glCreateShader _) expects(GL_VERTEX_SHADER.value) returning(42)
      (lib.glShaderSource _) expects(42, 1, *, *)
      (lib.glCompileShader _) expects(42)
    }
    //tracker.shader(vsh).run(lib).run(state)
  }
}
