package iliad
package gfx

import iliad.syntax.vectord._

import spire.implicits._
import cats.implicits._
import org.scalatest._

class CameraTests extends FunSuite {

  implicit def L: iliad.kernel.platform.MatrixLibrary = MatrixLibrary

  val camera = Camera(v"0f 0f 1f",
      v"0f 0f -1f",
      v"0f 1f 0f",
      0.01f, 10f,
      1f, 0.74f)


  test("Camera should scroll") {
    val t = System.currentTimeMillis
    println(s"Time is $t")
    val scrollf = Camera.scroll(v"1f 0f 0f", t, 1f, camera)
    assert(scrollf(t).matrix === camera.matrix)
  }
}
