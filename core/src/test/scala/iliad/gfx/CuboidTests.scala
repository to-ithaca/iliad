package iliad
package gfx

import org.scalatest._
import org.scalatest.prop._

import arbitrary._

class CuboidTests extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  val bf = boundedArbitrary[Float](0.1f, 100f)
  val bi = boundedArbitrary[Int](1, 5)
  
  test("should have 6 sides") {
    forAll(cuboidPanelArbitrary(bf, bi).arbitrary) { p =>
      p.vertices.map(_._2).toSet === Set(
        CuboidSurface.Top,
        CuboidSurface.Bottom,
        CuboidSurface.Left,
        CuboidSurface.Right,
        CuboidSurface.Front,
        CuboidSurface.Back
      )
    }
  }

  test("should have correct number of vertices")(pending)
  test("should have correct number of elements")(pending)
}
