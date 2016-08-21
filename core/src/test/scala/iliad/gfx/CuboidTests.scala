package iliad
package gfx

import iliad.implicits._

import spire.implicits._

import org.scalatest._
import org.scalatest.prop._

import org.scalacheck._

import arbitrary._

class CuboidTests extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  val bf = Gen.choose(0.1f, 100f)
  val bi = Gen.choose(1, 3)
  test("should have 6 sides") {
    forAll(CuboidGen(bf, bi)) { p =>
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

class PlaneTests extends FunSuite with Matchers {

  def average(p: Panel[Vec2d]): Vec2d = 
    p.vertices.foldLeft(v"0.0 0.0")((v0, v1) => v0 + v1) :/ 4.0

  test("unit plane should be centered at 0") {
    val plane = Carpenter.unitPlane[Double]
    plane.vertices should have size 4
    plane.elements should have size 6

    average(plane) should ===(v"0.0 0.0")
  }

  test("unit rect should be centred at 0") {
    val plane = Carpenter.fromRect(Rect.square(v"-1.0 -1.0", 2.0))
    average(plane) should ===(v"0.0 0.0")

  }

}
