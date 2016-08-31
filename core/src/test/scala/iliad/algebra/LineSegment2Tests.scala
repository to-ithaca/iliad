package iliad
package algebra

import iliad.algebra.syntax.vector._

import org.scalatest._
import org.scalatest.prop._

import spire._
import spire.algebra._
import spire.math._
import spire.implicits._

import arbitrary._

import shapeless._
import shapeless.ops.nat._

import org.scalacheck._

import org.typelevel.discipline.scalatest._
import cats.laws.discipline.FunctorTests

import arbitrary._

class LineSegment2Tests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks 
    with Inside with Discipline {

  checkAll("LineSegment2[Int]", FunctorTests[LineSegment2].functor[Int, Int, Int])

  val θ = 0.0872665 //5 degrees
  val α = 0.1
  val ds = 0.1


  test("the line of a line segment represents its infinite line") {
    val l = LineSegment2(v"0.0 0.0", v"3.0 0.0").line
    l.direction should ===(v"1.0 0.0")
  }

  test("cmap converts between numeric types") {
   assert(LineSegment2(v"2.0 3.0", v"5.0 6.0").cmap[Int] === LineSegment2(v"2 3", v"5 6"))
  }

  test("line segments only intersect if the intersection point is within bounds") {
    val l0 = LineSegment2(v"-1.0 -1.0", v"1.0 1.0")
    val l2 = LineSegment2(v"1.0 -1.0", v"2.0 -2.0")

    l0.intersects(l2, θ, α, ds) should equal(false)
  }

  test("line segments only intersect if the intersection point is within the acceptance distance") {
    val l0 = LineSegment2(v"-1.0 1.0", v"1.0 1.0")
    val l2 = LineSegment2(v"1.0 1.0", v"1.0 0.0")

    l0.intersects(l2, θ, α, ds) should equal(false)
  }

  test("line segments are overlaid if they are on top of each other") {
    val l0 = LineSegment2(v"0f 1.0", v"2.0 1.0")
    val l1 = LineSegment2(v"1.0 1.0", v"3.0 1.0")
    l0.overlays(l1, 0.1, θ) should equal(true)
    l1.overlays(l0, 0.1, θ) should equal(true)
  }

  test("line segments are overlaid if they are equal to each other") {
    val l0 = LineSegment2(v"3.0 1.0", v"3.0 2.0")
    val l1 = LineSegment2(v"3.0 1.0", v"3.0 2.0")
    l0.overlays(l1, 0.1, θ) should equal(true)
    l1.overlays(l0, 0.1, θ) should equal(true)
  }

  test("line segments are overlaid if they are opposite to each other") {
    val l0 = LineSegment2(v"3.0 1.0", v"3.0 2.0")
    val l1 = LineSegment2(v"3.0 2.0", v"3.0 1.0")
    l0.overlays(l1, 0.1, θ) should equal(true)
    l1.overlays(l0, 0.1, θ) should equal(true)
  }

  test("line segments are overlaid if they overlap each other") {
    val l0 = LineSegment2(v"3.0 1.0", v"3.0 2.0")
    val l1 = LineSegment2(v"3.0 -3.0", v"3.0 5.0")
    l0.overlays(l1, 0.1, θ) should equal(true)
    l1.overlays(l0, 0.1, θ) should equal(true)
  }

  test("line segments are not overlaid if they are consecutive") {
    val l0 = LineSegment2(v"3.0 1.0", v"3.0 2.0")
    val l1 = LineSegment2(v"3.0 2.0", v"3.0 5.0")
    l0.overlays(l1, 0.1, θ) should equal(false)
    l1.overlays(l0, 0.1, θ) should equal(false)
  }

  test("line segments are not overlaid if they are apart") {
    val l0 = LineSegment2(v"3.0 1.0", v"3.0 2.0")
    val l1 = LineSegment2(v"3.0 3.0", v"3.0 5.0")
    l0.overlays(l1, 0.1, θ) should equal(false)
    l1.overlays(l0, 0.1, θ) should equal(false)
  }

}
