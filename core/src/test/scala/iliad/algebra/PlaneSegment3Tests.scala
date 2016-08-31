package iliad
package algebra

import iliad.algebra.syntax.vector._

import scala.math._

import org.scalatest._
import org.scalatest.prop._

import spire._
import spire.algebra._
import spire.implicits._

import shapeless._
import shapeless.ops.nat._

import org.scalacheck._

import org.typelevel.discipline.scalatest._
import cats.laws.discipline.FunctorTests

import arbitrary._

class PlaneSegment3Tests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks 
    with Inside with Discipline{

  checkAll("PlaneSegment3[Int]", FunctorTests[PlaneSegment3].functor[Int, Int, Int])

  test("plane normal is calculated by x cross y") {
    val p = PlaneSegment3(v"0.0 0.0 0.0", v"0.0 1.0 0.0", v"1.0 0.0 0.0")
    p.normal === v"0.0 0.0 1.0"
  }

  test("line intersects with plane within acceptance angle") {

    val p = PlaneSegment3(v"0.0 0.0 0.0", v"0.0 1.0 0.0", v"1.0 0.0 0.0")
    val l = Line3(v"0.5 0.5 0.0", v"0.0 1.0 1.0".normalize)
    val c = cos( Pi / 4.0)

    forAll(Gen.choose(0.0, Pi / 2.0)) { θ => 
      val intersects = c > cos(θ)
      val iOpt = p.intersection(l, θ)
      assert(iOpt.nonEmpty == intersects)
      iOpt.foreach { v => 
        assert(v === v"0.5 0.5 0.0")
      }
    }
  }

    test("line does not intersect with plane if outside of plane bounds") {
      val p = PlaneSegment3(v"0.0 0.0 0.0", v"0.0 1.0 0.0", v"1.0 0.0 0.0")
      val l = Line3(v"2.0 2.0 0.0", v"0.0 1.0 1.0".normalize)

      p.intersection(l, Pi / 2.0) shouldBe empty
    }



  test("bounded line intersects with plane if start and end points are on opposite sides") {
    val p = PlaneSegment3(v"0.0 0.0 0.0", v"0.0 1.0 0.0", v"1.0 0.0 0.0")
    val l = LineSegment3(v"0.5 0.5 1.0", v"0.5 0.5 -1.0")
    val i = p.intersection(l, Pi / 4.0)
    inside(i) {
      case Some(p) => p should ===(v"0.5 0.5 0.0")
    }
  }

  test("bounded line does not intersect with plane if start and end points are on same side") {
    val p = PlaneSegment3(v"0.0 0.0 0.0", v"0.0 1.0 0.0", v"1.0 0.0 0.0")
    val l = LineSegment3(v"0.5 0.5 2.0", v"0.5 0.5 1.0")
    val i = p.intersection(l, Pi / 4.0)
    i shouldBe empty
  }
}
