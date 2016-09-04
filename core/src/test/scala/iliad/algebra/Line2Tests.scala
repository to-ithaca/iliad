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

class Line2Tests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with Inside with Discipline {

  implicit val floatArbitrary: Arbitrary[Float] = Arbitrary { Gen.choose(-100f, 100f).filter(_ != 0f) }

  checkAll("Line2[Float]", FunctorTests[Line2].functor[Float, Float, Float])

  val θ = 0.0872665 //5 degrees
  val α = 0.1

  val doubleGen = Gen.choose(0.0, 100.0)

  test("line does not intersect with parallel line") {
    forAll(VectorGen.gen[nat._2, Double](doubleGen), 
      VectorGen.gen[nat._2, Double](doubleGen),
      VectorGen.normalGen[nat._2, Double](Arbitrary.arbitrary[Double])) {
      (p0, p1, d) =>
      val l0 = Line2(p0, d)
      val l1 = Line2(p1, d)

      l0.intersection(l1, θ, α) should equal(None)
    }
  }

  test("l0 l0 intersects with l1, l1 intersects with l0") {
    forAll(Line2Gen.gen(doubleGen), 
      Line2Gen.gen(doubleGen)) { (l0, l1) =>
      inside(l0.intersection(l1, 0.0, 0.0)) {
        case None =>
          l1.intersection(l0, 0.0, 0.0) shouldBe empty
        case Some(p0) =>
          inside(l1.intersection(l0, 0.0, 0.0)) {
            case Some(p1) =>
              p0.x should equal (p1.x +- 0.01)
              p0.y should equal (p1.y +- 0.01)
          }
      }
    }
  }

  test("lines with the same origin point can intersect") {
    val p0 = v"0.0 0.0"
    val l0 = Line2.normalized(p0, v"1.0 1.0")
    val l1 = Line2.normalized(p0, v"1.0 5.0")
    l0.intersects(l1, θ, α) should equal(true)
  }

  test("diagonal lines intersect") {
    val l0 = Line2.normalized(v"-1.0 1.0", v"1.0 1.0")
    val l1 = Line2.normalized(v"0.0 0.0", v"1.0 -1.0")
    val pOpt = l0.intersection(l1, θ, α)
    pOpt.nonEmpty should equal(true)
    pOpt.foreach { p => 
      p === v"-1.0 1.0"
    }
  }

  test("horizontal lines can intersect") {
    val l0 = Line2.normalized(v"0.0 0.0", v"1.0 -1.0")
    val l1 = Line2.normalized(v"0.0 1.0", v"1.0 0.0")
    val pOpt = l0.intersection(l1, θ, α)
    pOpt.nonEmpty should equal(true)
    pOpt.foreach { p => 
      p === v"-1.0 1.0"
    }    
  }

  test("vertical lines can intersect") {
    val l0 = Line2.normalized(v"0.0 0.0", v"1.0 -1.0")
    val l1 = Line2.normalized(v"-1.0 0.0", v"0.0 1.0")
    val pOpt = l0.intersection(l1, θ, α)
    pOpt.nonEmpty should equal(true)
    pOpt.foreach { p => 
      p === v"-1.0 1.0"
    }    
  }

  test("normal is perpendicular to direction") {
    forAll(Line2Gen.gen(doubleGen)) { l =>
      (l.direction ⋅ l.normal) should ===(0.0 +- 0.001)
    }
  }

  test("distance gives shortest distance from point") {
    Line2(v"0.0 0.0", v"1.0 0.0").distance(v"3.0 3.0") should ===(3.0 +- 0.001)
  }

  test("contains is true if point is within ds") {
    val l = Line2(v"0.0 0.0", v"1.0 0.0")
    val ds = 2.0

    forAll(Gen.choose(-5.0, 5.0)) { y => 
      val p = v"3.0 $y"
      l.contains(p, ds) should ===(y.abs < ds)
    }
  }

  test("line is parallel to itself") {
    forAll(Line2Gen.gen(doubleGen)) { l => 
      l.parallel(l, α) should be(true)
    }
  }

  test("line is parallel to its own direction") {
    forAll(Line2Gen.gen(doubleGen)) { l => 
      l.parallel(l.direction, α) should be(true)
    }
  }

  test("line is colinear to itself") {
    forAll(Line2Gen.gen(doubleGen)) { l => 
      l.colinearEqv(l, ds = 0.01, α) should be(true)
    }
  }

  test("line equation is in form [y = mx + c]") {
    Line2.normalized(v"0.0 0.0", v"1.0 1.0").equation should be("[y = 1.0 x + 0.0]")
  }

  test("line equation for vertical line is in form [x = const]") {
    Line2(v"2.0 1.0", v"0.0 1.0").equation should be("[x = 2.0]")
  }
}
