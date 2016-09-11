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

class LineSegment3Tests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks 
    with Inside with Discipline {

  val doubleGen = Gen.choose(-10.0, 10.0)

  checkAll("LineSegment3[Int]", FunctorTests[LineSegment3].functor[Int, Int, Int])

  test("average of start and end is within bounds") {
    val ds = 0.0
    forAll(LineSegment3Gen.gen(doubleGen)) { l => 
      val p = (l.start + l.end) :/ 2.0
      assert(l.interior(ds)(p))
    }
  }

  test("double of start and end is outside bounds") {
    val ds = 0.0
    forAll(LineSegment3Gen.gen(doubleGen)) { l =>
      val p = l.start + ((l.end - l.start) :* 2.0)
      assert(!l.interior(ds)(p))
    }
  }

  test("start is within bounds") {
    val ds = 0.1
    forAll(LineSegment3Gen.gen(doubleGen)) { l =>
      assert(l.interior(ds)(l.start))
    }
  }

  test("end is within bounds") {
    val ds = 0.1
    forAll(LineSegment3Gen.gen(doubleGen)) { l =>
      assert(l.interior(ds)(l.end))
    }
  }


  test("LineSegment3[Double].interior has a margin of ds") {
    val l = LineSegment3(v"1.0 1.0 0.0", v"1.0 2.0 0.0")
    val ds = 0.1
    l.interior(ds)(v"1.0 2.1 0.0") shouldBe true
    l.interior(ds)(v"1.0 2.2 0.0") shouldBe false
  }

  test("toString is expected") {
    LineSegment3(v"0 1 2", v"3 4 5").toString should ===("LineSegment3(Vector(0, 1, 2), Vector(3, 4, 5))")
  }
}
