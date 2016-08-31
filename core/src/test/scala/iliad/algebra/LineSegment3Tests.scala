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
    forAll(LineSegment3Gen.gen(doubleGen)) { l => 
      val p = (l.start + l.end) :/ 2.0
      assert(l.interior(p))
    }
  }

  test("double of start and end is outside bounds") {
    forAll(LineSegment3Gen.gen(doubleGen)) { l =>
      val p = l.start + ((l.end - l.start) :* 2.0)
      assert(!l.interior(p))
    }
  }
}
