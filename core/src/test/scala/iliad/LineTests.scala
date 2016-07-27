package iliad

import iliad.syntax.vectord._

import org.scalatest._
import org.scalatest.prop._

import org.typelevel.discipline.scalatest._

import cats._
import cats.kernel.laws._

import spire.algebra.{Field, Trig}
import spire.implicits._

import arbitrary._

import shapeless._
import org.scalacheck._

class LineTests extends FunSuite 
    with Discipline with Matchers 
    with GeneratorDrivenPropertyChecks {

  val boundedFloat = Arbitrary(Gen.choose(-10f, 10f).filter(_ != 0f))

  {
    //Float has an Eq
    implicit val floatEq = cats.std.float.floatOrder
   
    Eq[Line[Float]]

    //Floats are bounded to generate reasonable values
    implicit val bf = boundedFloat

    checkAll("Line[Float]", OrderLaws[Line[Float]].eqv)
  }
}

class BoundedLineTests extends FunSuite 
    with Discipline
    with Matchers
    with GeneratorDrivenPropertyChecks {

    val boundedFloat = Arbitrary(Gen.choose(-10f, 10f).filter(_ != 0f))

  {
    //Float has an Eq
    implicit val floatEq = cats.std.float.floatOrder
   
    Eq[BoundedLine[Float]]

    //Floats are bounded to generate reasonable values
    implicit val bf = boundedFloat

    checkAll("BoundedLine[Float]", OrderLaws[BoundedLine[Float]].eqv)
  }

  test("average of start and end is within bounds") {
    implicit val bf = boundedFloat
    forAll(boundedLineArbitrary[Float].arbitrary) { l => 
      val p = (l.start + l.end) :/ 2f
      assert(l.withinBounds(p))
    }
  }

  test("double of start and end is outside bounds") {
    implicit val bf = boundedFloat
    forAll(boundedLineArbitrary[Float].arbitrary) { l =>
      val p = l.start + ((l.end - l.start) :* 2f)
      assert(!l.withinBounds(p))
    }
  }
}
