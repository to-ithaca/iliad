package iliad

import iliad.syntax.vectord._

import org.scalatest._
import org.scalatest.prop._

import org.typelevel.discipline.scalatest._

import cats._
import cats.kernel.laws._
import cats.implicits._

import spire.algebra.{Field, Trig}
import spire.implicits._

import arbitrary._

import shapeless._
import shapeless.ops.nat._

import org.scalacheck._

class LineTests extends FunSuite 
    with Discipline with Matchers 
    with GeneratorDrivenPropertyChecks {

  val boundedFloat = Arbitrary(Gen.choose(-10f, 10f).filter(_ != 0f))

  {
    //Float has an Eq
    Eq[Float]
   
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
    Eq[Float]
   
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

class Line2Tests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with Discipline {

  val boundedFloat: Arbitrary[Float] = boundedArbitrary(0f, 1000f)

  val θ = 0.0872665f //5 degrees
  val α = 0.1f


    {
    //Float has an Eq
    Eq[Float]
   
    Eq[Line2[Float]]

    //Floats are bounded to generate reasonable values
    implicit val bf = boundedFloat

    checkAll("Line2[Float]", OrderLaws[Line2[Float]].eqv)
  }


  test("line does not intersect with parallel line") {
    forAll(vectorDArbitrary[nat._2, Float](boundedFloat, ToInt[nat._2]).arbitrary,
      vectorDArbitrary[nat._2, Float](boundedFloat, ToInt[nat._2]).arbitrary, 
      normalArbitrary[nat._2, Float].arbitrary) { (p0, p1, d) =>
      val l0 = Line2(p0, d)
      val l1 = Line2(p1, d)

      l0.intersection(l1, θ, α) should equal(None)
    }
  }

  test("lines with the same origin point can intersect") {
    val p0 = v"0f 0f"
    val l0 = Line2.line(p0, v"1f 1f")
    val l1 = Line2.line(p0, v"1f 5f")
    l0.intersects(l1, θ, α) should equal(true)
  }

  test("diagonal lines intersect") {
    val l0 = Line2.line(v"-1f 1f", v"1f 1f")
    val l1 = Line2.line(v"0f 0f", v"1f -1f")
    val pOpt = l0.intersection(l1, θ, α)
    pOpt.nonEmpty should equal(true)
    pOpt.foreach { p => 
      p === v"-1f 1f"
    }
  }

  test("horizontal lines can intersect") {
    val l0 = Line2.line(v"0f 0f", v"1f -1f")
    val l1 = Line2.line(v"0f 1f", v"1f 0f")
    val pOpt = l0.intersection(l1, θ, α)
    pOpt.nonEmpty should equal(true)
    pOpt.foreach { p => 
      p === v"-1f 1f"
    }    
  }

  test("vertical lines can intersect") {
    val l0 = Line2.line(v"0f 0f", v"1f -1f")
    val l1 = Line2.line(v"-1f 0f", v"0f 1f")
    val pOpt = l0.intersection(l1, θ, α)
    pOpt.nonEmpty should equal(true)
    pOpt.foreach { p => 
      p === v"-1f 1f"
    }    
  }
}

class BoundedLine2Tests extends FunSuite with Matchers with Discipline {

  val θ = 0.0872665f //5 degrees
  val α = 0.1f
  val ds = 0.1f

  {
    //Float has an Eq
    Eq[Float]
   
    Eq[BoundedLine2[Float]]

    //Floats are bounded to generate reasonable values
    implicit val bf = boundedArbitrary(0f, 1000f)

    checkAll("BoundedLine2[Float]", OrderLaws[BoundedLine2[Float]].eqv)
  }


  test("bounded lines only intersect if the intersection point is within bounds") {
    val l0 = BoundedLine2(v"-1f -1f", v"1f 1f")
    val l2 = BoundedLine2(v"1f -1f", v"2f -2f")

    l0.intersects(l2, θ, α, ds) should equal(false)
  }

  test("bounded lines only intersect if the intersection point is within the acceptance distance") {
    val l0 = BoundedLine2(v"-1f 1f", v"1f 1f")
    val l2 = BoundedLine2(v"1f 1.05f", v"1f 0f")

    l0.intersects(l2, θ, α, ds) should equal(false)
  }

  test("bounded lines are overlaid if they are on top of each other") {
    val l0 = BoundedLine2(v"0f 1f", v"2f 1f")
    val l1 = BoundedLine2(v"1f 1f", v"3f 1f")
    l0.overlays(l1, 0.1f, 0.1f) should equal(true)
  }
}
