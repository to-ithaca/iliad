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


class PlaneTests extends FunSuite 
    with Discipline 
    with Matchers 
    with GeneratorDrivenPropertyChecks {

    val boundedFloat = Arbitrary(Gen.choose(-10f, 10f).filter(_ != 0f))

  {
    //Float has an Eq
    implicit val floatEq = cats.std.float.floatOrder
   
    Eq[Plane[Float]]

    //Floats are bounded to generate reasonable values
    implicit val bf = boundedFloat

    checkAll("Plane[Float]", OrderLaws[Plane[Float]].eqv)
  }

  test("line intersects with plane within acceptance angle") {
    implicit val floatEq = cats.std.float.floatOrder

    val p = Plane(v"0f 0f 0f", v"0f 0f 1f")
    val l = Line(v"0f 0f 0f", v"0f 1f 1f".normalize)
    val c = Trig[Float].cos(Math.PI.toFloat/4f)

    forAll(Gen.choose(0f, Math.PI.toFloat / 2f)) { θ => 
      val intersects = c > Trig[Float].cos(θ)
      val iOpt = p.intersection(l, θ)
      assert(iOpt.nonEmpty == intersects)
      iOpt.foreach { v => 
        assert(v === v"0f 0f 0f")
      }
    }
  }

  test("bounded line intersects with plane if start and end points are on opposite sides") {
    implicit val floatEq = cats.std.float.floatOrder
    val p = Plane(v"0f 0f 0f", v"0f 0f 1f")
    val l = BoundedLine(v"0f 0f 1f", v"0f 0f -1f")
    val i = p.intersection(l, Math.PI.toFloat/4f)
    assert(i.nonEmpty)
    i.foreach { pi => assert(pi === v"0f 0f 0f")}
  }

test("bounded line does not intersect with plane if start and end points are on same side") {
    implicit val floatEq = cats.std.float.floatOrder
    val p = Plane(v"0f 0f 0f", v"0f 0f 1f")
    val l = BoundedLine(v"0f 0f 2f", v"0f 0f 1f")
    val i = p.intersection(l, Math.PI.toFloat/4f)
    assert(i.isEmpty)
  }

}

class BoundedPlaneTests extends FunSuite 
    with Discipline 
    with Matchers 
    with GeneratorDrivenPropertyChecks {

    val boundedFloat = Arbitrary(Gen.choose(-10f, 10f).filter(_ != 0f))

  {
    //Float has an Eq
    implicit val floatEq = cats.std.float.floatOrder
   
    Eq[BoundedPlane[Float]]

    //Floats are bounded to generate reasonable values
    implicit val bf = boundedFloat

    checkAll("BoundedPlane[Float]", OrderLaws[BoundedPlane[Float]].eqv)
  }

  test("plane normal is calculated by x cross y") {
    implicit val floatEq = cats.std.float.floatOrder
    val p = BoundedPlane(v"0f 0f 0f", v"0f 1f 0f", v"1f 0f 0f")
    p.normal === v"0f 0f 1f"
  }

  test("line intersects with plane within acceptance angle") {
    implicit val floatEq = cats.std.float.floatOrder

    val p = BoundedPlane(v"0f 0f 0f", v"0f 1f 0f", v"1f 0f 0f")
    val l = Line(v"0.5f 0.5f 0f", v"0f 1f 1f".normalize)
    val c = Trig[Float].cos(Math.PI.toFloat/4f)

    forAll(Gen.choose(0f, Math.PI.toFloat / 2f)) { θ => 
      val intersects = c > Trig[Float].cos(θ)
      val iOpt = p.intersection(l, θ)
      assert(iOpt.nonEmpty == intersects)
      iOpt.foreach { v => 
        assert(v === v"0.5f 0.5f 0f")
      }
    }
  }

    test("line does not intersect with plane if outside of plane bounds") {
      val p = BoundedPlane(v"0f 0f 0f", v"0f 1f 0f", v"1f 0f 0f")
      val l = Line(v"2f 2f 0f", v"0f 1f 1f".normalize)

      assert(p.intersection(l, Math.PI.toFloat / 2f).isEmpty)
    }



  test("bounded line intersects with plane if start and end points are on opposite sides") {
    implicit val floatEq = cats.std.float.floatOrder
    val p = BoundedPlane(v"0f 0f 0f", v"0f 1f 0f", v"1f 0f 0f")
    val l = BoundedLine(v"0.5f 0.5f 1f", v"0.5f 0.5f -1f")
    val i = p.intersection(l, Math.PI.toFloat/4f)
    assert(i.nonEmpty)
    i.foreach { pi => assert(pi === v"0.5f 0.5f 0f")}
  }

  test("bounded line does not intersect with plane if start and end points are on same side") {
    implicit val floatEq = cats.std.float.floatOrder
    val p = BoundedPlane(v"0f 0f 0f", v"0f 1f 0f", v"1f 0f 0f")
    val l = BoundedLine(v"0.5f 0.5f 2f", v"0.5f 0.5f 1f")
    val i = p.intersection(l, Math.PI.toFloat/4f)
    assert(i.isEmpty)
  }
}
