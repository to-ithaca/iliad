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

class Plane3Tests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with Inside with Discipline { 

  implicit val floatArbitrary: Arbitrary[Float] = Arbitrary { Gen.choose(-100f, 100f).filter(_ != 0f) }

  checkAll("Plane3[Float]", FunctorTests[Plane3].functor[Float, Float, Float])

  test("line intersects with plane within acceptance angle") {
    val p = Plane3(v"0.0 0.0 0.0", v"0.0 0.0 1.0")
    val l = Line3(v"0.0 0.0 0.0", v"0.0 1.0 1.0".normalize)
    val c = cos(Pi / 4.0)

    forAll(Gen.choose(0.0, Pi / 2.0)) { θ => 
      val intersects = c > cos(θ)
      val iOpt = p.intersection(l, θ)
      assert(iOpt.nonEmpty == intersects)
      iOpt.foreach { v => 
        assert(v === v"0.0 0.0 0.0")
      }
    }
  }

  test("line segment intersects with plane if start and end points are on opposite sides") {
    val p = Plane3(v"0.0 0.0 0.0", v"0.0 0.0 1.0")
    val l = LineSegment3(v"0.0 0.0 1.0", v"0.0 0.0 -1.0")
    val ds = 0.0
    val i = p.intersection(l, Pi / 4.0, ds)
    assert(i.nonEmpty)
    i.foreach { pi => assert(pi === v"0.0 0.0 0.0")}
  }

  test("bounded line does not intersect with plane if start and end points are on same side") {
    val p = Plane3(v"0.0 0.0 0.0", v"0.0 0.0 1.0")
    val l = LineSegment3(v"0.0 0.0 2.0", v"0.0 0.0 1.0")
    val ds = 0.0
    val i = p.intersection(l, Pi / 4.0, ds)
    assert(i.isEmpty)
  }

  test("toString is expected") {
    Plane3(v"0 0 0", v"1 0 0").toString should ===("Plane3(Vector(0, 0, 0), Vector(1, 0, 0))")
  }
}
