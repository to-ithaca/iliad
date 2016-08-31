package iliad
package algebra

import org.typelevel.discipline.scalatest._
import algebra.syntax.vector._
import arbitrary._

import org.scalatest._
import org.scalatest.prop._

import org.scalacheck._

import shapeless._

import spire.laws.GroupLaws
import spire.implicits.{FloatAlgebra => _, _}

import cats.laws.discipline.FunctorTests

class VectorTests extends FunSuite with Discipline with GeneratorDrivenPropertyChecks with Matchers {
  implicit val fuzzyAlgebraFloat: spire.std.FloatAlgebra = new spire.std.FloatAlgebra {
    override def eqv(x: Float, y: Float): Boolean = {
      val percent = Math.abs((x / 20f))
      x === (y +- (Math.max(percent, 0.01f)))
    }
  }

  implicit val arbFloat: Arbitrary[Float] = Arbitrary {
    Gen.choose(-100f, 100f)
  }

  checkAll("Vector[nat._4, Int]", FunctorTests[Vector[nat._4, ?]].functor[Int, Int, Int])

  test("Vector[3, 3].cross.dot === 0") {
    forAll { (v1: Vec3f, v2: Vec3f) =>
      assert(((v1.normalize × v2.normalize) ⋅ v1.normalize) === (0f +- 0.1f))
    }
  }
  test("Vector[3, 3].cross is distributive") {
    forAll { (v1: Vec3f, v2: Vec3f, v3: Vec3f) =>
      assert(v1 × (v2 + v3) === ((v1 × v2) + (v1 × v3)))
    }
  }

  test("Vector basis[2, 3] is the z axis in 3D") {
    Vector.basis[Z, _3D, Int] should ===(v"0 0 1")
  }

  test("cross product follows right hand rule") {
    (v"1f 0f 0f" × v"0f 1f 0f") should ===(v"0f 0f 1f") 
  }

  test("padOne pads a vector with 1") {
    v"0 0".padOne(3) should ===(v"0 0 1")
  }

  test("Vector[2, Int].cmap[Double] is expected") {
    v"1 2".cmap[Double] should ===(v"1.0 2.0")
  }
}
