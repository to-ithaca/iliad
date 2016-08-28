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

import spire.std.int._


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

  test("Vector[3, 3].cross.dot === 0") {
    forAll { (v1: Vec3f, v2: Vec3f) =>
      assert(((v1 × v2) ⋅ v1) === (0f +- 0.1f))
    }
  }
  test("Vector[3, 3].cross is distributive") {
    forAll { (v1: Vec3f, v2: Vec3f, v3: Vec3f) =>
      assert(v1 × (v2 + v3) === ((v1 × v2) + (v1 × v3)))
    }
  }
}
