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

class Line3Tests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks with Inside { 

  //TODO: check if contains method is in use
  ignore("should contain points on the line") {
    val l = Line3(v"0.0 0.0 0.0", v"1.0 0.0 0.0")

    forAll(Gen.choose(-10.0, 10.0)) { d => 
      val p = v"$d 0.0 0.0"
      l.contains(p) should be(true)
    }
  }
}
