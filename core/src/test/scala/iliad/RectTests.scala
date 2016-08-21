package iliad

import iliad.syntax.all._

import org.typelevel.discipline.scalatest._
import org.scalatest._

import spire._
import spire.implicits._

import cats._
import cats.kernel.laws._

import shapeless._

import org.scalacheck.Arbitrary
import org.scalacheck._
import org.scalatest.prop._

import arbitrary._

class RectTests extends FunSuite with Discipline with GeneratorDrivenPropertyChecks with Matchers {

  { 
    // Int has a Numeric
    spire.math.Numeric[Int]
    
    // Int has an Eq
    implicit val intEq = cats.std.int.intOrder

    Semigroup[Rect[Int]]
   
    checkAll("Rect[Int]", GroupLaws[Rect[Int]].semigroup)
  }

  {
    // Int has an Eq
    implicit val intEq = cats.std.int.intOrder
    
    Eq[Rect[Int]]

    checkAll("Rect[Int]", OrderLaws[Rect[Int]].eqv)
  }

  def internalPoints(implicit rArb: Arbitrary[Rect[Int]]): Gen[(Rect[Int], Vec2i)] = for {
    r <- rArb.arbitrary
    p <- RectGen.internal(r)
  } yield r -> p

  def externalPoints(implicit rArb: Arbitrary[Rect[Int]]): Gen[(Rect[Int], Vec2i)] = for {
    r <- rArb.arbitrary
    p <- RectGen.external(r)
  } yield r -> p

  test("Rect should contain all internal points") {
    forAll(internalPoints) { case (r, p) => r.contains(p)}
  }

  test("Rect should not contain any external points") {
    forAll(externalPoints) { case (r, p) => !r.contains(p)}
  }
}
