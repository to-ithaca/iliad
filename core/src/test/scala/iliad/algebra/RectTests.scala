package iliad
package algebra

import iliad.algebra.syntax.vector._

import org.scalatest._
import org.scalatest.prop._

import spire._
import spire.algebra._
import spire.math._
import spire.implicits._

import shapeless._
import shapeless.ops.nat._

import org.scalacheck._

import org.typelevel.discipline.scalatest._
import cats.laws.discipline.FunctorTests

import arbitrary._

class RectTests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks 
    with Inside with Discipline {

  checkAll("Rect[Int]", FunctorTests[Rect].functor[Int, Int, Int])

  def internalPoints: Gen[(Rect[Int], Vec2i)] = for {
    r <- RectGen.gen(Arbitrary.arbitrary[Int])
    p <- RectGen.internal(r)
  } yield r -> p

  def externalPoints: Gen[(Rect[Int], Vec2i)] = for {
    r <- RectGen.gen(Arbitrary.arbitrary[Int])
    p <- RectGen.external(r)
  } yield r -> p

  test("Rect should contain all internal points") {
    forAll(internalPoints) { case (r, p) => r.contains(p)}
  }

  test("Rect should not contain any external points") {
    forAll(externalPoints) { case (r, p) => !r.contains(p)}
  }
}
