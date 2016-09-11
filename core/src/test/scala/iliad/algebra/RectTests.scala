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
    r <- Arbitrary.arbitrary[Rect[Int]]
    p <- RectGen.internal(r)
  } yield r -> p

  def externalPoints: Gen[(Rect[Int], Vec2i)] = for {
    r <- Arbitrary.arbitrary[Rect[Int]]
    p <- RectGen.external(r)
  } yield r -> p

  test("Rect should contain all internal points") {
    forAll(internalPoints) { case (r, p) => r.contains(p)}
  }

  test("Rect should not contain any external points") {
    forAll(externalPoints) { case (r, p) => !r.contains(p)}
  }

  test("Rect should overlap overlapping rect") {
    val target = Rect(v"1 1", v"3 3")
    val others = List(
      Rect(v"2 2", v"1 1"),
      Rect(v"0 0", v"2 2"),
      Rect(v"0 3", v"2 2"),
      Rect(v"3 0", v"2 2"),
      Rect(v"3 3", v"2 2")
    )

    others.foreach { o => 
      target.overlaps(o) shouldBe true
      o.overlaps(target) shouldBe true
    }
  }

  test("Rect should not overlap adjacent rect") {
    val target = Rect(v"1 1", v"1 1")
      val others = List(
        Rect(v"0 0", v"1 1"),
        Rect(v"2 2", v"1 1"),
        Rect(v"0 2", v"1 1"),
        Rect(v"2 0", v"1 1"),

        Rect(v"0 1", v"1 1"),
        Rect(v"2 1", v"1 1"),
        Rect(v"1 0", v"1 1"),
        Rect(v"1 2", v"1 1")
      )

    others.foreach { o => 
      target.overlaps(o) shouldBe false
      o.overlaps(target) shouldBe false
    }
  }

  test("Rect should overlap itself") {
    forAll { (r: Rect[Int]) =>
      r.overlaps(r) shouldBe true
    }
  }

  test("Rect.centredAt should centre a rect at a point") {
    val r = Rect.centredAt(v"1.0 1.0", v"2.0 2.0")
    (r === Rect(v"0.0 0.0", v"2.0 2.0")) shouldBe true
  }

  test("toString is expected") {
    Rect(v"1 2", v"3 4").toString should ===("Rect(Vector(1, 2), Vector(3, 4))")
  }
}
