package iliad

import org.typelevel.discipline.scalatest._
import org.scalatest._

import spire._
import spire.implicits._

import cats._
import cats.kernel.laws._

import shapeless._

import iliad.kernel.vectord._

import org.scalacheck.Arbitrary
import org.scalacheck._
import org.scalatest.prop._

class RectTests extends FunSuite with Discipline with GeneratorDrivenPropertyChecks with Matchers {

  implicit def rectGen[A : algebra.Signed](implicit arb: Arbitrary[A]): Arbitrary[Rect[A]] = Arbitrary {
    for {
      a <- arb.arbitrary
      b <- arb.arbitrary
      c <- arb.arbitrary
      d <- arb.arbitrary
    } yield Rect(v"$a $b", c.abs, d.abs)
  }

  implicit def vectGen[A: algebra.Signed](implicit arb: Arbitrary[A]): Arbitrary[VectorD[nat._2, A]] = Arbitrary {
    for {
      a <- arb.arbitrary
      b <- arb.arbitrary
    } yield v"$a $b"
  }

  { 
    // Int has a Numeric
    spire.math.Numeric[Int]
    
    // Int has an Eq
    implicit val intEq = cats.kernel.std.int.intOrder

    Semigroup[Rect[Int]]
   
    checkAll("Rect[Int]", GroupLaws[Rect[Int]].semigroup)
  }

  {
    // Int has an Eq
    implicit val intEq = cats.kernel.std.int.intOrder
    
    Eq[Rect[Int]]

    checkAll("Rect[Int]", OrderLaws[Rect[Int]].eqv)
  }

  def internalPointGen(implicit rGen: Arbitrary[Rect[Int]]): Gen[(Rect[Int], Vec2i)] = for {
    r <- rGen.arbitrary
    x <- Gen.choose(r.bottomLeft.x, r.topLeft.x)
    y <- Gen.choose(r.bottomLeft.y, r.topLeft.y)
  } yield r -> v"$x $y"


  def externalPointGen(implicit rArb: Arbitrary[Rect[Int]], bArb: Arbitrary[Boolean]): Gen[(Rect[Int], Vec2i)] = for {
    r <- rArb.arbitrary
    xUp <- bArb.arbitrary
    yUp <- bArb.arbitrary
    bothOut <- bArb.arbitrary
    x <- if(xUp) Gen.choose(r.topLeft.x, Integer.MAX_VALUE) else Gen.choose(Integer.MIN_VALUE, r.bottomLeft.x)
    y <- if(bothOut && yUp) Gen.choose(r.topLeft.y, Integer.MAX_VALUE) else if(bothOut) Gen.choose(Integer.MIN_VALUE, r.bottomLeft.y) else Gen.choose(r.bottomLeft.y, r.topLeft.y)
  } yield r -> v"$x $y"

  test("Rect should contain all internal points") {
    forAll(internalPointGen) { case (r, p) => r.contains(p)}
  }

  test("Rect should not contain any external points") {
    forAll(externalPointGen) { case (r, p) => !r.contains(p)}
  }
}
