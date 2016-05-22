package iliad

import org.typelevel.discipline.scalatest._
import org.scalatest._

import spire._
import spire.math._
import spire.implicits._

import cats._
import cats.laws.discipline._
import cats.kernel.laws._
import cats.implicits._

import shapeless._
import shapeless.ops.nat.ToInt

import iliad.implicits._

import org.scalacheck.Arbitrary

class RectTests extends FunSuite with Discipline {

  implicit def rectGen[A : algebra.Signed](implicit arb: Arbitrary[A]): Arbitrary[Rect[A]] = Arbitrary {
    for {
      a <- arb.arbitrary
      b <- arb.arbitrary
      c <- arb.arbitrary
      d <- arb.arbitrary
    } yield Rect(v"$a $b", c.abs, d.abs)
  }

  {
     Semigroup[Rect[Int]]
     checkAll("Rect[Int]", GroupLaws[Rect[Int]].semigroup)
  }

}
