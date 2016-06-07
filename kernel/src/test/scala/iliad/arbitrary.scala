package iliad

import org.scalacheck._

import shapeless._
import shapeless.ops.nat._

import spire._
import spire.implicits._

import iliad.kernel.vectord._

trait ArbitraryInstances {

  implicit def vectorDArbitrary[N <: Nat, A](implicit arb: Arbitrary[A], toInt: ToInt[N]): Arbitrary[VectorD[N, A]] = Arbitrary {
    for {
      a <- arb.arbitrary
    } yield VectorD.fill[N, A](a)
  }

  implicit def rectArbitrary[A : algebra.Signed](implicit arb: Arbitrary[A]): Arbitrary[Rect[A]] = Arbitrary {
    for {
      a <- arb.arbitrary
      b <- arb.arbitrary
      c <- arb.arbitrary
      d <- arb.arbitrary
    } yield Rect(v"$a $b", c.abs, d.abs)
  }
}

object arbitrary extends ArbitraryInstances
