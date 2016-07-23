package iliad

import iliad.syntax.all._

import org.scalacheck._

import shapeless._
import shapeless.ops.nat._

import spire._
import spire.implicits._

trait ArbitraryInstances {

  def boundedIntArbitrary(l: Int, u: Int): Arbitrary[Int] = Arbitrary (
    Gen.choose(l, u)
  )

  //TODO: add traverse instance for Gen
  private def traverse[A](gens: List[Gen[A]]): Gen[List[A]] = 
    gens.foldLeft(Gen.const(List.empty[A])) { (b, gen) => for {
      as <- b
      a <- gen
    } yield a :: as
    }

  implicit def vectorDArbitrary[N <: Nat, A](implicit arb: Arbitrary[A], toInt: ToInt[N]): Arbitrary[VectorD[N, A]] = Arbitrary {
    traverse((0 until toInt()).map(_ => arb.arbitrary).toList)
    .map { els =>
      VectorD.sized[N, A](els.toVector)
    }    
  }

  implicit def matrixDArbitrary[W <: Nat, H <: Nat, A](implicit arb: Arbitrary[A], toIntW: ToInt[W], toIntH : ToInt[H]): Arbitrary[MatrixD[W, H, A]] = Arbitrary {
    traverse((0 until toIntW() * toIntH()).map(_ => arb.arbitrary).toList)
    .map(els => MatrixD.sized[W, H, A](els.toVector))
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
