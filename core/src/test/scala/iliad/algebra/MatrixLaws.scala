package iliad
package algebra

import shapeless._
import shapeless.ops.nat._

import spire._
import spire.math._
import spire.algebra._
import spire.implicits._

import spire.laws._

import org.typelevel.discipline._

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop._

import iliad.algebra.arbitrary._

object MatrixLaws {
  def apply[F[_ <: Nat, _ <: Nat, _], A]: MatrixLaws[F, A] = new MatrixLaws[F, A] {}
}

trait MatrixLaws[F[_ <: Nat, _ <: Nat, _], A] extends Laws {

//  implicit def EqA: Eq[A]
//  implicit def ArbA: Arbitrary[A]

  def multiplicativeGroup[N <: Nat, N1 <: Nat](
    implicit MGNN: MatrixMultiplicativeGroup[F, N, N, A],
    EqN1N: Eq[F[N1, N, A]],
    EqNN: Eq[F[N, N, A]],
    toIntN: ToInt[N],
    toIntN1: ToInt[N1],
    ArbNN: Arbitrary[F[N, N, A]],
    Arb1N: Arbitrary[F[N1, N, A]]
  ) = {
    val N = toIntN()
    val N1 = toIntN1()
    new DefaultRuleSet (
      name = "multiplicativeGroup",
      parent = None,
      s"product.associative([$N,$N] x [$N1, $N])" → forAll((x: F[N, N, A], y: F[N, N, A], z: F[N1, N, A]) =>
        MGNN.product(MGNN.product(x, y), z) === MGNN.product(x, MGNN.product(y, z))
      )
    )
  }

}
