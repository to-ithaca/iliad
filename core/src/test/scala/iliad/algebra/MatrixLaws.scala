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

import org.scalacheck.{Arbitrary, Prop, Gen}
import org.scalacheck.Prop._

import iliad.algebra.arbitrary._

object MatrixLaws {
  def apply[F[_ <: Nat, _ <: Nat, _], A]: MatrixLaws[F, A] = new MatrixLaws[F, A] {}
}

trait MatrixLaws[F[_ <: Nat, _ <: Nat, _], A] extends Laws {
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
      name = "matrixMultiplicativeGroup",
      parent = None,
      s"product.associative([$N,$N] * [$N1, $N])" → forAll((x: F[N, N, A], y: F[N, N, A], z: F[N1, N, A]) =>
        MGNN.product(MGNN.product(x, y), z) === MGNN.product(x, MGNN.product(y, z))
      ),
      //TODO: implement properly
      s"transpose([$N, $N1]).transpose === m" → forAll((x: F[N, N, A]) =>
        MGNN.transpose(MGNN.transpose(x)) === x
      )
    )
  }

  def squareMultiplicativeGroup[N <: Nat](SymmetricGen: Gen[F[N, N, A]])(
    implicit G: SquareMatrixMultiplicativeGroup[F[N, N, A], A],
    EqA: Eq[A],
    ArbA: Arbitrary[A],
    ArbNN: Arbitrary[F[N, N, A]],
    EqNN: Eq[F[N, N, A]],
    toIntN: ToInt[N]
  ) = {
    val N = toIntN()
    new DefaultRuleSet(
      name = "squareMatrixMultiplicativeGroup",
      parent = Some(GroupLaws[F[N, N, A]].additiveGroup.asInstanceOf[RuleSet]),
      s"associative([$N, $N] * [$N, $N])" → forAll((x: F[N, N, A], y: F[N, N, A], z: F[N, N, A]) =>
        (x * y) * z === x * (y * z)
      ),
      s"inverse([$N, $N]) * m === id" → forAll((x: F[N, N, A]) =>
        G.inverse(x) * x === G.id
      ),
      s"symmetric([$N, $N]) is correct" → forAll(SymmetricGen)((x: F[N, N, A]) =>
        G.symmetric(x)
      )
    )
  }

}
