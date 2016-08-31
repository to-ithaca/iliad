package iliad
package algebra

import org.scalacheck._

import shapeless._
import shapeless.ops.nat._

import spire.math._
import spire.algebra._

trait ArbitraryInstances {
  implicit def matrixArbitrary[N <: Nat, M <: Nat, A](implicit arbA: Arbitrary[A], toIntN: ToInt[N], toIntM: ToInt[M]): Arbitrary[Matrix[N, M, A]] = Arbitrary {
    MatrixGen.gen(arbA.arbitrary)
  }
  implicit def orthoMatrixArbitrary[N <: Nat, M <: Nat, A](implicit matrixArb: Arbitrary[Matrix[N, M, A]]): Arbitrary[OrthoMatrix[N, M, A]] = Arbitrary {
    matrixArb.arbitrary.map(OrthoMatrix(_))
  }

  implicit def vectorArbitrary[N <: Nat, A](implicit arbA: Arbitrary[A], toInt: ToInt[N]): Arbitrary[Vector[N, A]] = Arbitrary {
    VectorGen.gen(arbA.arbitrary)
  }

  implicit def line2Arbitrary[A](implicit arbA: Arbitrary[A], F: Fractional[A], N: NormedVectorSpace[Vec2[A], A]): Arbitrary[Line2[A]] = Arbitrary {
    Line2Gen.gen(arbA.arbitrary)
  }

  implicit def lineSegment2Arbitrary[A](implicit arbA: Arbitrary[A], E: Eq[A]): Arbitrary[LineSegment2[A]] = Arbitrary {
    LineSegment2Gen.gen(arbA.arbitrary)
  }

  implicit def line3Arbitrary[A](implicit arbA: Arbitrary[A], F: Fractional[A], 
    N: NormedVectorSpace[Vec3[A], A]): Arbitrary[Line3[A]] = Arbitrary {
    Line3Gen.gen(arbA.arbitrary)
  }

  implicit def lineSegment3Arbitrary[A](implicit arbA: Arbitrary[A], E: Eq[A]): Arbitrary[LineSegment3[A]] =
    Arbitrary {
      LineSegment3Gen.gen(arbA.arbitrary)
    }

  implicit def plane3Arbitrary[A](implicit arbA: Arbitrary[A], F: Fractional[A], 
    N: NormedVectorSpace[Vec3[A], A]): Arbitrary[Plane3[A]] = Arbitrary {
    Plane3Gen.gen(arbA.arbitrary)
  }

  implicit def planeSegment3Arbitrary[A](implicit arbA: Arbitrary[A], E: Eq[A]): Arbitrary[PlaneSegment3[A]] =
    Arbitrary {
      PlaneSegment3Gen.gen(arbA.arbitrary)
    }

  implicit def rectArbitrary[A](implicit arbA: Arbitrary[A], S: Signed[A]): Arbitrary[Rect[A]] = Arbitrary {
    RectGen.gen(arbA.arbitrary)
  }
}

object arbitrary extends ArbitraryInstances
