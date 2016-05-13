package iliad

import org.typelevel.discipline.scalatest._
import org.scalatest._
import cats._
import cats.laws.discipline._
import algebra.laws._
import cats.implicits._
import cats.syntax._

import shapeless._
import shapeless.ops.nat.ToInt

import org.scalacheck.Arbitrary

class VectorDTests extends FunSuite with Discipline {

  implicit val iso = CartesianTests.Isomorphisms.invariant[VectorD[nat._3, ?]]

  implicit def vectorDArbitrary[N <: Nat, A](implicit arb: Arbitrary[A], toInt: ToInt[N]): Arbitrary[VectorD[N, A]] = Arbitrary {
    for {
      a <- arb.arbitrary
    } yield VectorD.fill[N, A](a)
  }

  {
    Functor[VectorD[nat._3, ?]]
    Applicative[VectorD[nat._3, ?]]
    checkAll("VectorD[nat._3, Int]", ApplicativeTests[VectorD[nat._3, ?]].applicative[Int, Int, Int])
  }

  {
    Semigroup[VectorD[nat._3, Int]]
    checkAll("VectorD[nat._3, Int]", GroupLaws[VectorD[nat._3, Int]].semigroup)
  }
}

