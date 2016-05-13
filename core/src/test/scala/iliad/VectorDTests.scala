package iliad

import org.typelevel.discipline.scalatest._
import org.scalatest._
import cats._
import cats.laws.discipline._
import cats.std.all._

import shapeless._
import shapeless.ops.nat.ToInt
import iliad.syntax.vector._

import org.scalacheck.Arbitrary

class VectorDTests extends FunSuite with Discipline {

  implicit def vectorDArbitrary[N <: Nat, A](implicit arb: Arbitrary[A], toInt: ToInt[N]): Arbitrary[VectorD[N, A]] = Arbitrary {
    for {
      a <- arb.arbitrary
    } yield VectorD.fill[N, A](a)
  }

  {
    Functor[VectorD[nat._3, ?]]
    checkAll("VectorD[nat._3, Int]", FunctorTests[VectorD[nat._3, ?]].functor[Int, Int, Int])
  }
}

