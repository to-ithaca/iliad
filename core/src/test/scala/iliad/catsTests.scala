package iliad

import cats._
import cats.data._
import cats.implicits._
import cats.laws.discipline._

import org.typelevel.discipline.scalatest._

import org.scalatest._
import org.scalacheck._

import CatsExtra._

class CatsTests extends FunSuite with Discipline with Matchers {

  import cats.laws.discipline.eq._

  implicit def stateTArbitrary[F[_]: Applicative, S, A](implicit F: Arbitrary[S => F[(S, A)]]): 
      Arbitrary[StateT[F, S, A]] =
    Arbitrary(F.arbitrary.map(f => StateT(f)))

  implicit def stateTEq[F[_], S, A](implicit S: Arbitrary[S], FSA: Eq[F[(S, A)]], F: FlatMap[F]): 
      Eq[StateT[F, S, A]] =
    Eq.by[StateT[F, S, A], S => F[(S, A)]](state =>
      s => state.run(s))

  implicit val iso = CartesianTests.Isomorphisms.invariant[StateT[Option, Int, ?]]

  {

    implicit val eq0: Eq[XorT[StateT[Option, Int, ?], Unit, Int]] =
      XorT.xorTEq[StateT[Option, Int, ?], Unit, Int]

    //F has a Monad
    MonadError[Option, Unit]

    MonadError[StateT[Option, Int, ?], Unit]

    checkAll("StateT[Option, Int, Int]", 
      MonadErrorTests[StateT[Option, Int, ?], Unit].monadError[Int, Int, Int])
    checkAll("MonadError[StateT[Option, Int, ?], Int]", 
      SerializableTests.serializable(MonadError[StateT[Option, Int, ?], Unit]))
  }
}
