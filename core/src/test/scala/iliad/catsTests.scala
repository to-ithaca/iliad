package iliad

import cats._
import cats.data._
import cats.functor._
import cats.implicits._
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import org.typelevel.discipline.scalatest._

import org.scalatest._
import org.scalacheck._

class CatsTests extends FunSuite with Discipline with Matchers with CatsInstances {

  import cats.laws.discipline.eq._

  implicit def stateTArbitrary[F[_]: Applicative, S, A](implicit F: Arbitrary[S => F[(S, A)]]): 
      Arbitrary[StateT[F, S, A]] =
    Arbitrary(F.arbitrary.map(f => StateT(f)))

  implicit def stateTEq[F[_], S, A](implicit S: Arbitrary[S], FSA: Eq[F[(S, A)]], F: FlatMap[F]): 
      Eq[StateT[F, S, A]] =
    Eq.by[StateT[F, S, A], S => F[(S, A)]](state =>
      s => state.run(s))


  implicit def catsLawsArbitraryForKleisli[F[_], A, B](implicit F: Arbitrary[F[B]]): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Kleisli[F, A, B](_ => fb)))


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
 
  {
    //F has a Monad
    Monad[Option]

    MonadReader[StateT[Option, Int, ?], Int]

    checkAll("StateT[Option, Int, Int]", MonadReaderTests[StateT[Option, Int, ?], Int].monadReader[Int, Int, Int])
    checkAll("MonadReader[StateT[Option, Int, ?], Int]", SerializableTests.serializable(MonadReader[StateT[Option, Int, ?], Int]))
  }
 
  { 
    case class Person(name: String)

    val SP = MonadState[State[Person, ?], Person]
    val I = Invariant[MonadState[State[Person, ?], ?]]
    
    implicit val SN = I.imap(SP)(_.name)(Person(_))

    checkAll("State[String, String]" , MonadStateTests[State[String, ?], String].monadState[String, String, String])
  }

  test("Functor.void is expected") {
    List(1, 2, 3).void should ===(List((), (), ()))
  }

  test("Functor.as is expected") {
    List(1, 2, 3).as("a") should ===(List("a", "a", "a"))
  }

  test("Traverse.sequenceM is expected") {
    val fga: List[Option[List[Int]]] = List(Some(List(1, 2)), Some(List(3)), Some(List(4, 5)))
    fga.sequenceM should ===(Some(List(1, 2, 3, 4, 5)))
  }
}
