package iliad

import monocle._
import monocle.macros._
import monocle.syntax.all._

import org.typelevel.discipline.scalatest._
import org.scalatest._
import org.scalatest.prop._

import cats._
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.kernel.laws._
import cats.implicits._

import org.scalacheck._

import MonocleExtra._

case class Person(name: Name)
case class Name(first: String, second: String)

trait MonocleTests {
  val _name: Lens[Person, Name] = GenLens[Person](_.name)
  val _firstName: Lens[Person, String] = _name ^|-> GenLens[Name](_.first)
  val _secondName: Lens[Person, String] = _name ^|-> GenLens[Name](_.second)

  implicit val personArbitrary: Arbitrary[Person] = Arbitrary {
    val s = implicitly[Arbitrary[String]].arbitrary
    for {
      first <- s
      second <- s
    } yield Person(Name(first, second))
  }

}

class LensTests extends FunSuite with Discipline with MonocleTests {

  import cats.laws.discipline.eq._

  implicit def lensEq[S, A](implicit S: Arbitrary[S], E: Eq[A]): Eq[Lens[S, A]] =
    Eq.by[Lens[S, A], S => A](_.get)

  implicit val iso = CartesianTests.Isomorphisms.invariant[Lens[Person, ?]]

  implicit val lensArbitrary: Arbitrary[Lens[Person, String]] = Arbitrary {
    val boolean = implicitly[Arbitrary[Boolean]].arbitrary
    boolean map (b => if(b) _firstName else _secondName)
  }

  checkAll("Lens[Person, String]", CartesianTests[Lens[Person, ?]].cartesian[String, String, String])
  checkAll("Cartesian[Lens[Person, ?]]", SerializableTests.serializable(Cartesian[Lens[Person, ?]]))
}

class GetterTests extends FunSuite with Discipline with MonocleTests {

  import cats.laws.discipline.eq._

  implicit def getterEq[S, A](implicit S: Arbitrary[S], E: Eq[A]): Eq[Getter[S, A]] =
    Eq.by[Getter[S, A], S => A](_.get)

  implicit val getterArbitrary: Arbitrary[Getter[Person, String]] = Arbitrary {
    val boolean = implicitly[Arbitrary[Boolean]].arbitrary
    boolean map (b => if(b) _firstName.asGetter else _secondName.asGetter)
  }

  implicit val getterFArbitrary: Arbitrary[Getter[Person, String => String]] = Arbitrary {
    val fa = implicitly[Arbitrary[String => String]].arbitrary
    fa.map(f => Getter[Person, String => String](_ => f))
  }

  checkAll("Getter[Person, String]", ApplyTests[Getter[Person, ?]].apply[String, String, String])
  checkAll("Apply[Getter[Person, ?]]", SerializableTests.serializable(Apply[Getter[Person, ?]]))
}
