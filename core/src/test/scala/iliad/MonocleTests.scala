package iliad

import monocle._
import monocle.macros._
import monocle.syntax.all._

import org.typelevel.discipline.scalatest._
import org.scalatest._
import org.scalatest.prop._

import cats._
import cats.data._
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.kernel.laws._
import cats.implicits._

import org.scalacheck._

case class Person(name: Name)
case class Name(first: String, second: String)

object Person {
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

class LensTests extends FunSuite with Discipline {

  import cats.laws.discipline.eq._

  implicit def lensEq[S, A](implicit S: Arbitrary[S], E: Eq[A]): Eq[Lens[S, A]] =
    Eq.by[Lens[S, A], S => A](_.get)

  implicit val iso = CartesianTests.Isomorphisms.invariant[Lens[Person, ?]]

  implicit val lensArbitrary: Arbitrary[Lens[Person, String]] = Arbitrary {
    val boolean = Arbitrary.arbitrary[Boolean]
    boolean map (b => if(b) Person._firstName else Person._secondName)
  }

  checkAll("Lens[Person, String]", CartesianTests[Lens[Person, ?]].cartesian[String, String, String])
  checkAll("Cartesian[Lens[Person, ?]]", SerializableTests.serializable(Cartesian[Lens[Person, ?]]))
}

class GetterTests extends FunSuite with Discipline
    with GeneratorDrivenPropertyChecks with Matchers {

  import cats.laws.discipline.eq._

  implicit def getterEq[S, A](implicit S: Arbitrary[S], E: Eq[A]): Eq[Getter[S, A]] =
    Eq.by[Getter[S, A], S => A](_.get)

  implicit val getterArbitrary: Arbitrary[Getter[Person, String]] = Arbitrary {
    val boolean = Arbitrary.arbitrary[Boolean]
    boolean map (b => if(b) Person._firstName.asGetter else Person._secondName.asGetter)
  }

  implicit val getterFArbitrary: Arbitrary[Getter[Person, String => String]] = Arbitrary {
    val fa = implicitly[Arbitrary[String => String]].arbitrary
    fa.map(f => Getter[Person, String => String](_ => f))
  }

  checkAll("Getter[Person, String]", MonadTests[Getter[Person, ?]].monad[String, String, String])
  checkAll("Monad[Getter[Person, ?]]", SerializableTests.serializable(Monad[Getter[Person, ?]]))

  test("GetterOps#view creates a MonadReader") {
    //A MonadReader exists
    MonadReader[Reader[Person, ?], Person]

    val getter = Person._firstName.asGetter
    val view = getter.view[Reader[Person, ?]]
    val reader = MonadReader[Reader[Person, ?], Person].reader(getter.get _)

    forAll { (p: Person) =>
      view.run(p) should  === (reader.run(p))
    }
  }

  test("GetterOps#inspect creates a MonadState") {
    //A MonadState exists
    MonadState[State[Person, ?], Person]

    val getter = Person._firstName.asGetter
    val inspect = getter.inspect[State[Person, ?]]
    val state = MonadState[State[Person, ?], Person].inspect(getter.get _)

    forAll { (p: Person) =>
      inspect.run(p).value should  === (state.run(p).value)
    }
  }
}


class SetterTests extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {
  
  //A MonadState exists
  val S = MonadState[State[Person, ?], Person]
    
  test("SetterOps#assign creates a MonadState#modify") {
    val setter = Person._firstName.asSetter

    forAll { (name: String, p: Person) =>
      val assign = setter.assign[State[Person, ?]](name)
      val state = S.modify(setter.set(name))
      assign.run(p).value should === (state.run(p).value)
    }
  }
  
  test("SetterOps#modifying creates a MonadState#modify") {
    val setter = Person._firstName.asSetter
    forAll { (f: String => String, p: Person) =>
      val modifying = setter.modifying[State[Person, ?]](f)
      val state = S.modify(setter.modify(f))
      modifying.run(p).value should === (state.run(p).value)
    }
  }
}

class OptionalTests extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  sealed trait Animal
  case class Cat(name: String) extends Animal
  case class Dog(name: String) extends Animal

  implicit val catArbitrary: Arbitrary[Cat] = Arbitrary { Arbitrary.arbitrary[String].map(Cat(_)) }
  val dogArbitrary: Arbitrary[Dog] = Arbitrary { Arbitrary.arbitrary[String].map(Dog(_)) }

  implicit val animalArbitrary: Arbitrary[Animal] = Arbitrary {
    Arbitrary.arbitrary[Boolean].flatMap { b => 
      if(b) catArbitrary.arbitrary else dogArbitrary.arbitrary
    }
  }

  test("Optional#narrow#getOption should exist if the subtype exists") {
    val narrow = Optional.narrow[Animal, Cat]
    
    forAll { (animal : Animal) =>
      animal match {
        case _ : Dog => 
          narrow.getOption(animal) should === (None)
        case c : Cat => 
          narrow.getOption(animal) should === (Some(c))
      }
    }
  }

  test("Optional#narrow#set should always set a value") {
    val narrow = Optional.narrow[Animal, Cat]
    
    forAll { (cat : Cat, animal: Animal) =>
      narrow.set(cat)(animal) should === (cat)
    }
  }
}
