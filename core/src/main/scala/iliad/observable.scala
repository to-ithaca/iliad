package iliad

import shapeless._
import shapeless.ops.hlist._

case class Key[A](name: String, description: String) {
  def scoped(scope: Scope): KeyProduct[A :: HNil, (Key[A], Scope) :: HNil] = new KeyProduct((this -> scope) :: HNil)
}

trait Scope

object Scope {
  case object Global extends Scope
}

//trait ScopedKey[A] {
//  def gen[B](generator: Transform[A, B]): Generator[B] = ???
//}



final class KeyProduct[L <: HList, Dep <: HList] private[iliad] (val dependencies: Dep) {
  def product[L1 <: HList, Dep1 <: HList](that: KeyProduct[L1, Dep1])(implicit prependOut: Prepend[L, L1], prependDep: Prepend[Dep, Dep1]): KeyProduct[prependOut.Out, prependDep.Out] =
    new KeyProduct(dependencies ++ that.dependencies)
  //def gen[B](transform: Transform[L, B])
}


//Node[(Vec3f, Float)](deps: position :: speed :: HNil)
//Should not have apply builder syntax -> perhaps a combine?
// val f = foo(a: Float, b: String): String = ???
// f.toTransform
trait Transform[A, B]

// peek : Boolean

//

object Generator {
  type TimeF[A, B] = (Long, A) => B
}

trait Generator[A, B] {
  import Generator._

  def value: TimeF[A, B]
  def peek(a: A): Option[TimeF[A, B]]
  def flatMapA[BB](a: A => Generator[BB, B]): Generator[BB, B] = ???

  //from (start to start + 30) use function1
  // events ... system time
  // if a > 5f
  // f
  // events...time ... => g

//  def publish(scope: ScopedKey[A]): Unit = ???
}

object Example {
  import shapeless._

  val position = Key[VectorD[nat._3, Float]]("position", "").scoped(Scope.Global)
  val speed = Key[Float]("speed", "").scoped(Scope.Global)

  position.product(speed)

  // val speed = initialPosition |@| speed
  // ScopedKey[A] <> ScopedKey[B] == ScopedKey[(A, B)]

//  val restaurantScope = new Scope {}
//  val sSpeed = ScopedKey.Leaf(speed, restaurantScope)
//  val speedFn: Transform[Float, String] = ???
//  val sGen: Generator[String] = sSpeed gen speedFn
//
//
//
//  val sPosition = ScopedKey.Leaf(position, restaurantScope)
//
//  val positionFn: Transform[(VectorD[nat._3, Float], Float), Int] = ???
//  (ScopedKey.Node[(VectorD[nat._3, Float], Float)](List.empty) gen positionFn) publish (position in newScope)


}