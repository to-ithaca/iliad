package iliad

import org.scalatest._
import org.scalacheck._
import Prop._

class EnumTests extends FunSuite {
  {
    //Int has an enum
    Enum[Int]
  }

  test("Enum sucessor") {
    forAll { (i: Int) => Enum[Int].succ(i) == i + 1 }
  }

  test("Enum predecessor") {
    forAll {(i: Int) => Enum[Int].pred(i) == i - 1 }
  }
}
