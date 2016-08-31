package iliad

import org.scalacheck._

import shapeless._
import shapeless.ops.nat._

import spire.{algebra => spa, math => spm}
import spire.implicits._

trait ArbitraryInstances {
 implicit def pointArbitrary: Arbitrary[InputEvent.Point] = Arbitrary {
   PointGen()
  }

  def tapArbitrary: Arbitrary[InputEvent.Tap] = Arbitrary {
    TapGen()
  }

  implicit def swipeArbitrary: Arbitrary[InputEvent.DragBecameSwipe] = Arbitrary {
    SwipeGen()
  }
}

object arbitrary extends ArbitraryInstances
