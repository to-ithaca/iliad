package iliad

import iliad.syntax.all._

import org.scalacheck._

import shapeless._
import shapeless.ops.nat._

import spire.{algebra => spa, math => spm}
import spire.implicits._

trait ArbitraryInstances {

  implicit def vectorDArbitrary[N <: Nat, A](implicit arbA: Arbitrary[A], toInt: ToInt[N]): Arbitrary[VectorD[N, A]] = Arbitrary {
    VectorDGen(arbA.arbitrary)
  }

  implicit def rectArbitrary[A : spa.Signed](implicit arbA: Arbitrary[A]): Arbitrary[Rect[A]] = Arbitrary {
    RectGen(arbA.arbitrary)
  }

  implicit def matrixDArbitrary[W <: Nat, H <: Nat, A](implicit arbA: Arbitrary[A], toIntW: ToInt[W], toIntH: ToInt[H]): Arbitrary[MatrixD[W, H, A]] = Arbitrary {
    MatrixDGen(arbA.arbitrary)
  }

  implicit def planeArbitrary[A: spm.Fractional: spa.Trig](implicit arbA: Arbitrary[A]): Arbitrary[Plane[A]] = Arbitrary {
    PlaneGen(arbA.arbitrary)
  }

  implicit def boundedPlaneArbitrary[A: spm.Fractional : spa.Eq : spa.Trig](implicit arbA: Arbitrary[A]): Arbitrary[BoundedPlane[A]] = Arbitrary {
    BoundedPlaneGen(arbA.arbitrary)
  }

  implicit def lineArbitrary[A: spm.Fractional : spa.Eq](implicit arbA: Arbitrary[A]): Arbitrary[Line[A]] = Arbitrary {
    LineGen(arbA.arbitrary)
  }

  implicit def boundedLineArbitrary[A: spm.Fractional : spa.Eq](implicit arbA: Arbitrary[A]): Arbitrary[BoundedLine[A]] = Arbitrary {
    BoundedLineGen(arbA.arbitrary)
  }

  implicit def line2Arbitrary[A: spm.Fractional: spa.Eq](implicit arbA: Arbitrary[A]): Arbitrary[Line2[A]] = Arbitrary {
    Line2Gen(arbA.arbitrary)
  }

  implicit def boundedLine2Arbitrary[A: spm.Fractional: spa.Eq](implicit arbA: Arbitrary[A]): Arbitrary[BoundedLine2[A]] = Arbitrary {
    BoundedLine2Gen(arbA.arbitrary)
  }

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
