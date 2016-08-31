package iliad

import iliad.algebra._
import iliad.gfx._

import spire.{algebra => spa, math => spm}
import spire.implicits._

import shapeless._
import shapeless.ops.nat.ToInt

import org.scalacheck._

object PointGen {
  def apply(): Gen[InputEvent.Point] = for {
      x <- Gen.choose(0f, 1f)
      y <- Gen.choose(0f, 1f)
      t <- Gen.choose(0L, 30000L)
  } yield InputEvent.Point(t, x, y)

}

object TapGen {
  def apply(): Gen[InputEvent.Tap] = PointGen().map(InputEvent.Tap)
}

object SwipeGen {
  def apply(): Gen[InputEvent.DragBecameSwipe] = for {
    s <- PointGen()
    n <- Gen.choose(1, 10)
    t <- Gen.listOfN(n, PointGen())
    e <- PointGen().filter { e =>
      e.position =!= s.position  && e.at > s.at
    }
  } yield InputEvent.DragBecameSwipe(e :: (t :+ s))
}

object CuboidGen {

 def apply[A : spm.Fractional](genA: Gen[A], genI: Gen[Int]):
     Gen[Panel[(Vec3[A], CuboidSurface)]] =
    for {
      x <- genA
      xSeg <- genI
      y <- genA
      ySeg <- genI
      z <- genA
      zSeg <- genI
    } yield Carpenter.cuboid.shape(x, xSeg, y, ySeg, z, zSeg)
}
