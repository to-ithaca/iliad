package iliad

import iliad.gfx.{Panel, Carpenter, CuboidSurface}

import iliad.syntax.all._
import iliad.kernel.InputEvent

import org.scalacheck._

import shapeless._
import shapeless.ops.nat._

import spire._
import spire.{algebra => spa}
import spire.implicits._

import cats.data._

trait ArbitraryInstances {

  def boundedArbitrary[A: Gen.Choose](l: A, u: A): Arbitrary[A] = 
    Arbitrary { Gen.choose(l, u) }

  //TODO: add traverse instance for Gen
   private def traverse[A](gens: List[Gen[A]]): Gen[List[A]] = 
    gens.foldLeft(Gen.const(List.empty[A])) { (b, gen) => for {
      as <- b
      a <- gen
    } yield a :: as
    }

  implicit def vectorDArbitrary[N <: Nat, A](implicit arb: Arbitrary[A], toInt: ToInt[N]): Arbitrary[VectorD[N, A]] = Arbitrary {
    traverse((0 until toInt()).map(_ => arb.arbitrary).toList)
    .map { els =>
      VectorD.sized[N, A](els.toVector)
    }    
  }

  def normalArbitrary[N <: Nat, A : spa.NRoot : spa.PartialOrder](implicit arb: Arbitrary[A], F: spa.Field[A], toInt: ToInt[N]): Arbitrary[VectorD[N, A]] = 
    Arbitrary {
    vectorDArbitrary[N, A].arbitrary
      .map(_.normalize)
    .filter { v => 
      val n = v.norm
      n > F.zero && n < F.fromInt(2)
    }
  }

  implicit def matrixDArbitrary[W <: Nat, H <: Nat, A](implicit arb: Arbitrary[A], toIntW: ToInt[W], toIntH : ToInt[H]): Arbitrary[MatrixD[W, H, A]] = Arbitrary {
    traverse((0 until toIntW() * toIntH()).map(_ => arb.arbitrary).toList)
    .map(els => MatrixD.sized[W, H, A](els.toVector))
  }

  implicit def rectArbitrary[A : spa.Signed](implicit arb: Arbitrary[A]): Arbitrary[Rect[A]] = Arbitrary {
    for {
      a <- arb.arbitrary
      b <- arb.arbitrary
      c <- arb.arbitrary
      d <- arb.arbitrary
    } yield Rect(v"$a $b", c.abs, d.abs)
  }

  implicit def lineArbitrary[A: spa.Field : spa.NRoot : spa.PartialOrder : spa.Eq](implicit arb: Arbitrary[A]): Arbitrary[Line[A]] = Arbitrary {
    for {
      p <- vectorDArbitrary[nat._3, A].arbitrary
      n <- normalArbitrary[nat._3, A].arbitrary
    } yield Line(p, n.normalize)
  }

  implicit def boundedLineArbitrary[A: spa.Field : spa.NRoot : spa.Eq : spa.PartialOrder](implicit arb: Arbitrary[A]): Arbitrary[BoundedLine[A]] = Arbitrary {
    for {
      s <- vectorDArbitrary[nat._3, A].arbitrary
      e <- vectorDArbitrary[nat._3, A].arbitrary
      .filter(_ =!= s)
    } yield BoundedLine(s, e)
  }

  implicit def planeArbitrary[A : spire.math.Fractional : spa.Trig](implicit arb: Arbitrary[A]): Arbitrary[Plane[A]] =
    Arbitrary {
    for {
      p <- vectorDArbitrary[nat._3, A].arbitrary
      n <- normalArbitrary[nat._3, A].arbitrary
    } yield Plane(p, n)
    }

  implicit def boundedPlaneArbitrary[A : spire.math.Fractional : spa.Eq : spa.Trig](implicit arb: Arbitrary[A]): Arbitrary[BoundedPlane[A]] = Arbitrary {
    for {
      x0y0 <- vectorDArbitrary[nat._3, A].arbitrary
      x0y1 <- vectorDArbitrary[nat._3, A].arbitrary.filter(_ =!= x0y0)
      x1y0 <- vectorDArbitrary[nat._3, A].arbitrary.filter(v => v =!= x0y0 &&  v =!= x0y1)
    } yield BoundedPlane(x0y0, x0y1, x1y0)
  }

  implicit def line2Arbitrary[A: spire.math.Fractional : spa.PartialOrder : spa.Eq](implicit arb: Arbitrary[A]): Arbitrary[Line2[A]] = Arbitrary {
    for {
      p <- vectorDArbitrary[nat._2, A].arbitrary
      n <- normalArbitrary[nat._2, A].arbitrary
    } yield Line2(p, n.normalize)
  }

  implicit def boundedLine2Arbitrary[A: spa.Field : spa.NRoot : spa.Eq : spa.PartialOrder](implicit arb: Arbitrary[A]): Arbitrary[BoundedLine2[A]] = Arbitrary {
    for {
      s <- vectorDArbitrary[nat._2, A].arbitrary
      e <- vectorDArbitrary[nat._2, A].arbitrary
      .filter(_ =!= s)
    } yield BoundedLine2(s, e)
  }



  implicit def pointArbitrary: Arbitrary[InputEvent.Point] = Arbitrary {
    for {
      x <- Gen.choose(0f, 1f)
      y <- Gen.choose(0f, 1f)
      t <- Gen.choose(0L, 30000L)
    } yield InputEvent.Point(t, x, y)
  }

  def tapArbitrary: Arbitrary[InputEvent.Tap] = Arbitrary {
    pointArbitrary.arbitrary.map(InputEvent.Tap)
  }

  implicit def swipeArbitrary: Arbitrary[InputEvent.DragBecameSwipe] = Arbitrary {
    for {
      s <- pointArbitrary.arbitrary
      n <- Gen.choose(1, 10)
      t <- traverse((0 to n).toList.map(_ => pointArbitrary.arbitrary))
      e <- pointArbitrary.arbitrary.filter { e => 
        (e.x != s.x || e.y != s.y) && e.at > s.at
      }
    } yield InputEvent.DragBecameSwipe(s, t :+ e)
  }

  def cuboidPanelArbitrary[A : spire.math.Fractional](
    a: Arbitrary[A], i: Arbitrary[Int]):
      Arbitrary[Panel[(Vec3[A], CuboidSurface)]] = Arbitrary {
    for {
      x <- a.arbitrary
      xSeg <- i.arbitrary
      y <- a.arbitrary
      ySeg <- i.arbitrary
      z <- a.arbitrary
      zSeg <- i.arbitrary
    } yield Carpenter.cuboid.shape(x, xSeg, y, ySeg, z, zSeg)
  }
}

object arbitrary extends ArbitraryInstances
