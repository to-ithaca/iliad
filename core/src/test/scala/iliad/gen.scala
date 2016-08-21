package iliad

import iliad.gfx._
import iliad.syntax.vectord._

import spire.{algebra => spa, math => spm}
import spire.implicits._

import shapeless._
import shapeless.ops.nat.ToInt

import org.scalacheck._

object VectorDGen {
  def apply[N <: Nat, A](genA: Gen[A])(implicit toInt: ToInt[N]): Gen[VectorD[N, A]] = 
    Gen.listOfN(toInt(), genA).map(as => VectorD.sized[N, A](as.toVector))

  def normal[N <: Nat, A: spm.Fractional](genA: Gen[A])(implicit toInt: ToInt[N]): Gen[VectorD[N, A]] =
    apply[N, A](genA).map(_.normalize).filter { v => 
      val n = v.norm
      n > spa.Field[A].zero && n < spa.Field[A].fromInt(2)
    }
}

object RectGen {

  def apply[A : spa.Signed](genA: Gen[A]): Gen[Rect[A]] = for {
    a <- genA
    b <- genA
    c <- genA
    d <- genA
  } yield Rect(v"$a $b", v"${c.abs} ${d.abs}")

  def internal(r: Rect[Int]): Gen[Vec2i] = for {
    x <- Gen.choose(r.bottomLeft.x, r.topLeft.x)
    y <- Gen.choose(r.bottomLeft.y, r.topLeft.y)
  } yield v"$x $y"

  def external(r: Rect[Int]): Gen[Vec2i] = {
    val boolean = implicitly[Arbitrary[Boolean]].arbitrary
    for {
      xUp <- boolean
      yUp <- boolean
      bothOut <- boolean
      x <- if(xUp) Gen.choose(r.topLeft.x, Integer.MAX_VALUE)
      else Gen.choose(Integer.MIN_VALUE, r.bottomLeft.x)
      y <- if(bothOut && yUp) Gen.choose(r.topLeft.y, Integer.MAX_VALUE)
      else if(bothOut) Gen.choose(Integer.MIN_VALUE, r.bottomLeft.y)
      else Gen.choose(r.bottomLeft.y, r.topLeft.y)
    } yield v"$x $y"
  }
}

object MatrixDGen {

  def apply[W <: Nat, H <: Nat, A](genA: Gen[A])(implicit toIntW: ToInt[W], toIntH : ToInt[H]): Gen[MatrixD[W, H, A]] =
    Gen.listOfN(toIntW() * toIntH(), genA).map(as => MatrixD.sized[W, H, A](as.toVector))
}

object PlaneGen {
  def apply[A: spm.Fractional: spa.Trig](genA: Gen[A]): Gen[Plane[A]] = for {
    p <- VectorDGen[nat._3, A](genA)
    n <- VectorDGen.normal[nat._3, A](genA)
  } yield Plane(p, n)
}

object BoundedPlaneGen {
  def apply[A: spm.Fractional : spa.Eq : spa.Trig](genA: Gen[A]): Gen[BoundedPlane[A]] = for {
    x0y0 <- VectorDGen[nat._3, A](genA)
    x0y1 <- VectorDGen[nat._3, A](genA).filter(_ =!= x0y0)
    x1y0 <- VectorDGen[nat._3, A](genA).filter(v => v =!= x0y0 && v =!= x0y1)
  } yield BoundedPlane(x0y0, x0y1, x1y0)
}

object LineGen {

  def apply[A: spm.Fractional : spa.Eq](genA: Gen[A]): Gen[Line[A]] = for {
    p <- VectorDGen[nat._3, A](genA)
    n <- VectorDGen.normal[nat._3, A](genA)
  } yield Line(p, n)

}

object BoundedLineGen {

  def apply[A: spm.Fractional : spa.Eq](genA: Gen[A]): Gen[BoundedLine[A]] = for {
    s <- VectorDGen[nat._3, A](genA)
    e <- VectorDGen[nat._3, A](genA).filter(_ =!= s)
  } yield BoundedLine(s, e)

}

object Line2Gen {

  def apply[A: spm.Fractional : spa.Eq](genA: Gen[A]): Gen[Line2[A]] = for {
    p <- VectorDGen[nat._2, A](genA)
    n <- VectorDGen.normal[nat._2, A](genA)
  } yield Line2(p, n)

}

object BoundedLine2Gen {

  def apply[A: spm.Fractional : spa.Eq](genA: Gen[A]): Gen[BoundedLine2[A]] = for {
    s <- VectorDGen[nat._2, A](genA)
    e <- VectorDGen[nat._2, A](genA).filter(_ =!= s)
  } yield BoundedLine2(s, e)

}

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
