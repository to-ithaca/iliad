package iliad
package algebra

import shapeless._
import shapeless.ops.nat._

import org.scalacheck._

import spire._
import spire.implicits._
import spire.algebra._
import spire.math._

import iliad.algebra.syntax.matrix._
import iliad.algebra.syntax.vector._

import scala.{Vector => SVector}

object MatrixGen {
  def gen[N <: Nat, M <: Nat, A](genA: Gen[A])(implicit toIntN: ToInt[N], toIntM: ToInt[M]): Gen[Matrix[N, M, A]] =
    Gen.listOfN(toIntN() * toIntM(), genA).map(as => Matrix.sized[A, N, M](as.toVector))

  def symmetric[N <: Nat, A](genA: Gen[A])(implicit toIntN: ToInt[N]): Gen[Matrix[N, N, A]] = {
    val N = toIntN()
    for {
      diag <- Gen.listOfN(N, genA)
      els <- Gen.listOfN(N * (N - 1), genA)
    } yield {
      val builder = SVector.newBuilder[A]
      (0 until N).foreach { i =>
        (0 until N).foreach { j =>
          if(i == j)
            builder += diag(i)
          else {
            val n = (i + j) % els.size
            builder += els(n)
          }
        }
      }
      val m = Matrix.sized[A, N, N](builder.result())
      builder.clear()
      m
    }
  }

  def rotation[N <: Nat, A](genA: Gen[A])(implicit toIntN: ToInt[N], Trig: Trig[A], G: AdditiveGroup[A]): Gen[Matrix[N, N, A]] = {
    toIntN() match {
      case 2 => genA.map { theta =>
        mat"""${Trig.cos(theta)} ${-Trig.sin(theta)}
              ${Trig.sin(theta)} ${Trig.cos(theta)}""".asInstanceOf[Matrix[N, N, A]]
      }
      case _ => sys.error("unimplemented")
    }

  }
}

object OrthoMatrixGen {
  def gen[N <: Nat, M <: Nat, A](genA: Gen[A])(implicit toIntN: ToInt[N], toIntM: ToInt[M]): Gen[OrthoMatrix[N, M , A]] = MatrixGen.gen[N, M, A](genA).map(OrthoMatrix(_))
}

object VectorGen {
  def gen[N <: Nat, A](genA: Gen[A])(implicit toInt: ToInt[N]): Gen[Vector[N, A]] =
    Gen.listOfN(toInt(), genA).map(as => Vector.sized[N, A](as.toVector))

  def normalGen[N <: Nat, A](genA: Gen[A])(implicit toInt: ToInt[N], F: Fractional[A],
    N: NormedVectorSpace[Vector[N, A], A]): Gen[Vector[N, A]] =
    gen[N, A](genA).map(_.normalize).filter { v => 
      val n = v.norm
      n > F.zero && n < F.fromInt(2)
    }
}

object Line2Gen {
  def gen[A](genA: Gen[A])(implicit F: Fractional[A], N: NormedVectorSpace[Vec2[A], A]): Gen[Line2[A]] = for {
    p <- VectorGen.gen[nat._2, A](genA)
    n <- VectorGen.normalGen[nat._2, A](genA)
  } yield Line2(p, n)
}

object LineSegment3Gen {

  def gen[A : Eq](genA: Gen[A]): Gen[LineSegment3[A]] = for {
    s <- VectorGen.gen[nat._3, A](genA)
    e <- VectorGen.gen[nat._3, A](genA).filter(_ =!= s)
  } yield LineSegment3(s, e)

}

object RectGen {

  def gen[A : Signed](genA: Gen[A]): Gen[Rect[A]] = for {
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
    val boolean = Arbitrary.arbitrary[Boolean]
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
