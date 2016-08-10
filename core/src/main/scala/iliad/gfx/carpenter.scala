package iliad
package gfx

import iliad.syntax.vectord._

import cats.Functor
import cats.data.Xor
import cats.implicits._

import spire.{algebra => spa, math => spm}
import spire.implicits._

object Carpenter {
  case class Vertex[A](index: Int, xSeg: Int, ySeg: Int, v: Vec2[A])

  def cuboid[A: spm.Fractional]: CuboidCarpenter[A] = new CuboidCarpenter[A]
  def unitCube[A](
      implicit F: spm.Fractional[A]): Panel[(Vec3[A], CuboidSurface)] =
    cuboid.shape(F.one, 1, F.one, 1, F.one, 1)

  def plane[A: spm.Fractional]: PlaneCarpenter[A] = new PlaneCarpenter[A]
  def unitPlane[A](implicit F: spm.Fractional[A]): Panel[Vec2[A]] =
    plane.shape(F.one, 1, F.one, 1)

  def fromRect[A: spm.Fractional](r: Rect[A]): Panel[Vec2[A]] =
    plane.shape(r.width, 1, r.height, 1).map(_ + r.midpoint)

  def prism[A: spm.Fractional]: PrismCarpenter[A] = new PrismCarpenter[A]
  def unitPrism[A](
      implicit F: spm.Fractional[A]): Panel[(Vec3[A], PrismSurface)] =
    prism.shape(F.one, 1, 1, F.one, 1)
}

class Carpenter[A: spm.Fractional] {
  import Carpenter._

  private def panelVertices(dx: A,
                            xSegments: Int,
                            dy: A,
                            ySegments: Int): List[Vertex[A]] = {
    val vs = for {
      xSeg <- 0 to xSegments
      ySeg <- 0 to ySegments
    } yield {
      val x = spa.Field[A].fromInt(xSeg) * dx
      val y = spa.Field[A].fromInt(ySeg) * dy
      (xSeg, ySeg) -> v"$x $y"
    }
    vs.zipWithIndex.map {
      case (((xSeg, ySeg), v), i) => Vertex[A](i, xSeg, ySeg, v)
    }.toList
  }

  private val offsets = List(0 -> 0, 1 -> 0, 0 -> 1, 0 -> 1, 1 -> 0, 1 -> 1)

  private def panelElement(vs: List[Vertex[A]],
                           offset: (Int, Int),
                           xSegment: Int,
                           ySegment: Int): MissingElementError Xor Int =
    offset match {
      case (xo, yo) =>
        vs.find(v => v.xSeg == (xSegment + xo) && v.ySeg == (ySegment + yo))
          .map(_.index)
          .toRightXor(MissingElementError(xSegment + xo, ySegment + yo))
    }

  private def panelElements(
      vs: List[Vertex[A]],
      xSegments: Int,
      ySegments: Int): MissingElementError Xor List[Int] =
    (for {
      x <- 0 to (xSegments - 1)
      y <- 0 to (ySegments - 1)
      o <- offsets
    } yield panelElement(vs, o, x, y)).toList.sequence

  /** Generates a panel of uniformly spaced vertices and joining elements
    *
    * @param dx Width of a segment in the x direction
    * @param xSegments number of segments in the x direction
    * @param dy Width of a segment in the y direction
    * @param ySegments number of segments in the y direction
    */
  def panel(dx: A,
            xSegments: Int,
            dy: A,
            ySegments: Int): MissingElementError Xor Panel[Vec2[A]] = {
    val vs = panelVertices(dx, xSegments, dy, ySegments)
    val esx = panelElements(vs, xSegments, ySegments)
    esx.map { es =>
      Panel(vs.map(_.v), es)
    }
  }

  /** Joins multiple panels into a single vertex-element group by shifting the elements of subsequent panels */
  def join[A](tiles: Seq[Panel[A]]): Panel[A] =
    tiles.foldLeft(Panel(List.empty[A], List.empty[Int]))((p, t) =>
          p.copy(vertices = p.vertices ++ t.vertices,
                 elements = p.elements ++ t.elements.map(_ + p.vertices.size)))
}

case class Panel[A](vertices: List[A], elements: List[Int]) {
  def map[B](f: A => B): Panel[B] = Panel(vertices.map(f), elements)
}

object Panel {
  implicit def panelFunctor: Functor[Panel] = new PanelFunctor
}

private[gfx] final class PanelFunctor extends Functor[Panel] {
  def map[A, B](fa: Panel[A])(f: A => B): Panel[B] = fa.map(f)
}

sealed trait CarpenterError extends IliadError
case class MissingElementError(xSeg: Int, ySeg: Int) extends CarpenterError {
  def message: String =
    s"Unable to find vertex with xSegment: $xSeg ySegment $ySeg"
}

sealed trait CuboidSurface

object CuboidSurface {
  case object Top extends CuboidSurface
  case object Bottom extends CuboidSurface
  case object Front extends CuboidSurface
  case object Back extends CuboidSurface
  case object Left extends CuboidSurface
  case object Right extends CuboidSurface
}

class CuboidCarpenter[A: spm.Fractional] {
  import CuboidSurface._
  private case class Blueprint(x: A,
                               xSegments: Int,
                               y: A,
                               ySegments: Int,
                               z: A,
                               zSegments: Int) {
    val dx: A = x / spa.Field[A].fromInt(xSegments)
    val xOffset: A = x / spa.Field[A].fromInt(2)
    val dy: A = y / spa.Field[A].fromInt(ySegments)
    val yOffset: A = y / spa.Field[A].fromInt(2)
    val dz: A = z / spa.Field[A].fromInt(zSegments)
    val zOffset: A = z / spa.Field[A].fromInt(2)
  }

  private val carpenter: Carpenter[A] = new Carpenter

  private def fixX(
      s: CuboidSurface,
      x: A,
      b: Blueprint): MissingElementError Xor Panel[(Vec3[A], CuboidSurface)] =
    carpenter
      .panel(b.dy, b.ySegments, b.dz, b.zSegments)
      .map(_.map { v =>
        val y = v(0)
        val z = v(1)
        (v"$x ${y - b.yOffset} ${z - b.zOffset}", s)
      })

  private def fixY(
      s: CuboidSurface,
      y: A,
      b: Blueprint): MissingElementError Xor Panel[(Vec3[A], CuboidSurface)] =
    carpenter
      .panel(b.dx, b.xSegments, b.dz, b.zSegments)
      .map(_.map { v =>
        val x = v(0)
        val z = v(1)
        (v"${x - b.xOffset}  $y ${z - b.zOffset}", s)
      })

  private def fixZ(
      s: CuboidSurface,
      z: A,
      b: Blueprint): MissingElementError Xor Panel[(Vec3[A], CuboidSurface)] =
    carpenter
      .panel(b.dx, b.xSegments, b.dy, b.ySegments)
      .map(_.map(v => (v"${v.x - b.xOffset} ${v.y - b.yOffset} $z", s)))

  def shape(x: A,
            xSegments: Int,
            y: A,
            ySegments: Int,
            z: A,
            zSegments: Int): Panel[(Vec3[A], CuboidSurface)] = {
    val b = Blueprint(x, xSegments, y, ySegments, z, zSegments)
    val px = for {
      top <- fixZ(Top, b.zOffset, b)
      bottom <- fixZ(Bottom, -b.zOffset, b)
      front <- fixX(Front, b.xOffset, b)
      back <- fixX(Back, -b.xOffset, b)
      right <- fixY(Right, b.yOffset, b)
      left <- fixY(Left, -b.yOffset, b)
    } yield carpenter.join(List(top, bottom, front, back, right, left))
    px match {
      case Xor.Right(p) => p
      case Xor.Left(err) =>
        throw new Error(s"Cuboid factory is incorrect!  $err")
    }
  }
}

class PlaneCarpenter[A: spm.Fractional] {

  private val carpenter = new Carpenter[A]

  private case class Blueprint(x: A, xSegments: Int, y: A, ySegments: Int) {
    val dx: A = x / spa.Field[A].fromInt(xSegments)
    val xOffset: A = x / spa.Field[A].fromInt(2)
    val dy: A = y / spa.Field[A].fromInt(ySegments)
    val yOffset: A = y / spa.Field[A].fromInt(2)
  }

  def shape(x: A, xSegments: Int, y: A, ySegments: Int): Panel[Vec2[A]] = {
    val b: Blueprint = Blueprint(x, xSegments, y, ySegments)
    val px = carpenter
      .panel(b.dx, b.xSegments, b.dy, b.ySegments)
      .map(
          _.map(
              _ - v"${b.xOffset} ${b.yOffset}"
          ))
    px match {
      case Xor.Right(p) => p
      case Xor.Left(err) =>
        throw new Error(s"Plane carpenter is incorrect!  $err")
    }
  }
}

sealed trait PrismSurface
object PrismSurface {
  case object AlongX extends PrismSurface
  case object AlongY extends PrismSurface
  case object Hypotenuse extends PrismSurface
  case object Front extends PrismSurface
  case object Back extends PrismSurface
}

class PrismCarpenter[A: spm.Fractional] {
  import PrismSurface._
  private val carpenter = new Carpenter[A]

  private case class Blueprint(x: A,
                               xSegments: Int,
                               lSegments: Int,
                               z: A,
                               zSegments: Int) {
    val l: A = x * spa.Field[A].fromDouble(Math.sqrt(2.0))
    val dx: A = x / spa.Field[A].fromInt(xSegments)
    val dl: A = l / spa.Field[A].fromInt(lSegments)
    val dz: A = z / spa.Field[A].fromInt(zSegments)
    val zOffset: A = z / spa.Field[A].fromInt(2)
  }

  private def fixX(
      b: Blueprint): MissingElementError Xor Panel[(Vec3[A], PrismSurface)] =
    carpenter
      .panel(b.dx, b.xSegments, b.dz, b.zSegments)
      .map(_.map { v =>
        val y = v(0)
        val z = v(1)
        (v"${spa.Field[A].zero} $y ${z - b.zOffset}", AlongY)
      })

  private def fixY(
      b: Blueprint): MissingElementError Xor Panel[(Vec3[A], PrismSurface)] =
    carpenter
      .panel(b.dx, b.xSegments, b.dz, b.zSegments)
      .map(_.map { v =>
        val x = v(0)
        val z = v(1)
        (v"$x ${spa.Field[A].zero} ${z - b.zOffset}", AlongX)
      })

  private def fixL(
      b: Blueprint): MissingElementError Xor Panel[(Vec3[A], PrismSurface)] = {
    val factor = spm.Fractional[A].fromDouble(Math.cos(Math.PI / 4.0))
    carpenter
      .panel(b.dl, b.lSegments, b.dz, b.zSegments)
      .map(_.map { v =>
        val l = v(0)
        val z = v(1)
        val y = l * factor
        val x = b.x - y
        (v"$x $y ${z - b.zOffset}", Hypotenuse)
      })
  }

  private def triangle(b: Blueprint, z: A, s: PrismSurface) =
    Panel[Vec3[A]](List(v"${spa.Field[A].zero} ${spa.Field[A].zero} $z",
                        v"${b.x}               ${spa.Field[A].zero} $z",
                        v"${spa.Field[A].zero} ${b.x}               $z"),
                   List(0, 1, 2)).map(_ -> s)

  def shape(x: A,
            xSegments: Int,
            lSegments: Int,
            z: A,
            zSegments: Int): Panel[(Vec3[A], PrismSurface)] = {
    val b: Blueprint = Blueprint(x, xSegments, lSegments, z, zSegments)

    val px = for {
      alongY <- fixX(b)
      alongX <- fixY(b)
      hypotenuse <- fixL(b)
      front <- triangle(b, b.zOffset, Front).right
      back <- triangle(b, -b.zOffset, Back).right
    } yield carpenter.join(List(alongY, alongX, hypotenuse, front, back))
    px match {
      case Xor.Right(p) => p
      case Xor.Left(err) =>
        throw new Error(s"Prism carpenter is incorrect!  $err")
    }
  }
}
