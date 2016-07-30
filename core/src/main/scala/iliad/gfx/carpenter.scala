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
  def plane[A : spm.Fractional]: PlaneCarpenter[A] = new PlaneCarpenter[A]
}

class Carpenter[A : spm.Fractional] {
  import Carpenter._

  private def panelVertices(dx: A, 
    xSegments: Int, dy: A, ySegments: Int): List[Vertex[A]] = {
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

  private val offsets = List(
    0 -> 0, 1 -> 0, 0 -> 1,
    0 -> 1, 1 -> 0, 1 -> 1)

  private def panelElement(vs: List[Vertex[A]], offset: (Int, Int), 
    xSegment: Int, ySegment: Int): MissingElementError Xor Int = offset match {
    case (xo, yo) => vs.find( v => 
      v.xSeg == (xSegment + xo) && v.ySeg == (ySegment + yo) 
    ).map(_.index).toRightXor(MissingElementError(xSegment + xo, ySegment + yo))
  }

  private def panelElements(vs: List[Vertex[A]], 
    xSegments: Int, ySegments: Int): MissingElementError Xor List[Int] = (for {
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
  def panel(dx: A, xSegments: Int, dy: A, ySegments: Int): MissingElementError Xor Panel[Vec2[A]] = {
    val vs = panelVertices(dx, xSegments, dy, ySegments)
    val esx = panelElements(vs, xSegments, ySegments)
    esx.map { es => 
      Panel(vs.map(_.v), es)
    }
  }

  /** Joins multiple panels into a single vertex-element group by shifting the elements of subsequent panels */
  def join[A](tiles: Seq[Panel[A]]): Panel[A] = tiles.foldLeft(Panel(List.empty[A], List.empty[Int]))((p, t) =>
      p.copy(vertices = p.vertices ++ t.vertices, elements = p.elements ++ t.elements.map(_ + p.vertices.size)))
}

case class Panel[A](vertices: List[A], elements: List[Int]) {

  def map[B](f: A => B): Panel[B] = Panel(vertices.map(f), elements)

  /*def shape(implicit sizeOf: iliad.io.ByteSizeOf[A], put: iliad.io.BufferPut[A, ByteBuffer]): TriangleData = {
    val vData = VertexData(IOBuffer(vertices: _*), vertices.size)
    val eData = ElementData(IOBuffer[Int, IntBuffer](elements: _*), elements.size)
    TriangleData(vData, eData)
  }*/
}

object Panel {
  implicit def panelFunctor: Functor[Panel] = new PanelFunctor {}
}

private [gfx] trait PanelFunctor extends Functor[Panel] {
  def map[A, B](fa: Panel[A])(f: A => B): Panel[B] = fa.map(f)
}

sealed trait CarpenterError extends IliadError
case class MissingElementError(xSeg: Int, ySeg: Int) extends CarpenterError {
  def message: String = s"Unable to find vertex with xSegment: $xSeg ySegment $ySeg"
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

  private def fixX(s: CuboidSurface, x: A, b: Blueprint): 
      MissingElementError Xor Panel[(Vec3[A], CuboidSurface)] =
    carpenter.panel(b.dy, b.ySegments, b.dz, b.zSegments).map(_.map { v =>
      val y = v(0)
      val z = v(1)
      (v"$x ${y - b.yOffset} ${z - b.zOffset}", s)
    })

  private def fixY(s: CuboidSurface, y: A, b: Blueprint): 
     MissingElementError Xor Panel[(Vec3[A], CuboidSurface)] =
    carpenter.panel(b.dx, b.xSegments, b.dz, b.zSegments).map(_.map { v =>
      val x = v(0)
      val z = v(1)
      (v"${x - b.xOffset}  $y ${z - b.zOffset}", s)
    })

  private def fixZ(s: CuboidSurface, z: A, b: Blueprint): 
     MissingElementError Xor Panel[(Vec3[A], CuboidSurface)] =
    carpenter.panel(b.dx, b.xSegments, b.dy, b.ySegments).map(_.map( v =>
      (v"${v.x - b.xOffset} ${v.y - b.yOffset} $z", s)
    ))

  def shape(x: A, xSegments: Int, y: A, ySegments: Int, z: A, zSegments: Int): 
      Panel[(Vec3[A], CuboidSurface)] = {
    val b = Blueprint(x, xSegments, y, ySegments, z, zSegments)
    val px = for {
     top <- fixZ(Top, b.zOffset, b)
      bottom <- fixZ(Bottom, - b.zOffset, b)
      front <- fixX(Front, b.xOffset, b)
      back <- fixX(Back, - b.xOffset, b)
      right <- fixY(Right, b.yOffset, b)
      left <- fixY(Left, - b.yOffset, b)
    } yield carpenter.join(List(top, bottom, front, back, right, left))
    px match {
      case Xor.Right(p) => p
      case Xor.Left(err) => 
        throw new Error(s"Cuboid factory is incorrect!  $err")
    }
  }
}

class PlaneCarpenter[A : spm.Fractional] {

  private val carpenter = new Carpenter[A]

  private case class Blueprint(x: A, 
    xSegments: Int, 
    y: A, 
    ySegments: Int) {
    val dx: A = x / spa.Field[A].fromInt(xSegments)
    val xOffset: A = x / spa.Field[A].fromInt(2)
    val dy: A = y / spa.Field[A].fromInt(ySegments)
    val yOffset: A = y / spa.Field[A].fromInt(2)
  }

  def shape(x: A, xSegments: Int, y: A, ySegments: Int): Panel[Vec2[A]] = {
    val b: Blueprint = Blueprint(x, xSegments, y, ySegments)
    val px = carpenter.panel(b.dx, b.xSegments, b.dy, b.ySegments).map(_.map(
      _ - v"${b.xOffset} ${b.yOffset}"
    ))
    px match {
      case Xor.Right(p) => p
      case Xor.Left(err) =>
        throw new Error(s"Plane carpenter is incorrect!  $err")
    }
  }
}
