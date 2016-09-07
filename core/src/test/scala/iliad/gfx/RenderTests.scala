package iliad
package gfx

import org.scalatest._

import cats._
import cats.implicits._

class RenderTests extends FunSuite {

  implicit val renderInt: Render[Int] = new Render[Int] {
    def show(i: Int): List[GFX] = Nil
    def hide(i: Int): List[GFX] = Nil
  }

  Render[Int]
  Render[List[Int]]
}
