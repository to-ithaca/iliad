package iliad

import iliad.algebra._
import iliad.algebra.syntax.vector._

import org.scalatest._
import org.scalatest.prop._

import cats.data._
import cats.implicits._

import spire.implicits._

import arbitrary._

class EventTests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {


  test("the bottom left corner of the screen has window coords (-1, -1)") {
    val p = InputEvent.Point(0L, v"0.0 0.0")
    assert(p.windowCoord === v"-1.0 -1.0")
  }

  test("the top right screen corner has window coords (1, 1)") {
    val p = InputEvent.Point(0L, v"1.0 1.0")
    assert(p.windowCoord === v"1.0 1.0")
  }

  test("the duration of a swipe is always positive") {
    forAll(swipeArbitrary.arbitrary) { s => 
      assert(s.duration > 0L)
    }
  }

  test("a vertical swipe up is a swipe up") {
    val start = InputEvent.Point(0L, v"0.5 0.0")
    val end = InputEvent.Point(10L, v"0.5 1.0")
    val s = InputEvent.DragBecameSwipe(List(end, start))
    assert(s.isUp(0.9))
  }

  test("a horizontal swipe right is a swipe right") {
    val start = InputEvent.Point(0L, v"0.0 0.5")
    val end = InputEvent.Point(10L, v"1.0 0.5")
    val s = InputEvent.DragBecameSwipe(List(end, start))
    assert(s.isRight(0.9))
  }

  test("a drag has a correct standard deviation") {
    val p0 = InputEvent.Point(0L, v"0.0 1.0")
    val p1 = InputEvent.Point(0L, v"0.0 -1.0")
    val p2 = InputEvent.Point(10L, v"1.0 0.0")
    val p3 = InputEvent.Point(10L, v"-1.0 0.0")
    val drag = InputEvent.DragContinued(p3 :: p2 :: p1 :: p0 :: Nil)
    drag.standardDeviation should ===(2.0)

  }
  test("a drag is a long press") {
    val p0 = InputEvent.Point(0L, v"0.0 1.0")
    val p1 = InputEvent.Point(5L, v"0.0 -1.0")
    val p2 = InputEvent.Point(10L, v"1.0 0.0")
    val p3 = InputEvent.Point(15L, v"-1.0 0.0")
    val drag = InputEvent.DragContinued(p3 :: p2 :: p1 :: p0 :: Nil)

    drag.isLongPress(10L, 3.0) shouldBe true
    drag.isLongPress(20L, 3.0) shouldBe false
    drag.isLongPress(10L, 1.0) shouldBe false
    drag.isLongPress(20L, 1.0) shouldBe false
  }


}
