package iliad

import iliad.kernel.InputEvent
import iliad.syntax.all._

import org.scalatest._
import org.scalatest.prop._

import cats.implicits._

import arbitrary._

class EventTests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {

  test("a point position is given by x and y") {
    forAll(pointArbitrary.arbitrary) { p =>
      assert(p.position === v"${p.x} ${p.y}")
    }
  }

  test("the bottom left corner of the screen has window coords (-1, -1)") {
    val p = InputEvent.Point(0L, 0f, 0f)
    assert(p.windowCoord === v"-1f -1f")
  }

  test("the top right screen corner has window coords (1, 1)") {
    val p = InputEvent.Point(0L, 1f, 1f)
    assert(p.windowCoord === v"1f 1f")
  }

  test("the duration of a swipe is always positive") {
    forAll(swipeArbitrary.arbitrary) { s => 
      assert(s.duration > 0L)
    }
  }

  test("a vertical swipe up is a swipe up") {
    val start = InputEvent.Point(0L, 0.5f, 0f)
    val end = InputEvent.Point(10L, 0.5f, 1f)
    val s = InputEvent.DragBecameSwipe(start, end :: Nil)
    assert(s.isUp(0.9f))
  }

  test("a horizontal swipe right is a swipe right") {
    val start = InputEvent.Point(0L, 0f, 0.5f)
    val end = InputEvent.Point(10L, 1f, 0.5f)
    val s = InputEvent.DragBecameSwipe(start, end :: Nil)
    assert(s.isRight(0.9f))
  }
}
