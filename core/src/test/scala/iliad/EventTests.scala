package iliad

import iliad.kernel.InputEvent
import iliad.syntax.all._

import org.scalatest._
import org.scalatest.prop._

import cats.implicits._

import arbitrary._

class EventTests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {

  test("a tap position is given by x and y") {
    forAll(tapArbitrary.arbitrary) { t =>
      assert(t.position === v"${t.x} ${t.y}")
    }
  }

  test("the bottom left corner of the screen has window coords (-1, -1)") {
    val t = InputEvent.Tap(0L, 0f, 0f)
    assert(t.windowCoord === v"-1f -1f")
  }

  test("the top right screen corner has window coords (1, 1)") {
    val t = InputEvent.Tap(0L, 1f, 1f)
    assert(t.windowCoord === v"1f 1f")
  }

  test("the duration of a swipe is always positive") {
    forAll(swipeArbitrary.arbitrary) { s => 
      assert(s.duration > 0L)
    }
  }

  test("a vertical swipe up is a swipe up") {
    val start = InputEvent.Tap(0L, 0.5f, 0f)
    val end = InputEvent.Tap(10L, 0.5f, 1f)
    val s = InputEvent.Swipe(start, end)
    assert(s.isUp(0.9f))
  }

  test("a horizontal swipe right is a swipe right") {
    val start = InputEvent.Tap(0L, 0f, 0.5f)
    val end = InputEvent.Tap(10L, 1f, 0.5f)
    val s = InputEvent.Swipe(start, end)
    assert(s.isRight(0.9f))
  }
}
