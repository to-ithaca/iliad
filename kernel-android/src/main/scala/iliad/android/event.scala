package iliad.android

import iliad.kernel._
import iliad.kernel.utils.vectord._

import android.view.{GestureDetector, MotionEvent}

import EventHandler._
import InputEvent._

trait AndroidEventHandler extends GestureDetector.OnGestureListener
    with GestureDetector.OnDoubleTapListener
    with EventHandler {

  def screenSize: Vec2f

  private var tapCallback: Callback[Tap] = EventHandler.zero

  def registerTapCallback(cb: Callback[Tap]): Unit = {
    tapCallback = cb
  }

  override def onDown(event: MotionEvent): Boolean =  {
    println("onDown: " + event.toString());
    return true;
  }

  override def onFling(event1: MotionEvent, event2: MotionEvent, velocityX: Float, velocityY: Float): Boolean = {
    println("onFling: " + event1.toString()+event2.toString());
    return true;
  }

  override def onLongPress(event: MotionEvent): Unit = {
    println("onLongPress: " + event.toString());
  }

  override def onScroll(e1: MotionEvent, e2: MotionEvent, distanceX: Float, distanceY: Float): Boolean = {
    println("onScroll: " + e1.toString()+e2.toString());
    return true;
  }

  override def onShowPress(event: MotionEvent): Unit = {
    println("onShowPress: " + event.toString());
  }

  override def onSingleTapUp(event: MotionEvent): Boolean = {
    println("onSingleTapUp: " + event.toString());
    return true;
  }

  override def onDoubleTap(event: MotionEvent): Boolean = {
    println("onDoubleTap: " + event.toString());
    return true;
  }

  override def onDoubleTapEvent(event: MotionEvent): Boolean = {
    println("onDoubleTapEvent: " + event.toString());
    return true;
  }

  override def onSingleTapConfirmed(event: MotionEvent): Boolean = {
    println("onSingleTapConfirmed: " + event.toString());
    val x = event.getX.toFloat / screenSize(0).toFloat
    val y = event.getY.toFloat / screenSize(1).toFloat
    tapCallback(Tap(event.getEventTime, v"$x $y"))
    return true;
  }
}
