package iliad
package kernel

import android.view.{GestureDetector, MotionEvent}

import EventHandler._
import InputEvent._

import com.typesafe.scalalogging._

trait AndroidEventHandler extends GestureDetector.OnGestureListener
    with GestureDetector.OnDoubleTapListener
    with EventHandler with LazyLogging {

  def width: Int
  def height: Int

  private var tapCallback: Callback[Tap] = EventHandler.zero

  def onTap(cb: Callback[Tap]): Unit = tapCallback = cb

  override def onDown(event: MotionEvent): Boolean =  {
    logger.debug("onDown: " + event.toString());
    return true;
  }

  override def onFling(event1: MotionEvent, event2: MotionEvent, velocityX: Float, velocityY: Float): Boolean = {
    logger.debug("onFling: " + event1.toString()+event2.toString());
    return true;
  }

  override def onLongPress(event: MotionEvent): Unit = {
    logger.debug("onLongPress: " + event.toString());
  }

  override def onScroll(e1: MotionEvent, e2: MotionEvent, distanceX: Float, distanceY: Float): Boolean = {
    logger.debug("onScroll: " + e1.toString()+e2.toString());
    return true;
  }

  override def onShowPress(event: MotionEvent): Unit = {
    logger.debug("onShowPress: " + event.toString());
  }

  override def onSingleTapUp(event: MotionEvent): Boolean = {
    logger.debug("onSingleTapUp: " + event.toString());
    return true;
  }

  override def onDoubleTap(event: MotionEvent): Boolean = {
    logger.debug("onDoubleTap: " + event.toString());
    return true;
  }

  override def onDoubleTapEvent(event: MotionEvent): Boolean = {
    logger.debug("onDoubleTapEvent: " + event.toString());
    return true;
  }

  override def onSingleTapConfirmed(event: MotionEvent): Boolean = {
    logger.debug("onSingleTapConfirmed: " + event.toString());
    val x = event.getX.toFloat / width.toFloat
    val y = event.getY.toFloat / height.toFloat
    tapCallback(Tap(event.getEventTime, x, y))
    return true;
  }
}
