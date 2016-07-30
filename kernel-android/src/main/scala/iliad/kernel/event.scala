package iliad
package kernel

import android.app.Activity
import android.view.{GestureDetector, MotionEvent}
import android.support.v4.view.GestureDetectorCompat

import EventHandler._
import InputEvent._

import com.typesafe.scalalogging._

trait AndroidEventHandler extends Activity with GestureDetector.OnGestureListener
    with GestureDetector.OnDoubleTapListener
    with EventHandler with LazyLogging {

  def width: Int
  def height: Int

  private var eventCallback: Callback[InputEvent] = EventHandler.zero

  var detector: GestureDetectorCompat = _
  var recogniser: EventRecogniser = _

  def onEvent(cb: Callback[InputEvent]): Unit = eventCallback = cb

  private def tapEvent(e: MotionEvent): InputEvent.Tap = {
    val x = e.getX.toFloat / width.toFloat
    val y = e.getY.toFloat / height.toFloat
    Tap(System.currentTimeMillis, x, y)
  }

  override def onTouchEvent(event: MotionEvent): Boolean = {
    logger.debug(s"OnTouchEvent: $event")
    detector.onTouchEvent(event)      
    if(event.getAction() == MotionEvent.ACTION_UP) {
      val (n, e) = recogniser.onActionUp
      recogniser = n
      e.foreach(eventCallback)
    }
    super.onTouchEvent(event)
  }

  override def onDown(event: MotionEvent): Boolean =  {
    logger.debug("onDown: " + event.toString());
    return true;
  }

  override def onFling(event1: MotionEvent, event2: MotionEvent, velocityX: Float, velocityY: Float): Boolean = {
    logger.debug("onFling: " + event1.toString()+event2.toString());
    val (n, e) = recogniser.onFling(event1, event2)
    recogniser = n
    e.foreach(eventCallback)
    return true;
  }

  override def onLongPress(event: MotionEvent): Unit = {
    logger.debug("onLongPress: " + event.toString());
  }

  override def onScroll(e1: MotionEvent, e2: MotionEvent, distanceX: Float, distanceY: Float): Boolean = {
    logger.debug("onScroll: " + e1.toString()+e2.toString());
    val (n, e) = recogniser.onScroll(e1, e2)
    recogniser = n
    e.foreach(eventCallback)
    return true;
  }

  override def onShowPress(event: MotionEvent): Unit = {
    logger.debug("onShowPress: " + event.toString());
  }

  override def onSingleTapUp(event: MotionEvent): Boolean = {
    logger.debug("onSingleTapUp: " + event.toString());
    val (n, e) = recogniser.onTap(event)
    recogniser = n
    e.foreach(eventCallback)
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
    return true;
  }
}

sealed trait EventRecogniser {
  def onTap(event: MotionEvent): (EventRecogniser, Option[InputEvent])
  def onScroll(event1: MotionEvent, event2: MotionEvent): 
      (EventRecogniser, Option[InputEvent])
  def onFling(event1: MotionEvent, event2: MotionEvent): 
      (EventRecogniser, Option[InputEvent])
  def onActionUp: (EventRecogniser, Option[InputEvent])
}

object EventRecogniser {

  private def tapEvent(e: MotionEvent, width: Int, height: Int): InputEvent.Tap = {
    val x = e.getX.toFloat / width.toFloat
    val y = e.getY.toFloat / height.toFloat
    Tap(System.currentTimeMillis, x, y)
  }

  case class Blank(width: Int, height: Int) extends EventRecogniser with LazyLogging {
    def onTap(event: MotionEvent): 
        (EventRecogniser, Option[InputEvent]) = {
      this -> Some(tapEvent(event, width, height))
    }

    def onFling(event1: MotionEvent, event2: MotionEvent): (EventRecogniser, Option[InputEvent]) = {
      this -> Some(InputEvent.Swipe(tapEvent(event1, width, height), tapEvent(event2, width, height)))
    }

    def onScroll(event1: MotionEvent, event2: MotionEvent): (EventRecogniser, Option[InputEvent]) = {
      logger.info("started scroll")
      Scroll(event1, Nil, width, height) -> Option.empty
    }

    def onActionUp: (EventRecogniser, Option[InputEvent]) = {
      this -> Option.empty[InputEvent]
    }
  }

  case class Scroll(head: MotionEvent, tail: List[MotionEvent], width: Int, height: Int) extends EventRecogniser with LazyLogging{
    
    def onTap(event: MotionEvent): 
        (EventRecogniser, Option[InputEvent]) = {
      logger.warn("Scroll: detected invalid tap")
      Blank(width, height) -> Option.empty[InputEvent]
    }

    def onFling(event1: MotionEvent, event2: MotionEvent): 
        (EventRecogniser, Option[InputEvent]) = {
      logger.warn("Scroll: detected invalid fling")
      Blank(width, height) -> Option.empty[InputEvent]
    }

    def onScroll(event1: MotionEvent, event2: MotionEvent): 
        (EventRecogniser, Option[InputEvent]) = {
      Scroll(head, tail :+ event2, width, height) -> Option.empty[InputEvent]
    }

    def onActionUp: (EventRecogniser, Option[InputEvent]) = {
      logger.info("finished scroll")
      Blank(width, height) -> Some(InputEvent.DragFinished(tapEvent(head, width, height), 
        tail.map(e => tapEvent(e, width, height))))
    }
  }
}
