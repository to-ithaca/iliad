package iliad
package kernel

import com.sun.jna.platform.unix.X11._
import iliad.kernel.platform.unix.X11

import org.slf4j._

import com.typesafe.scalalogging._

import EventHandler._

trait X11EventHandler extends EventHandler {

  private val log = LoggerFactory.getLogger(classOf[X11EventHandler])

  //TODO: This should be the meta information of the actual "platform itself"
  def width: Int
  def height: Int

  private var eventCallback: Callback[InputEvent] = EventHandler.zero

  def onEvent(cb: Callback[InputEvent]): Unit = eventCallback = cb

  private var recogniser: EventRecogniser = EventRecogniser.Blank

  def handleEvent(e: XEvent): Unit = {
    val (next, eventOpt) = recogniser.handle(e)(width, height)
    eventOpt.foreach(eventCallback)
    recogniser = next
  }
}

sealed trait EventRecogniser {
  def handle(e: XEvent)(width: Int,
                        height: Int): (EventRecogniser, Option[InputEvent])
}

object EventRecogniser {

  private def buttonEvent(e: XEvent,
                          width: Int,
                          height: Int): InputEvent.Point = {
    e.readField("xbutton")
    e.xbutton.readField("x")
    e.xbutton.readField("y")
    val xFraction = e.xbutton.x.toFloat / width.toFloat
    val yFraction = 1f - e.xbutton.y.toFloat / height.toFloat
    InputEvent.Point(System.currentTimeMillis, xFraction, yFraction)
  }

  private def motionEvent(e: XEvent,
                          width: Int,
                          height: Int): InputEvent.Point = {
    e.readField("xmotion")
    e.xmotion.readField("x")
    e.xmotion.readField("y")
    val xFraction = e.xmotion.x.toFloat / width.toFloat
    val yFraction = 1f - e.xmotion.y.toFloat / height.toFloat
    InputEvent.Point(System.currentTimeMillis, xFraction, yFraction)
  }

  /**Captures events without propagation */
  case class Capture(evts: List[String])
      extends EventRecogniser
      with LazyLogging {
    def handle(e: XEvent)(width: Int,
                          height: Int): (EventRecogniser, Option[InputEvent]) =
      e.`type` match {
        case ButtonPress =>
          Capture("ButtonPress" :: evts) -> Option.empty
        case MotionNotify =>
          Capture("MotionNotify" :: evts) -> Option.empty
        case ButtonRelease =>
          Capture("ButtonRelease" :: evts) -> Option.empty
        case LeaveNotify =>
          Capture("LeaveNotify" :: evts) -> Option.empty
        case other =>
          logger.warn("Unhandled event of type {}", other)
          this -> Option.empty
      }
    override def toString: String =
      s"Capture(captured: ${evts.reverse})"
  }

  case object Blank extends EventRecogniser with LazyLogging {
    def handle(e: XEvent)(width: Int,
                          height: Int): (EventRecogniser, Option[InputEvent]) =
      e.`type` match {
        case ButtonPress =>
          logger.info("Blank: detected button press")
          val point = buttonEvent(e, width, height)
          MouseDown(point) -> Option.empty
        case MotionNotify =>
          this -> Option.empty
        case ButtonRelease =>
          this -> Option.empty
        case LeaveNotify =>
          this -> Option.empty
        case other =>
          logger.warn("Blank: Unhandled event of type {}", other)
          this -> Option.empty
      }
  }

  case class MouseDown(point: InputEvent.Point)
      extends EventRecogniser
      with LazyLogging {
    def handle(e: XEvent)(width: Int,
                          height: Int): (EventRecogniser, Option[InputEvent]) =
      e.`type` match {
        case ButtonRelease =>
          val tap = InputEvent.Tap(point)
          logger.info(s"MouseDown: detected tap: $tap")
          Blank -> Some(tap)
        case MotionNotify =>
          logger.info("MouseDown: detected motionNotify")
          val current = motionEvent(e, width, height)
          DragContinuing(point, List(current)) -> Some(
              InputEvent.DragStarted(point, current))
        case LeaveNotify =>
          logger.warn("MouseDown: detected leaveNotity")
          Blank -> Option.empty
        case other =>
          logger.warn("MouseDown: Unhandled event of type {}", other)
          this -> Option.empty
      }
  }

  case class DragContinuing(start: InputEvent.Point,
                            tail: List[InputEvent.Point])
      extends EventRecogniser
      with LazyLogging {
    def handle(e: XEvent)(width: Int,
                          height: Int): (EventRecogniser, Option[InputEvent]) =
      e.`type` match {
        case ButtonRelease =>
          val end = buttonEvent(e, width, height)
          if (InputEvent.distance(start, end) < 0.1) {
            logger.info("DragContinuing: detected tap")
            Blank -> Some(InputEvent.Tap(start))
          } else if (end.at - start.at < 1000L) {
            logger.info("DragContinuing: detected swipe")
            Blank -> Some(InputEvent.DragBecameSwipe(start, tail :+ end))
          } else {
            logger.info(s"DragContinuing: detected drag finish")
            Blank -> Some(InputEvent.DragFinished(start, tail :+ end))
          }
        case MotionNotify =>
          logger.debug("DragContinuing: detected drag")
          val end = motionEvent(e, width, height)
          this -> Some(InputEvent.DragContinued(start, tail :+ end))
        case LeaveNotify =>
          logger.warn("DragContinuing: detected leaveNotify")
          Blank -> Option.empty
        case ButtonPress =>
          logger.warn("DragContinuing: detected button press")
          Blank -> Option.empty
        case other =>
          logger.warn("DragContinuing: Unhandled event of type {}", other)
          this -> Option.empty
      }
  }
}
