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

  private def buttonEvent(e: XEvent, width: Int, height: Int): InputEvent.Tap = {
    e.readField("xbutton")
    e.xbutton.readField("x")
    e.xbutton.readField("y")
    val xFraction = e.xbutton.x.toFloat / width.toFloat
    val yFraction = 1f - e.xbutton.y.toFloat / height.toFloat
    InputEvent.Tap(System.currentTimeMillis, xFraction, yFraction)
  }

  private def motionEvent(e: XEvent, width: Int, height: Int): InputEvent.Tap = {
    e.readField("xmotion")
    e.xmotion.readField("x")
    e.xmotion.readField("y")
    val xFraction = e.xmotion.x.toFloat / width.toFloat
    val yFraction = 1f - e.xmotion.y.toFloat / height.toFloat
    InputEvent.Tap(System.currentTimeMillis, xFraction, yFraction)
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
          logger.debug("Blank: detected button press")
          val tap = buttonEvent(e, width, height)
          MouseDown(tap) -> Option.empty
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

  case class MouseDown(tap: InputEvent.Tap)
      extends EventRecogniser
      with LazyLogging {
    def handle(e: XEvent)(width: Int,
                          height: Int): (EventRecogniser, Option[InputEvent]) =
      e.`type` match {
        case ButtonRelease =>
          logger.debug(s"MouseDown: detected tap: $tap")
          Blank -> Some(tap)
        case MotionNotify =>
          logger.debug("MouseDown: detected motionNotify")
          Swipe(tap) -> Option.empty
        case LeaveNotify =>
          logger.debug("MouseDown: detected leaveNotity")
          Blank -> Option.empty
        case other =>
          logger.warn("MouseDown: Unhandled event of type {}", other)
          this -> Option.empty
      }
  }

  case class Swipe(start: InputEvent.Tap)
      extends EventRecogniser
      with LazyLogging {
    def handle(e: XEvent)(width: Int,
                          height: Int): (EventRecogniser, Option[InputEvent]) =
      e.`type` match {
        case ButtonRelease =>
          val end = buttonEvent(e, width, height)
          val swipe = InputEvent.Swipe(start, end)
          if (swipe.distance < 0.1) {
            logger.debug("Swipe: detected tap")
            this -> Some(start)
          } else {
            logger.debug(s"Swipe: detected swipe: $swipe")
            this -> Some(swipe)
          }
        case MotionNotify =>
          logger.debug("Swipe: detected motionNotify")
          val t = System.currentTimeMillis
          if(t - start.at > 600L) {
            val tap = motionEvent(e, width, height)
            Drag(start, List(tap)) -> Some(InputEvent.DragStarted(start, tap))
          } 
          else this -> Option.empty
        case LeaveNotify =>
          logger.debug("Swipe: detected leaveNotify")
          Blank -> Option.empty
        case ButtonPress =>
          logger.warn("Swipe: detected unwanted button press - exiting swipe")
          Blank -> Option.empty
        case other =>
          logger.warn("Swipe: Unhandled event of type {}", other)
          this -> Option.empty
      }
  }

  case class Drag(head: InputEvent.Tap, tail: List[InputEvent.Tap]) 
      extends EventRecogniser with LazyLogging {
    def handle(e: XEvent)(width: Int,
                          height: Int): (EventRecogniser, Option[InputEvent]) = 
      e.`type` match {
        case ButtonRelease =>
          logger.debug("Drag: detected button release")
          val end = buttonEvent(e, width, height)
          Blank -> Some(InputEvent.DragFinished(head, tail :+ end))
        case MotionNotify =>
          logger.debug("Drag: detected motionNotify")
          val tap = motionEvent(e, width, height)
          val nextTail = tail :+ tap
          //TODO: add in the DragContinuing events later
          Drag(head, nextTail) -> Option.empty
        case LeaveNotify =>
          logger.debug("Drag: detected leaveNotify")
          Blank -> Some(InputEvent.DragFinished(head, tail))
        case ButtonPress =>
          logger.warn("Drag: detected unwanted button press - exiting drag")
          Blank -> Some(InputEvent.DragFinished(head, tail))
        case other =>
          logger.warn("Drag: unhandled event of type {}, exiting drag", other)
          Blank -> Some(InputEvent.DragFinished(head, tail))
      }
  }
}
