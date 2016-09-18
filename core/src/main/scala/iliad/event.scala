package iliad

import iliad.algebra._
import iliad.algebra.syntax.vector._

import fs2._
import fs2.util._

import cats._
import cats.data._
import cats.implicits._

import spire.implicits._

import com.typesafe.scalalogging._

import scala.concurrent.duration._

import shapeless.nat

trait EventStream extends LazyLogging {

  private implicit val S: Strategy = Strategy.fromFixedDaemonPool(1, "worker")

  private def baseStream[A](register: (A => Unit) => Unit): Stream[Task, A] =
    Stream.eval(async.boundedQueue[Task, A](10)).flatMap { q =>
      register { (a: A) =>
        q.enqueue1(a)
          .unsafeRunAsync(_.toXor match {
            case Xor.Left(err) => logger.error(s"Error registering event $err")
            case _ =>
          })
      }
      q.dequeue
    }

  def eventStream: Stream[Task, InputEvent] = baseStream(EventHandler.onEvent)
}

sealed trait InputEvent {
  def recent: Long
}

object InputEvent {
  case class Point(at: Long, position: Vec2d) {

    /** window coordinates range from -1.0 to 1.0 */
    def windowCoord: Vec2d = (position :* 2.0) - 1.0
  }

  case class Tap(point: Point) extends InputEvent {
    lazy val at: Long = point.at
    lazy val recent: Long = at
    
    def position: Vec2d = point.position

    /** window coordinates range from -1.0 to 1.0 */
    def windowCoord: Vec2d = point.windowCoord
  }

  case class DragStarted(start: Point, current: Point) extends InputEvent {
    lazy val recent: Long = current.at
  }
  case class DragContinued(points: List[InputEvent.Point])
      extends InputEvent {
    lazy val recent: Long = end.at
    def start: Point = points.toList.last
    def prev: Point = points.tail.head
    def end: Point = points.head
    def distance: Double = (end.position - start.position).norm

    def isLongPress(duration: Long, deviation: Double): Boolean = 
      (end.at - start.at) > duration && standardDeviation < deviation

    def standardDeviation: Double = {
      val average = points.map(_.position).foldLeft(Vector.zero[nat._2, Double])((p, n) => p + n) :/ points.size.toDouble
      val sum = points.map(p => (p.position - average).norm).map(x => x * x).sum
      Math.sqrt(sum)
    }
  }

  case class DragBecameSwipe(points: List[InputEvent.Point])
      extends InputEvent {
    lazy val recent: Long = end.at
    lazy val start: Point = points.toList.last
    lazy val end: Point = points.head
    lazy val distance: Double = (end.position - start.position).norm
    
    def duration: Long = end.at - start.at
    def direction: Vec2d = (end.position - start.position).normalize
    def isDown(acceptance: Double): Boolean = direction ⋅ v"0.0 -1.0" > acceptance
    def isLeft(acceptance: Double): Boolean = direction ⋅ v"-1.0 0.0" > acceptance
    def isRight(acceptance: Double): Boolean = direction ⋅ v"1.0 0.0" > acceptance
    def isUp(acceptance: Double): Boolean = direction ⋅ v"0.0 1.0" > acceptance
  }

  case class DragFinished(points: List[InputEvent.Point]) extends InputEvent {
    lazy val recent: Long = points.head.at
    lazy val start: Point = points.toList.last
    lazy val end: Point = points.head
  }

  def distance(s: Point, e: Point): Double = (e.position - s.position).norm
}

import InputEvent._

#+x11
import com.sun.jna.platform.unix.X11._
import iliad.platform.unix.X11

object EventHandler {

  type Callback[A] = A => Unit
  def zero[A](a: A): Unit = {}

  private var eventCallback: Callback[InputEvent] = EventHandler.zero

  def onEvent(cb: Callback[InputEvent]): Unit = eventCallback = cb

  private var recogniser: EventRecogniser = EventRecogniser.Blank

  def handleEvent(e: XEvent, width: Int, height: Int): Unit = {
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

  val minDt = ((1 / 30) seconds).toMillis

  private def buttonEvent(e: XEvent,
                          width: Int,
                          height: Int): InputEvent.Point = {
    e.readField("xbutton")
    e.xbutton.readField("x")
    e.xbutton.readField("y")
    val xFraction = e.xbutton.x.toDouble / width.toDouble
    val yFraction = 1f - e.xbutton.y.toDouble / height.toDouble
    InputEvent.Point(System.currentTimeMillis, v"$xFraction $yFraction")
  }

  private def motionEvent(e: XEvent,
                          width: Int,
                          height: Int): InputEvent.Point = {
    e.readField("xmotion")
    e.xmotion.readField("x")
    e.xmotion.readField("y")
    val xFraction = e.xmotion.x.toDouble / width.toDouble
    val yFraction = 1f - e.xmotion.y.toDouble / height.toDouble
    InputEvent.Point(System.currentTimeMillis, v"$xFraction $yFraction")
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
          DragContinuing(current :: List(point)) -> Some(
              InputEvent.DragStarted(point, current))
        case LeaveNotify =>
          logger.warn("MouseDown: detected leaveNotity")
          Blank -> Option.empty
        case other =>
          logger.warn("MouseDown: Unhandled event of type {}", other)
          this -> Option.empty
      }
  }

  case class DragContinuing(points: List[InputEvent.Point])
      extends EventRecogniser
      with LazyLogging {

    def start: InputEvent.Point = points.toList.last

    def handle(e: XEvent)(width: Int,
                          height: Int): (EventRecogniser, Option[InputEvent]) =
      e.`type` match {
        case ButtonRelease =>
          val end = buttonEvent(e, width, height)
          if (InputEvent.distance(start, end) < 0.01) {
            logger.info("DragContinuing: detected tap")
            Blank -> Some(InputEvent.Tap(start))
          } else if (end.at - start.at < 1000L) {
            logger.info("DragContinuing: detected swipe")
            Blank -> Some(InputEvent.DragBecameSwipe(end :: points))
          } else {
            logger.info(s"DragContinuing: detected drag finish")
            Blank -> Some(InputEvent.DragFinished(end :: points))
          }
        case MotionNotify =>
          logger.debug("DragContinuing: detected drag")
          val end = motionEvent(e, width, height)
          if((end.at - points.head.at) > minDt) 
            DragContinuing(end :: points) -> Some(InputEvent.DragContinued(end :: points))
          else this -> Option.empty
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
#-x11

#+win32
import com.sun.jna.platform.win32.User32._
import com.sun.jna.platform.win32.WinDef._

import iliad.platform.win32.User32._
import iliad.platform.win32.User32.Macros._

object EventHandler extends LazyLogging {

  type Callback[A] = A => Unit
  def zero[A](a: A): Unit = {}

  private var eventCallback: Callback[InputEvent] = EventHandler.zero

  def onEvent(cb: InputEvent => Unit): Unit = eventCallback = cb

  def handleEvent(hwnd: HWND,
                  uMsg: Int,
                  wParam: WPARAM,
                  lParam: LPARAM, 
    width: Int, height: Int): Boolean =
    uMsg match {
      case WM_LBUTTONDOWN =>
        logger.debug("received tap")
        val xFraction = Macros.GET_X_LPARAM(lParam).toDouble / width.toDouble
        val yFraction = GET_Y_LPARAM(lParam).toDouble / height.toDouble
        //TODO: windows must have a better way of getting the time
        eventCallback(InputEvent.Tap(InputEvent.Point(System.currentTimeMillis(), v"$xFraction $yFraction")))
        true
      case _ => false
    }
}
#-win32

#+android
object EventHandler {
  type Callback[A] = A => Unit
  def zero[A](a: A): Unit = {}

  private var eventCallback: Callback[InputEvent] = EventHandler.zero
  def onEvent(cb: Callback[InputEvent]): Unit = eventCallback = cb

  def handleEvent(e: InputEvent): Unit = eventCallback(e)
}

import android.view.{GestureDetector, MotionEvent}
trait AndroidEventHandler extends GestureDetector.OnGestureListener
    with GestureDetector.OnDoubleTapListener
    with LazyLogging {

  def width: Int
  def height: Int

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
    val x = event.getX.toDouble / width.toDouble
    val y = event.getY.toDouble / height.toDouble
    EventHandler.handleEvent(InputEvent.Tap(InputEvent.Point(event.getEventTime, v"$x $y")))
    return true;
  }
}
#-android
