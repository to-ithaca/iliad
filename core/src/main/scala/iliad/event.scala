package iliad

import iliad.implicits._

import fs2._
import fs2.util._

import com.typesafe.scalalogging._

trait EventStream {

  private implicit val S: Strategy = Strategy.fromFixedDaemonPool(1, "worker")

  private def baseStream[A](register: (A => Unit) => Unit)(
      implicit R: Async.Run[Task]): Stream[Task, A] = {
    for {
      q <- Stream.eval(async.unboundedQueue[Task, A])
      _ <- Stream.suspend {
            register { (a: A) =>
              R.unsafeRunAsyncEffects(q.enqueue1(a))(_ => ())
            }
            Stream.emit(())
          }
      a <- q.dequeue
    } yield a
  }

  def eventStream: Stream[Task, InputEvent] = baseStream(EventHandler.onTap)
}


//Moved from kernel
//TODO: Parameterize on at
sealed trait InputEvent

object InputEvent {
  case class Tap(at: Long, x: Float, y: Float) extends InputEvent
}

import InputEvent._

object EventHandler extends LazyLogging {
  type Callback[A] = A => Unit
  def zero[A](a: A): Unit = {}

#+x11
import com.sun.jna.platform.unix.X11._
import iliad.platform.unix.X11

  private var tapCallback: Callback[Tap] = EventHandler.zero

  def onTap(cb: Callback[Tap]): Unit = tapCallback = cb

  def handleEvent(e: XEvent, width: Int, height: Int): Unit = e.`type` match {
    case ButtonPress =>
      logger.debug("received tap")
      e.readField("xbutton")
      val xFraction =
        (e.xbutton.x - e.xbutton.x_root).toFloat / width.toFloat
      val yFraction =
        (e.xbutton.y - e.xbutton.y_root).toFloat / height.toFloat
      tapCallback(Tap(e.xbutton.time.longValue, xFraction, yFraction))
    case other =>
      logger.warn("Unhandled event of type {}", other)
  }
#-x11

}
