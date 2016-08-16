package iliad

import iliad.implicits._

import fs2._
import fs2.util._

trait EventStream extends EventHandler {

  private implicit val S: Strategy = Strategy.fromFixedDaemonPool(8, "worker")

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

  def eventStream: Stream[Task, InputEvent] = baseStream(onTap)
}


//Moved from kernel
//TODO: Parameterize on at
sealed trait InputEvent

object InputEvent {
  case class Tap(at: Long, x: Float, y: Float) extends InputEvent
}

object EventHandler {
  type Callback[A] = A => Unit
  def zero[A](a: A): Unit = {}
}

import InputEvent._
import EventHandler._

trait EventHandler {
  def onTap(cb: Callback[Tap]): Unit
}
