package iliad

import iliad.implicits._
import iliad.kernel._

import fs2._
import fs2.util._

trait EventStream { handler: EventHandler =>

  private implicit val S: Strategy = Strategy.fromFixedDaemonPool(8, "worker")

  private def baseStream[A](register: (A => Unit) => Unit)(implicit R: Async.Run[Task]): Stream[Task, A] = {
    for {
      q <- Stream.eval(async.unboundedQueue[Task, A])
      _ <- Stream.suspend {
        register {(a: A) => R.unsafeRunEffects(q.enqueue1(a)) }
        Stream.emit(())
      }
      a <- q.dequeue
    } yield a
  }

  def eventStream: Stream[Task, Event] = baseStream(handler.registerTapCallback).map(Event.Touch)
}
