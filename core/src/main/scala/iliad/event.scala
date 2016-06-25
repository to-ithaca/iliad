package iliad

import iliad.implicits._
import iliad.kernel._

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
