package iliad

import iliad.implicits._
import iliad.kernel._

import fs2._
import fs2.util._

trait EventStream extends EventHandler {

  private implicit val S: Strategy = Strategy.fromFixedDaemonPool(8, "worker")

  private def baseStream[A](register: (A => Unit) => Unit)(
      implicit R: Async.Run[Task]): Stream[Task, A] =
    Stream.eval(async.unboundedQueue[Task, A]).flatMap { q =>
      register { (a: A) =>
        println(s"REGISTERING EVENT $a")
        q.enqueue1(a).unsafeRunAsync(msg => println(s"RAN REGISTRATION $msg"))
      }
      q.dequeue
    }

  def eventStream: Stream[Task, InputEvent] = baseStream(onEvent)
}
