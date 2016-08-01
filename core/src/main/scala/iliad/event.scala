package iliad

import iliad.implicits._
import iliad.kernel._

import fs2._
import fs2.util._

import cats.data._
import cats.implicits._

import com.typesafe.scalalogging._

trait EventStream extends EventHandler with LazyLogging {

  private implicit val S: Strategy = Strategy.fromFixedDaemonPool(8, "worker")

  private def baseStream[A](register: (A => Unit) => Unit)(
      implicit R: Async.Run[Task]): Stream[Task, A] =
    Stream.eval(async.unboundedQueue[Task, A]).flatMap { q =>
      register { (a: A) =>
        q.enqueue1(a)
          .unsafeRunAsync(_.toXor match {
            case Xor.Left(err) => logger.error(s"Error registering event $err")
            case _ =>
          })
      }
      q.dequeue
    }

  def eventStream: Stream[Task, InputEvent] = baseStream(onEvent)
}
