package iliad
package kernel

import scala.concurrent.duration._

import fs2._
import fs2.util._

import org.scalatest._

class BlockingPromiseTests extends FunSuite with Matchers with Inside {

  implicit val S: Strategy = Strategy.fromFixedDaemonPool(2)
  implicit val scheduler = Scheduler.fromFixedDaemonPool(2)

  test("blocking promise calls callback on success") {
    val p = new BlockingPromise[Int]
    Task.delay(10 milliseconds).map(_ => p.set(1)).unsafeRunAsync(_ => ())
    p.task.unsafeRun shouldBe 1
  }

  test("blocking promise calls callback on failure") {
    val p = new BlockingPromise[Int]
    val err = new Error("some error")
    p.set(err)
    inside(p.task.unsafeAttemptRun) {
      case Left(e) => err shouldBe e
    }
  }
}
