package iliad
package kernel

import fs2._
import fs2.util._

/** promise that blocks on the thread until the value is set*/
private[kernel] final class BlockingPromise[A] {

  private var _session: Option[Either[Throwable, A]] = None

  def set(either: Either[Throwable, A]): Unit = {
    this.synchronized {
      _session = Some(either)
      notify()
    }
  }

  def set(t: Throwable): Unit = set(Left(t))
  def set(a: A): Unit = set(Right(a))

  def task(implicit S: Strategy): Task[A] = Task.async {
    cb => this.synchronized {
      if(_session.isEmpty) {
        this.wait()
      }  
      cb(_session.get)
    }
  }
}
