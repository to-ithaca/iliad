package iliad

import fs2._

object Fs2Extra {
  implicit def toNestedStreamOps[F[_], A](
      s: Stream[F, F[A]]): NestedStreamOps[F, A] =
    new NestedStreamOps(s)
  implicit def toStreamOps[F[_], A](s: Stream[F, A]): StreamOps[F, A] =
    new StreamOps(s)
}

final class NestedStreamOps[F[_], A](val s: Stream[F, F[A]]) extends AnyVal {
  def eval: Stream[F, A] = s.evalMap(identity)
}

final class StreamOps[F[_], A](val s: Stream[F, A]) extends AnyVal {
  def mapAccumulate2[B, O](b: B)(f: (B, A) => (B, O)): Stream[F, O] =
    s.mapAccumulate(b)(f).map(_._2)
  //def withFilter(p: A => Boolean): WithFilter[F, A] = new WithFilter(s, p)
}

final class WithFilter[F[_], A](s: Stream[F, A], p: A => Boolean) {
  def flatMap[B](f: A => Stream[F, B]): Stream[F, B] = s filter p flatMap f
  def withFilter(q: A => Boolean): WithFilter[F, A] =
    new WithFilter(s, x => p(x) && q(x))
}
