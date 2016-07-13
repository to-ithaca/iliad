package cats
package free

import cats.data.Xor

object FreeExtra {

  implicit def freeTraverse[S[_]: Foldable : Traverse: Functor]: Traverse[Free[S, ?]] = 
    new FreeTraverse[S] {
      def F0: Foldable[S] = Foldable[S]
      def F1: Functor[S] = Functor[S]
      def F2: Traverse[S] = Traverse[S]
    }
}

private trait FreeFoldable[S[_]] extends Foldable[Free[S, ?]] {

  def F0: Foldable[S]
  implicit def F1: Functor[S]

  def foldLeft[A, B](fa: Free[S,A], b: B)(f: (B, A) => B): B = 
    fa.resume match {
      case Xor.Right(a) => f(b, a)
      case Xor.Left(s) => F0.foldLeft(s, b)((b, a) => foldLeft(a, b)(f))
    }

  def foldRight[A, B](fa: Free[S,A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = 
    fa.resume match {
      case Xor.Right(a) => f(a, lb)
      case Xor.Left(s) => F0.foldRight(s, lb)(foldRight(_, _)(f))
    }
}

private trait FreeTraverse[S[_]] extends Traverse[Free[S, ?]] 
    with FreeFoldable[S] {

  implicit def F1: Functor[S]
  def F2: Traverse[S]

  def traverse[G[_]: Applicative, A, B](fa: Free[S,A])(f: A => G[B]): G[Free[S,B]] =
    fa.resume match {
      case Xor.Right(a) => 
        Applicative[G].map(f(a))(Free.pure)
      case Xor.Left(s) =>
        val faa: G[S[Free[S, B]]] = F2.traverse(s)(traverse(_)(f))
        Applicative[G].map(faa)(Free.liftF(_).flatMap(identity))
    }
}
