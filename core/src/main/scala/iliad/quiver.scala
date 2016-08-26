package iliad

import quiver._

trait QuiverInstances {
  implicit def graphOps[N, A, B](g: Graph[N, A, B]): GraphOps[N, A, B] =
    new GraphOps[N, A, B](g)
}

final class GraphOps[N, A, B](val g: Graph[N, A, B]) extends AnyVal {

  def orderedWith(ns: Seq[N], acc: Vector[N]): Vector[N] =
    if (ns.isEmpty || g.isEmpty) acc
    else
      g.decomp(ns.head) match {
        case Decomp(Some(c), nextG) =>
          if (c.predecessors.forall(acc.contains))
            (new GraphOps(nextG))
              .orderedWith(ns.tail ++ c.successors, acc :+ c.vertex)
          else orderedWith(ns.tail :+ ns.head, acc)
        case Decomp(None, nextG) =>
          sys.error(s"Node ${ns.head} not present in quiver graph")
      }

  def ordered: Vector[N] = orderedWith(g.roots.toSeq, Vector.empty)
}
