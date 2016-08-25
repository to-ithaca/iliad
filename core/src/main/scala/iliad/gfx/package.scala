package iliad

import iliad.{gl => GL}

import shapeless._

import cats.data.{ReaderT, StateT, Xor, XorT, NonEmptyList}

package object gfx
    extends LoadFunctions
    with ActionFunctions
    with UniformCacheFunctions
    with AnimationFunctions
    with ConstructFunctions
    with InstantiateFunctions
    with ShaderFunctions {

  type GraphTraversal = Graph.Instance => Vector[Node.Instance]

  object GraphTraversal {
    val ordered: GraphTraversal = g => 
    g.constructed.nodes.flatMap { n =>
      g.graph.nodes.filter(_.constructor == n)
    }
  }

  type GFX = UniformCache :+: Load :+: Action :+: CNil
}
