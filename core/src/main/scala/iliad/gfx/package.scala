package iliad

import iliad.{gl => GL}

import shapeless._

import cats.data.{ReaderT, StateT, Xor, XorT, NonEmptyList}

package object gfx
    extends LoadFunctions
    with ActionFunctions
    with UniformCacheFunctions
    with ConstructFunctions
    with InstantiateFunctions
    with ShaderFunctions 
    with Update.ToUpdateOps 
    with Render.ToRenderOps {

  type GraphTraversal = Graph.Instance => Vector[Node.Instance]

  object GraphTraversal {
    val ordered: GraphTraversal = g =>
    g.constructed.nodes.flatMap { n =>
      g.graph.nodes.filter(_.constructor == n.constructor)
    }
  }

  type GFX = UniformCache :+: Load :+: Action :+: CNil
}
