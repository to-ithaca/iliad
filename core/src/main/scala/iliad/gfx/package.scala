package iliad

import shapeless._

package object gfx
    extends LoadFunctions
    with ActionFunctions
    with UniformCacheFunctions
    with ConstructFunctions 
    with InstantiateFunctions {
  type Graphics = UniformCache :+: Load :+: Action :+: CNil

  type GraphTraversal = Graph.QInstance => Vector[Node.Instance]
}
