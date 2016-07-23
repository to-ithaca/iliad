package iliad

import shapeless._

package object gfx
    extends LoadFunctions
    with ActionFunctions
    with UniformCacheFunctions
    with AnimationFunctions
    with ConstructFunctions
    with InstantiateFunctions 
    with ShaderFunctions {

  type GraphTraversal = Graph.QInstance => Vector[Node.Instance]
}
