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

  type GraphTraversal = Graph.QInstance => Vector[Node.Instance]

  type GFX = UniformCache :+: Load :+: Action :+: CNil
}
