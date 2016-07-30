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

  type Graphics = UniformCache :+: Load :+: Action :+: CNil
  object Graphics extends GraphicsFunctions {
      case class Config(pageSize: Int,
                    graph: Graph.Constructed,
                    graphTraversal: GraphTraversal)

    case class State(uniformCache: UniformCache.State, graph: Graph.Instance)

    type PRG = ReaderT[StateT[Xor[NonEmptyList[GraphicsError], ?], State, ?],
                     Config,
                     XorT[GL.GL.DSL, GL.GLError, Unit]]
  }
}
