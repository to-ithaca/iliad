package iliad
package gl

import cats._
import cats.free._, Free._

sealed trait Load[A]

case class LoadVertexShader(s: VertexShader.Source)
    extends Load[VertexShader.Compiled]
case class LoadFragmentShader(s: FragmentShader.Source)
    extends Load[FragmentShader.Compiled]
case class LoadProgram(vs: VertexShader.Compiled, fs: FragmentShader.Compiled)
    extends Load[Program.Linked]

object Load {
  type DSL[A] = Free[Load, A]

  def apply(s: VertexShader.Source): DSL[VertexShader.Compiled] =
    liftF(LoadVertexShader(s))
  def apply(s: FragmentShader.Source): DSL[FragmentShader.Compiled] =
    liftF(LoadFragmentShader(s))
  def apply(vs: VertexShader.Compiled,
            fs: FragmentShader.Compiled): DSL[Program.Linked] =
    liftF(LoadProgram(vs, fs))
  def apply(p: Program.Unlinked): DSL[Program.Linked] =
    for {
      v <- Load(p.vs)
      f <- Load(p.fs)
      p <- Load(v, f)
    } yield p

  def parse[F[_]: Monad](f: GL ~> F): Load ~> F = new (Load ~> F) {
    def apply[A](load: Load[A]): F[A] = LoadParser(load).foldMap(f)
  }
}

private object LoadParser extends (Load ~> GL.DSL) {
  def apply[A](load: Load[A]): GL.DSL[A] = load match {
    case LoadVertexShader(s) =>
      GL.makeVertexShader(s.s).map(VertexShader.Compiled(_, s))
    case LoadFragmentShader(s) => GL.makeFragmentShader(s.s).map(FragmentShader.Compiled(_, s))
    case LoadProgram(vs, fs) => GL.makeProgram(vs.id, fs.id).map(Program.Linked(_, Program.Unlinked(vs.s, fs.s)))
  }
}
