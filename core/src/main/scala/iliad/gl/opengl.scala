package iliad
package gl

import iliad.kernel.platform.GLES30Library

import cats._
import cats.data._
import cats.free._, Free._
import cats.implicits._

import freek._

/**
TODO: split this into different traits
keep the runInterpreter in a separate trait
put the type aliases in a package object if necessary

Writer State for Cached
lift both into a common stack
*/

object GL {

  type DSL[A] = Free[GL, A]

  val createProgram: DSL[Int] = liftF(GLCreateProgram)
  val createVertexShader: DSL[Int] = liftF(GLCreateShader(???))
  val createFragmentShader: DSL[Int] = liftF(GLCreateShader(???))
  def shaderSource(shader: Int, sources: List[String]): DSL[Unit] =
    liftF(GLShaderSource(shader, sources))
  def compileShader(shader: Int): DSL[Unit] = liftF(GLCompileShader(shader))
  def attachShader(program: Int, shader: Int): DSL[Unit] =
    liftF(GLAttachShader(program, shader))
  def linkProgram(program: Int): DSL[Unit] = liftF(GLLinkProgram(program))

  def makeVertexShader(source: String): DSL[Int] =
    for {
      id <- createVertexShader
      _ <- shaderSource(id, List(source))
      _ <- compileShader(id)
    } yield id

  val runInterpreter: GL ~> Reader[GLES30Library, ?] =
    new (GL ~> Reader[GLES30Library, ?]) {
      def apply[A](gl: GL[A]): Reader[GLES30Library, A] = gl match {
        case GLCreateShader(t) => Reader(_.glCreateShader(t.value))
        case GLShaderSource(shader, sources) =>
          Reader(
              _.glShaderSource(shader,
                               sources.size,
                               sources.toArray,
                               sources.map(_.length).toArray))
        case GLCompileShader(shader) => Reader(_.glCompileShader(shader))
        case GLCreateProgram => Reader(_.glCreateProgram())
        case GLAttachShader(program, shader) =>
          Reader(_.glAttachShader(program, shader))
        case GLLinkProgram(program) => Reader(_.glLinkProgram(program))
      }
    }

  type LogEffect[A] = ReaderT[Writer[List[String], ?], GLES30Library, A]

  val logAfter: Id ~> Writer[List[String], ?] = new (Id ~> Writer[List[String], ?]) {
    def apply[A](a: Id[A]): Writer[List[String], A] = a.writer(List(s"returned $a"))
  }

  val logInterpreter: GL ~> LogEffect = new (GL ~> LogEffect) {
    def apply[A](gl: GL[A]): LogEffect[A] = {
      List(s"called $gl").tell.liftT[ReaderT[?[_], GLES30Library, ?]] >> 
      runInterpreter(gl).transform(logAfter)
    }
  }
}

sealed trait GL[A]

case class GLCreateShader(`type`: ShaderType) extends GL[Int]
case class GLShaderSource(shader: Int, sources: List[String]) extends GL[Unit]
case class GLCompileShader(shader: Int) extends GL[Unit]
case object GLCreateProgram extends GL[Int]
case class GLAttachShader(program: Int, shader: Int) extends GL[Unit]
case class GLLinkProgram(program: Int) extends GL[Unit]

object Load {
  type DSL[A] = Free[Load, A]

  def apply(s: VertexShader.Source): DSL[VertexShader.Compiled] =
    liftF(LoadVertexShader(s))
  def apply(s: FragmentShader.Source): DSL[FragmentShader.Compiled] =
    liftF(LoadFragmentShader(s))
  def apply(vs: VertexShader.Compiled,
            fs: FragmentShader.Compiled): DSL[Program.Linked] =
    liftF(LoadProgram(vs, fs))
  def apply(p: Program.UnLinked): DSL[Program.Linked] =
    for {
      v <- Load(p.vs)
      f <- Load(p.fs)
      p <- Load(v, f)
    } yield p

  val parse: DSL ~> GL = ???
}

sealed trait Load[A]

case class LoadVertexShader(s: VertexShader.Source)
    extends Load[VertexShader.Compiled]
case class LoadFragmentShader(s: FragmentShader.Source)
    extends Load[FragmentShader.Compiled]
case class LoadProgram(vs: VertexShader.Compiled, fs: FragmentShader.Compiled)
    extends Load[Program.Linked]

object Cached {
  type DSL[A] = Free[Cached, A]

  def get(vs: VertexShader.Source): DSL[Option[VertexShader.Compiled]] =
    liftF(VertexShaderGet(vs))
  def put(vs: VertexShader.Compiled): DSL[Unit] = liftF(VertexShaderPut(vs))
  def get(fs: FragmentShader.Source): DSL[Option[FragmentShader.Compiled]] =
    liftF(FragmentShaderGet(fs))
  def put(fs: FragmentShader.Compiled): DSL[Unit] =
    liftF(FragmentShaderPut(fs))
  def get(p: Program.UnLinked): DSL[Option[Program.Linked]] =
    liftF(ProgramGet(p))
  def put(p: Program.Linked): DSL[Unit] = liftF(ProgramPut(p))

  type CachedState = String

  def cache: DSL ~> State[CachedState, ?] = ???
}

sealed trait Cached[A]

case class VertexShaderGet(vs: VertexShader.Source)
    extends Cached[Option[VertexShader.Compiled]]
case class VertexShaderPut(vs: VertexShader.Compiled) extends Cached[Unit]
case class FragmentShaderGet(fs: FragmentShader.Source)
    extends Cached[Option[FragmentShader.Compiled]]
case class FragmentShaderPut(fs: FragmentShader.Compiled) extends Cached[Unit]
case class ProgramGet(p: Program.UnLinked)
    extends Cached[Option[Program.Linked]]
case class ProgramPut(p: Program.Linked) extends Cached[Unit]

object CachedLoad {

  type CLoad[A] = (Load.DSL :|: Cached.DSL :|: FXNil)#Cop[A]
  type DSL[A] = Free[CLoad, A]

  def apply(s: VertexShader.Source): DSL[VertexShader.Compiled] =
    Cached.get(s).freek[CLoad] flatMap {
      case Some(v) => Free.pure(v)
      case None =>
        for {
          v <- Load(s).freek[CLoad]
          _ <- Cached.put(v).freek[CLoad]
        } yield v
    }

  //Load ~> GL
}
object VertexShader {
  case class Source(s: String)
  case class Compiled(id: Int, s: Source)
}

object FragmentShader {
  case class Source(s: String)
  case class Compiled(id: Int, s: Source)
}

object Program {
  case class UnLinked(vs: VertexShader.Source, fs: FragmentShader.Source)
  case class Linked(id: Int, unlinked: UnLinked)
}
