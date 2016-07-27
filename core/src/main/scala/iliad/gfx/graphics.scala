package iliad
package gfx

import iliad.gl._

import cats._
import cats.data.{State => CatsState, StateT, Xor, XorT, ReaderT, Reader, NonEmptyList}
import cats.implicits._

import monocle.macros._

import shapeless._

import CatsExtra._
import MonocleExtra._

object Graphics {

  type Graphics = UniformCache :+: Load :+: Action :+: CNil

  case class Config(pageSize: Int,
                    graph: Graph.Constructed,
                    graphTraversal: GraphTraversal)

  case class State(uniformCache: UniformCache.State, graph: Graph.Instance)

  type PRG = ReaderT[StateT[Xor[NonEmptyList[GraphicsError], ?], State, ?],
                     Config,
                     XorT[GL.DSL, GLError, Unit]]

  private val _graph: monocle.Lens[State, Graph.Instance] =
    GenLens[State](_.graph)
  private val _uniformCache: monocle.Lens[State, UniformCache.State] =
    GenLens[State](_.uniformCache)

  def empty(gc: Graph.Constructed): State =
    State(Map.empty, gc.instance)

  private def liftAction(fa: Action.Effect): PRG =
    KleisliExtra.lift(fa.applyLens(_graph).map(_ => XorT.pure(())))

  private def liftLoad(fa: Load.Effect): PRG =
    ReaderT(cfg => StateT.pure(fa.run(cfg)))

  private def liftUniformCache(fa: UniformCache.Effect): PRG =
    KleisliExtra.lift(
        fa.applyLens(_uniformCache)
          .transformF(a => a.leftMap(e => NonEmptyList(e, Nil)))
          .map(_ => XorT.pure(())))

  private def transform(g: Graphics): PRG = g match {
    case Inl(a) => liftUniformCache(UniformCache(a))
    case Inr(Inl(l)) => liftLoad(Load(l))
    case Inr(Inr(Inl(a))) => liftAction(Action(a))
    case Inr(Inr(Inr(_))) => sys.error("Impossible case!")
  }

  def apply(gs: List[Graphics]): PRG = {
    val start =
      ReaderT.pure[StateT[Xor[NonEmptyList[GraphicsError], ?], State, ?],
                   Config,
                   XorT[GL.DSL, GLError, Unit]](XorT.pure(()))
    gs.foldLeft(start) { (b, g) =>
      for {
        x <- b
        xx <- transform(g)
      } yield x >> xx
    }
  }

  def draws(s: State, us: UniformCache.Values)
    : ReaderT[XorT[GL.DSL, IliadError, ?], Config, Unit] =
    for {
      ns <- s.graph
             .nodes(us)
             .local[Config](_.graphTraversal)
             .mapF(xor =>
                   XorT
                     .fromXor[GL.DSL]
                     .apply[IliadError, Vector[Node.Drawable]](xor))
      gls <- ReaderT { (cfg: Config) =>
              val gls: List[XorT[GL.DSL, GLError, Unit]] =
                ToGL.run(ToGL(ns.toList)).run(cfg.graph)
              gls.sequenceUnit.leftWiden[IliadError]
            }
    } yield gls
}
