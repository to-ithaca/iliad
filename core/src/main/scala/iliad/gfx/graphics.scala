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

private[iliad] object Graphics {

  case class State(animation: Animation.State, graph: Graph.Instance)
  case class Config(pageSize: Int,
                    graph: Graph.Constructed,
                    algorithm: Algorithm)

  type PRG = ReaderT[StateT[Xor[NonEmptyList[GraphicsError], ?], State, ?],
                     Config,
                     XorT[GL.DSL, GLError, Unit]]

  private val _graph: monocle.Lens[State, Graph.Instance] =
    GenLens[State](_.graph)
  private val _animation: monocle.Lens[State, Animation.State] =
    GenLens[State](_.animation)

  def empty(gc: Graph.Constructed): State =
    State(Map.empty, gc.instance)

  private def liftAction(fa: Action.Effect): PRG =
    KleisliExtra.lift(fa.applyLens(_graph).map(_ => XorT.pure(())))

  private def liftLoad(fa: Load.Effect): PRG =
    ReaderT(cfg => StateT.pure(fa.run(cfg)))

  private def liftAnimation(fa: Animation.Effect): PRG =
    KleisliExtra.lift(
        fa.applyLens(_animation)
          .transformF(a => a.value.right[NonEmptyList[GraphicsError]])
          .map(_ => XorT.pure(())))

  private def transform(g: Graphics): PRG = g match {
    case Inl(a) => liftAnimation(Animation(a))
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

  def draws(s: State, us: Animation.Values)
    : ReaderT[XorT[GL.DSL, IliadError, ?], Config, Unit] =
    for {
      ns <- s.graph
             .nodes(us)
             .local[Config](_.algorithm)
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
