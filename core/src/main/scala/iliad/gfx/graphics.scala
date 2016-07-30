package iliad
package gfx

import iliad.gl._

import cats._
import cats.data._
import cats.implicits._

import monocle.macros._

import shapeless._

import CatsExtra._
import MonocleExtra._

// To workaround SI-7139 `object Graphics` needs to be defined inside the package object
// next to the type alias
trait GraphicsFunctions {

  private val _graph: monocle.Lens[Graphics.State, Graph.Instance] =
    GenLens[Graphics.State](_.graph)
  private val _uniformCache: monocle.Lens[Graphics.State, UniformCache.State] =
    GenLens[Graphics.State](_.uniformCache)

  def empty(gc: Graph.Constructed): Graphics.State =
    Graphics.State(Map.empty, gc.instance)

  private def liftAction(fa: Action.Effect): Graphics.PRG =
    KleisliExtra.lift(fa.applyLens(_graph).map(_ => XorT.pure(())))

  private def liftLoad(fa: Load.Effect): Graphics.PRG =
    ReaderT(cfg => StateT.pure(fa.run(cfg)))

  private def liftUniformCache(fa: UniformCache.Effect): Graphics.PRG =
    KleisliExtra.lift(
        fa.applyLens(_uniformCache)
          .transformF(a => a.leftMap(e => NonEmptyList(e, Nil)))
          .map(_ => XorT.pure(())))

  private def transform(g: Graphics): Graphics.PRG = g match {
    case Inl(a) => liftUniformCache(UniformCache(a))
    case Inr(Inl(l)) => liftLoad(Load(l))
    case Inr(Inr(Inl(a))) => liftAction(Action(a))
    case Inr(Inr(Inr(_))) => sys.error("Impossible case!")
  }

  def apply(gs: List[Graphics]): Graphics.PRG = {
    val start =
      ReaderT.pure[StateT[Xor[NonEmptyList[GraphicsError], ?], Graphics.State, ?],
                   Graphics.Config,
                   XorT[GL.DSL, GLError, Unit]](XorT.pure(()))
    gs.foldLeft(start) { (b, g) =>
      for {
        x <- b
        xx <- transform(g)
      } yield x >> xx
    }
  }

  def draws(s: Graphics.State, us: UniformCache.Values)
    : ReaderT[XorT[GL.DSL, IliadError, ?], Graphics.Config, Unit] =
    for {
      ns <- s.graph
             .nodes(us)
             .local[Graphics.Config](_.graphTraversal)
             .mapF(xor =>
                   XorT
                     .fromXor[GL.DSL]
                     .apply[IliadError, Vector[Node.Drawable]](xor))
      gls <- ReaderT { (cfg: Graphics.Config) =>
              val gls: List[XorT[GL.DSL, GLError, Unit]] =
                ToGL.run(ToGL(ns.toList)).run(cfg.graph)
              gls.sequenceUnit.leftWiden[IliadError]
            }
    } yield gls
}
