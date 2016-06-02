package iliad
package kernel
package egl

import scala.reflect._

import cats._
import cats.data._
import cats.implicits._


/** Runs EGL commands with the [[EGL.Logger]] (WriterT) effect type */
class Logging[F[_]: Monad, NDisp, NWin, Disp, Cfg: ClassTag, Sfc, Ctx](egl: EGL[F, NDisp, NWin, Disp, Cfg, Sfc, Ctx]) extends EGL[EGL.Logger[F, ?], NDisp, NWin, Disp, Cfg, Sfc, Ctx] {  
  import EGL._
  import Constants._

  type FIO[A] = ReaderT[F, EGLLib, A]

  private def lift[A](io: FIO[A]): IO[A] = io.mapF{ fa => fa.liftT[EGL.Logger] }
  private def log[A](s: String)(io: FIO[A]): IO[A] = lift(io).mapF {_.mapWritten(_ => List(s))}
  private def logOutput[A](io: FIO[A])(logf: A => String) = lift(io).mapF(_.mapBoth( (_, a) => (List(logf(a)), a)))

  def getError(): IO[Int] = log("getError")(egl.getError)

  def getDisplay(displayId: NDisp): IO[Disp] = log("getDisplay")(egl.getDisplay(displayId))
  def initialise(display: Disp): IO[(Int, Int)] = log("initialise")(egl.initialise(display))

  def chooseConfig(display: Disp, attributes: ConfigAttributes): IO[Cfg] = logOutput(egl.chooseConfig(display, attributes))(cfg => s"eglChooseConfig: chosen config $cfg")
  def createWindowSurface(display: Disp, config: Cfg, win: NWin, attributes: WindowAttributes): IO[Sfc] = log("createWindowSurface")(egl.createWindowSurface(display, config, win, attributes))
  def createPBufferSurface(display: Disp, config: Cfg, attributes: PBufferAttributes): IO[Sfc] = log("createPBufferSurface")(egl.createPBufferSurface(display, config, attributes))

  private[kernel] def bindApi(api: API): IO[Unit] = log("bindApi")(egl.bindApi(api))  
  private[kernel] def createContext(display: Disp, config: Cfg, shareContext: Ctx, attributes: ContextAttributes): IO[Ctx] = log("createContext")(egl.createContext(display, config, shareContext, attributes))
}
