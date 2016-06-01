package iliad
package kernel
package egl

import scala.reflect._

import cats._
import cats.data._
import cats.implicits._
import cats.std._

/** Runs EGL commands with the Debugger (XorT) effect type
  * 
  *  Calls eglGetError after each command
  *  Terminates if an error is detected
  */
class Debugging[F[_], NDisp, NWin, Disp, Cfg: ClassTag, Sfc, Ctx](val egl: EGL[F, NDisp, NWin, Disp, Cfg, Sfc, Ctx])(implicit val M: Monad[F]) extends EGL[EGL.Debugger[F, ?], NDisp, NWin, Disp, Cfg, Sfc, Ctx] {
  import EGL._
  import Constants._

  type FIO[A] = ReaderT[F, EGLLib, A]

  private def lift[A](io: FIO[A]): IO[A] = io.mapF{ fa => fa.liftT[EGL.Debugger]}

  //TODO: add marco expansion for error codes
  private def debug[A](method: String)(io: FIO[A]) = lift(io).flatMap(a => getError.mapF(_.flatMap { code =>
    val xor = if(code == EGL_SUCCESS.value) a.right
    else s"$method failed with error code $code".left
    XorT.fromXor(xor)
  }))

  def getError = lift(egl.getError)

  def getDisplay(displayID: NDisp): IO[Disp] = debug("getDisplay")(egl.getDisplay(displayID))

  def initialise(display: Disp): IO[(Int, Int)] = debug("initialise")(egl.initialise(display))
  def chooseConfig(display: Disp, attributes: ConfigAttributes): IO[Cfg] = debug("chooseConfig")(egl.chooseConfig(display, attributes))
  def createWindowSurface(display: Disp, config: Cfg, win: NWin): IO[Sfc] = debug("createWindowSurface")(egl.createWindowSurface(display, config, win))
  
  private[kernel] def bindApi(api: API): IO[Unit] = debug("bindApi")(egl.bindApi(api))
 
  private[kernel] def createContext(display: Disp, config: Cfg, shareContext: Ctx, attributes: ContextAttributes): IO[Ctx] = debug("createContext")(egl.createContext(display, config, shareContext, attributes)) 
}
