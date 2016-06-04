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

  private def error(method: String, value: Int, codes: Set[IntConstant]): String = codes.find(_.value == value) match {
    case Some(code) => s"call $method failed with error code $code"
    case None => s"call $method failed with undefined error code $value"
  }

  private def debug[A, C <: IntConstant](method: String)(io: FIO[A])(codes: Set[C]) = lift(io).flatMap(a => getError.mapF(_.flatMap { value =>
    val xor = if(value == EGL_SUCCESS.value) a.right
    else error(method, value, codes.map(identity(_))).left
    XorT.fromXor(xor)
  }))

  def getError = lift(egl.getError)
  def getConfigAttrib(display: Disp, config: Cfg, attribute: FixedConfigAttrib): IO[Int] = debug("getConfigAttrib")(egl.getConfigAttrib(display, config, attribute))(Set.empty)
  def queryString(dpy: Disp, name: QueryKey): IO[String] = debug("queryString")(egl.queryString(dpy, name))(Set.empty)
  def getDisplay(displayID: NDisp): IO[Disp] = debug("getDisplay")(egl.getDisplay(displayID))(Set.empty)

  def initialise(display: Disp): IO[(Int, Int)] = debug("initialise")(egl.initialise(display))(SealedEnum.values[DisplayErrorCode])
  def chooseConfig(display: Disp, attributes: ConfigAttributes): IO[Cfg] = debug("chooseConfig")(egl.chooseConfig(display, attributes))(SealedEnum.values[ChooseConfigErrorCode])
  def createWindowSurface(display: Disp, config: Cfg, win: NWin, attributes: WindowAttributes): IO[Sfc] = debug("createWindowSurface")(egl.createWindowSurface(display, config, win, attributes))(SealedEnum.values[WindowSurfaceErrorCode])
  def createPbufferSurface(display: Disp, config: Cfg, attributes: PBufferAttributes): IO[Sfc] = debug("createPBufferSurface")(egl.createPbufferSurface(display, config, attributes))(SealedEnum.values[PBufferSurfaceErrorCode])

  private[kernel] def bindApi(api: API): IO[Unit] = debug("bindApi")(egl.bindApi(api))(SealedEnum.values[APIErrorCode])
 
  private[kernel] def createContext(d: Disp, cfg: Cfg, ctx: Ctx, as: ContextAttributes): IO[Ctx] = debug("createContext")(egl.createContext(d, cfg, ctx, as))(SealedEnum.values[CreateContextErrorCode])
}
