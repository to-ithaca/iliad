package iliad
package kernel

import scala.reflect._

import cats._
import cats.data._
import cats.implicits._
import cats.std._
import cats.std.option._

import EGLConstants._

/** Runs EGL commands with the Debugger (XorT) effect type
  * 
  *  Calls eglGetError after each command
  *  Terminates if an error is detected
  */
class EGLDebugger[F[_], NDisp, NWin, Disp, Cfg: ClassTag, Sfc, Ctx](
    val egl: EGL[F, NDisp, NWin, Disp, Cfg, Sfc, Ctx])(
    implicit val M: Monad[F])
    extends EGL[EGL.DebugEffect[F, ?], NDisp, NWin, Disp, Cfg, Sfc, Ctx] {
  import EGL._

  type FIO[A] = ReaderT[F, EGLLib, A]

  private def lift[A](io: FIO[A]): IO[A] = io.mapF { fa =>
    fa.liftT[EGL.DebugEffect]
  }

  private def errorText(method: String, code: Int Xor ErrorCode): String =
    code match {
      case Xor.Right(code) => s"call $method failed with error code $code"
      case Xor.Left(v) => s"call $method failed with undefined error code $v"
    }

  private def debug[A](method: String)(io: FIO[A]) =
    lift(io).flatMap(a =>
          getError.mapF(_.flatMap { code =>
        val xor = code.map(c => errorText(method, c)).toLeftXor(a)
        XorT.fromXor(xor)
      }))

  def getError: IO[Option[Int Xor ErrorCode]] = lift(egl.getError)
  def getConfigAttrib(
      display: Disp, config: Cfg, attribute: FixedConfigAttrib): IO[Int] =
    debug("getConfigAttrib")(egl.getConfigAttrib(display, config, attribute))
  def queryString(dpy: Disp, name: QueryKey): IO[String] =
    debug("queryString")(egl.queryString(dpy, name))
  def getDisplay(displayID: NDisp): IO[Disp] =
    debug("getDisplay")(egl.getDisplay(displayID))

  def initialise(display: Disp): IO[(Int, Int)] =
    debug("initialise")(egl.initialise(display))
  def chooseConfig(display: Disp, attributes: ConfigAttributes): IO[Cfg] =
    debug("chooseConfig")(egl.chooseConfig(display, attributes))
  def createWindowSurface(display: Disp,
                          config: Cfg,
                          win: NWin,
                          attributes: WindowAttributes): IO[Sfc] =
    debug("createWindowSurface")(
        egl.createWindowSurface(display, config, win, attributes))
  def createPbufferSurface(
      display: Disp, config: Cfg, attributes: PBufferAttributes): IO[Sfc] =
    debug("createPBufferSurface")(
        egl.createPbufferSurface(display, config, attributes))

  private[kernel] def bindApi(api: API): IO[Unit] =
    debug("bindApi")(egl.bindApi(api))

  private[kernel] def createContext(
      d: Disp, cfg: Cfg, ctx: Ctx, as: ContextAttributes): IO[Ctx] =
    debug("createContext")(egl.createContext(d, cfg, ctx, as))

  //TODO: enum error codes
  def swapBuffers(display: Disp, surface: Sfc): IO[Unit] =
    debug("swapBuffers")(egl.swapBuffers(display, surface))
  def makeCurrent(
      display: Disp, draw: Sfc, read: Sfc, context: Ctx): IO[Unit] =
    debug("makeCurrent")(egl.makeCurrent(display, draw, read, context))
}
