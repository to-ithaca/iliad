package iliad
package kernel
package egl

import iliad.kernel.platform.egl.Lib

import scala.reflect._

import cats._
import cats.data._

object EGL {

  /** Effect type of Debugging runner */
  type Debugger[F[_], A] = XorT[F, String, A]

  /** Effect type of Logging runner */
  type Logger[F[_], A] = WriterT[F, List[String], A]

  /** Major and minor EGL versions for the display */
  case class DisplayVersion(major: Int, minor: Int)

  private def attribList(as: Map[Constants.IntConstant, Xor[Int, Constants.IntConstant]]): Array[Int] = {
    (as.flatMap { case (key, xor) =>  Seq(key.value, xor.map(_.value).merge) }.toSeq :+ Constants.EGL_NONE.value).toArray
  }

  case class ConfigAttributes(attributes: Map[Constants.ConfigAttrib, Xor[Int, Constants.ConfigAttribValue]]) {
    /** Produces an array of the form key, value, key, value ... EGL_NONE */
    def toArray: Array[Int] = attribList(attributes.map(identity)) 
  }

  case class ContextAttributes(attributes: Map[Constants.ContextAttrib, Xor[Int, Constants.ContextAttribValue]]) {
    /** Produces an array of the form key, value, key, value ... EGL_NONE */
    def toArray: Array[Int] = attribList(attributes.map(identity))
  }
}

/** EGL API to be implemented by EGL runners.
  * 
  * 
  * [[NDisp]] corresponds to [[Lib.EGLNativeDisplayType]]
  * [[NWin]] correponds to [[Lib.EGLNativeWindowType]]
  * [[Disp]] corresponds to [[Lib.EGLDisplay]]
  * [[Cfg]] corresponds to [[Lib.EGLConfig]]
  * [[Sfc]] corresponds to [[Lib.EGLSurface]]
  * [[Ctx]] corresponds to [[Lib.EGLContext]]
  */
abstract class EGL[F[_]: Monad, NDisp, NWin, Disp, Cfg : ClassTag, Sfc, Ctx] {
  import EGL._
  import Constants._

  type EGLLib = Lib.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx]
  type IO[A] = ReaderT[F, EGLLib, A]

  def getError: IO[Int]

  def getDisplay(displayID: NDisp): IO[Disp]
  private[egl] def initialise(display: Disp): IO[(Int, Int)]
  def initialisedVersion(display: Disp): IO[DisplayVersion] = initialise(display).map(t => DisplayVersion(t._1, t._2))

  def chooseConfig(display: Disp, attributes: ConfigAttributes): IO[Cfg]
  def createWindowSurface(display: Disp, config: Cfg, win: NWin): IO[Sfc]
  
  private[egl] def bindApi(api: API): IO[Unit]
  def bindOpenGLESApi: IO[Unit] = bindApi(EGL_OPENGL_ES_API)
  
  private[egl] def createContext(display: Disp, config: Cfg, shareContext: Ctx, attributes: ContextAttributes): IO[Ctx]
  def createSingleContext(display: Disp, config: Cfg, noContext: Ctx, attributes: ContextAttributes): IO[Ctx] = createContext(display, config, noContext, attributes)
  def createSharedContext(display: Disp, config: Cfg, primaryContext: Ctx, attributes: ContextAttributes): IO[Ctx] = createContext(display, config, primaryContext, attributes)
}
