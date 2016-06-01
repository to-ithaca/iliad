package iliad
package kernel
package egl

import scala.reflect._

import java.nio.IntBuffer

import cats._
import cats.data._

/** Runs EGL commands with the Id effect type */
class NoEffectsRunning[NDisp, NWin, Disp, Cfg: ClassTag, Sfc, Ctx] extends EGL[Id, NDisp, NWin, Disp, Cfg, Sfc, Ctx] {
  import EGL._
  import Constants._

  def getError: IO[Int] = Reader(egl => egl.eglGetError)

  def getDisplay(displayID: NDisp): IO[Disp] = Reader(_.eglGetDisplay(displayID))

  //TODO: add native intBuffer support
  def initialise(display: Disp): IO[(Int, Int)] = Reader { egl =>
    val major: IntBuffer = ???
    val minor: IntBuffer = ???
    egl.eglInitialize(display, major, minor)
    (major.get, minor.get)
  }

  def chooseConfig(display: Disp, attributes: ConfigAttributes): IO[Cfg] = Reader { egl => 
    val configs = new Array[Cfg](1)
    val numConfig: IntBuffer = ???
    egl.eglChooseConfig(display, attributes.toArray, configs, 1, numConfig)
    configs(0)
  }

  def createWindowSurface(display: Disp, config: Cfg, win: NWin): IO[Sfc] = Reader(_.eglCreateWindowSurface(display, config, win, Array(EGL_NONE.value)))

  def bindApi(api: API): IO[Unit] = Reader( _.eglBindAPI(api.value))

  def createContext(display: Disp, config: Cfg, shareContext: Ctx, attributes: ContextAttributes): IO[Ctx] = Reader( _.eglCreateContext(display, config, shareContext, attributes.toArray))
}
