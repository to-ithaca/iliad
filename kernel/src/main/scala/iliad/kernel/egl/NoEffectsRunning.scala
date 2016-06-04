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
  def getConfigAttrib(display: Disp, config: Cfg, attribute: FixedConfigAttrib): IO[Int] = Reader { egl => 
    val buf = Buffer.capacity[Int](1)
    egl.eglGetConfigAttrib(display, config, attribute.value, buf)
    buf.get
  }
  def queryString(dpy: Disp, name: QueryKey): IO[String] = Reader(egl => egl.eglQueryString(dpy, name.value))

  def getDisplay(displayID: NDisp): IO[Disp] = Reader(_.eglGetDisplay(displayID))

  def initialise(display: Disp): IO[(Int, Int)] = Reader { egl =>
    val major: IntBuffer = Buffer.capacity[Int](1)
    val minor: IntBuffer = Buffer.capacity[Int](1)
    egl.eglInitialize(display, major, minor)
    (major.get, minor.get)
  }

  def chooseConfig(display: Disp, attributes: ConfigAttributes): IO[Cfg] = Reader { egl => 
    val configs = new Array[Cfg](1)
    val numConfig: IntBuffer = Buffer.capacity[Int](1)
    egl.eglChooseConfig(display, attributes.toArray, configs, 1, numConfig)
    configs(0)
  }

  def createWindowSurface(display: Disp, config: Cfg, win: NWin, attributes: WindowAttributes): IO[Sfc] = Reader(_.eglCreateWindowSurface(display, config, win, attributes.toArray))
  def createPbufferSurface(display: Disp, config: Cfg, attributes: PBufferAttributes): IO[Sfc] = Reader(_.eglCreatePbufferSurface(display, config, attributes.toArray))
  def bindApi(api: API): IO[Unit] = Reader( _.eglBindAPI(api.value))

  def createContext(display: Disp, config: Cfg, shareContext: Ctx, attributes: ContextAttributes): IO[Ctx] = Reader( _.eglCreateContext(display, config, shareContext, attributes.toArray))
}
