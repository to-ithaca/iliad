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

  type FIO[A] = ReaderT[F, EGLLib, A]

  private def lift[A](io: FIO[A]): IO[A] = io.mapF{ fa => fa.liftT[EGL.Logger] }
  private def log[A](io: FIO[A])(s: String): IO[A] = lift(io).mapF {_.mapWritten(_ => List(s))}
  private def logOutput[A](io: FIO[A])(logf: A => String) = lift(io).mapF(_.mapBoth( (_, a) => (List(logf(a)), a)))

  def getError(): IO[Option[Int Xor ErrorCode]] = lift(egl.getError)
  def getConfigAttrib(display: Disp, config: Cfg, attribute: FixedConfigAttrib): IO[Int] = lift(egl.getConfigAttrib(display, config, attribute))
  
  override def getEnumConfigAttrib(display: Disp, config: Cfg, attribute: EnumConfigAttrib): IO[Int Xor ConfigAttribValue] = logOutput(egl.getEnumConfigAttrib(display, config, attribute))(v => v match {
    case Xor.Right(v) => s"eglGetConfigAttrib: [$attribute - $v]"
    case Xor.Left(i) =>  s"eglGetConfigAttrib: [$attribute - undefined enum $i]"
  })
  override def getIntConfigAttrib(display: Disp, config: Cfg, attribute: IntConfigAttrib): IO[Int] = logOutput(egl.getIntConfigAttrib(display, config, attribute))(i => s"eglGetConfigAttrib: [$attribute - $i]")

  def queryString(dpy: Disp, name: QueryKey): IO[String] = logOutput(egl.queryString(dpy, name))(result => s"eglQueryString: [$name - $result]")

  def getDisplay(displayId: NDisp): IO[Disp] = log(egl.getDisplay(displayId))("eglGetDisplay")

  def initialise(display: Disp): IO[(Int, Int)] = logOutput(egl.initialise(display))(vs => s"eglInitialize:  major version ${vs._1} minor version ${vs._2}").flatMap { vs => 
    val queries = SealedEnum.values[QueryKey].toList.map(q => queryString(display, q))
    implicitly[Traverse[List]].sequence(queries).mapF(_.map(_ => vs))
  }

  def chooseConfig(display: Disp, attributes: ConfigAttributes): IO[Cfg] = log(egl.chooseConfig(display, attributes))("eglChooseConfig").flatMap { cfg => 
    val enumValues = SealedEnum.values[EnumConfigAttrib].toList.map(a => getEnumConfigAttrib(display, cfg, a).map(_ => ()))
    val intValues = SealedEnum.values[IntConfigAttrib].toList.map(a => getIntConfigAttrib(display, cfg, a).map(_ => ()))
    val valueList = implicitly[Traverse[List]].sequence(enumValues ::: intValues)
    valueList.mapF(_.map( _ => cfg))
  }

  def createWindowSurface(display: Disp, config: Cfg, win: NWin, attributes: WindowAttributes): IO[Sfc] = log(egl.createWindowSurface(display, config, win, attributes))("eglCreateWindowSurface")
  def createPbufferSurface(display: Disp, config: Cfg, attributes: PBufferAttributes): IO[Sfc] = log(egl.createPbufferSurface(display, config, attributes))("eglCreatePbufferSurface")

  private[kernel] def bindApi(api: API): IO[Unit] = log(egl.bindApi(api))("eglBindApi")  
  private[kernel] def createContext(display: Disp, config: Cfg, shareContext: Ctx, attributes: ContextAttributes): IO[Ctx] = log(egl.createContext(display, config, shareContext, attributes))("eglCreateContext")

  def swapBuffers(display: Disp, surface: Sfc): IO[Unit] = log(egl.swapBuffers(display, surface))("eglSwapBuffers")
  def makeCurrent(display: Disp, draw: Sfc, read: Sfc, context: Ctx): IO[Unit] = log(egl.makeCurrent(display, draw, read, context))("eglMakeCurrent")
}
