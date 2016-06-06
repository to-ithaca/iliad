package iliad
package kernel

import iliad.kernel.platform.EGL14Library

import scala.reflect._

import cats._
import cats.data._
import cats.implicits._

import EGLConstants._

object EGL {

  /** Effect type of Debugging runner */
  type DebugEffect[F[_], A] = XorT[F, String, A]

  /** Effect type of Logging runner */
  type LogEffect[F[_], A] = WriterT[F, List[String], A]

  /** Major and minor EGL versions for the display */
  case class DisplayVersion(major: Int, minor: Int)


  /** Attribute constructors */
  def configValue(i: Int): Int Xor ConfigAttribValue = i.left
  def configValue(i: ConfigAttribValue): Int Xor ConfigAttribValue = i.right
  def contextValue(i: Int): Int Xor ContextAttribValue = i.left
  def pbufferValue(i: Int): Int Xor PBufferAttribValue = i.left
  def pbufferValue(i: PBufferAttribValue): Int Xor PBufferAttribValue = i.right

  case class Attributes[K <: IntConstant, V <: IntConstant](attributes: Map[K, Int Xor V]) {
    private def attribList(as: Map[IntConstant, Xor[Int, IntConstant]]): Array[Int] = {
      (as.flatMap { case (key, xor) =>  Seq(key.value, xor.map(_.value).merge) }.toSeq :+ EGL_NONE.value).toArray
    }

    /** Produces an array of the form key, value, key, value ... EGL_NONE */
    def toArray: Array[Int] = attribList(attributes.map(identity))
  }

  type ConfigAttributes = Attributes[ConfigAttrib, ConfigAttribValue]
  type ContextAttributes = Attributes[ContextAttrib, ContextAttribValue]
  type WindowAttributes = Attributes[WindowAttrib, WindowAttribValue]
  type PBufferAttributes = Attributes[PBufferAttrib, PBufferAttribValue]

  /** Session returned by context creation */
  case class Session[Disp, Cfg, Sfc, Ctx](display: Disp, config: Cfg, surface: Sfc, context: Ctx)

  /** EGL runners */
  def runner[NDisp, NWin, Disp, Cfg: ClassTag, Sfc, Ctx]: EGL[Id, NDisp, NWin, Disp, Cfg, Sfc, Ctx] = new EGLRunner[NDisp, NWin, Disp, Cfg, Sfc, Ctx]()
  def debugging[NDisp, NWin, Disp, Cfg: ClassTag, Sfc, Ctx]: EGL[DebugEffect[Id, ?], NDisp, NWin, Disp, Cfg, Sfc, Ctx] = new EGLDebugger(runner[NDisp, NWin, Disp, Cfg, Sfc, Ctx])
  def logging[NDisp, NWin, Disp, Cfg: ClassTag, Sfc, Ctx]: EGL[LogEffect[Id, ?], NDisp, NWin, Disp, Cfg, Sfc, Ctx] = new EGLLogger(runner[NDisp, NWin, Disp, Cfg, Sfc, Ctx])
  def debuggingLogging[NDisp, NWin, Disp, Cfg: ClassTag, Sfc, Ctx]: EGL[DebugEffect[LogEffect[Id, ?], ?], NDisp, NWin, Disp, Cfg, Sfc, Ctx] = new EGLDebugger(new EGLLogger(new EGLRunner[NDisp, NWin, Disp, Cfg, Sfc, Ctx]()))
}

/** EGL API to be implemented by EGL runners.
  * 
  * Type parameters are mapped as follows:
  *   NDisp corresponds to [[iliad.kernel.platform.EGL14Library.EGLNativeDisplayType]]
  *   NWin  corresponds to [[iliad.kernel.platform.EGL14Library.EGLNativeWindowType]]
  *   Disp  corresponds to [[iliad.kernel.platform.EGL14Library.EGLDisplay]]
  *   Cfg   corresponds to [[iliad.kernel.platform.EGL14Library.EGLConfig]]
  *   Sfc   corresponds to [[iliad.kernel.platform.EGL14Library.EGLSurface]]
  *   Ctx   corresponds to [[iliad.kernel.platform.EGL14Library.EGLContext]]
  */
abstract class EGL[F[_]: Monad, NDisp, NWin, Disp, Cfg : ClassTag, Sfc, Ctx] {
  import EGL._

  type EGLLib = EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx]
  type IO[A] = ReaderT[F, EGLLib, A]

  def getError: IO[Option[Int Xor ErrorCode]]
  private[kernel] def getConfigAttrib(display: Disp, config: Cfg, attribute: FixedConfigAttrib): IO[Int]
  def getEnumConfigAttrib(display: Disp, config: Cfg, attribute: EnumConfigAttrib): IO[Int Xor ConfigAttribValue] = getConfigAttrib(display, config, attribute).map( v => 
    Xor.fromOption(SealedEnum.values[ConfigAttribValue].find(_.value == v), v)
  )
  def getIntConfigAttrib(display: Disp, config: Cfg, attribute: IntConfigAttrib): IO[Int] = getConfigAttrib(display, config, attribute)

  def queryString(dpy: Disp, name: QueryKey): IO[String]

  def getDisplay(displayID: NDisp): IO[Disp]
  private[kernel] def initialise(display: Disp): IO[(Int, Int)]
  def initialisedVersion(display: Disp): IO[DisplayVersion] = initialise(display).map(t => DisplayVersion(t._1, t._2))

  def chooseConfig(display: Disp, attributes: ConfigAttributes): IO[Cfg]
  def createWindowSurface(display: Disp, config: Cfg, win: NWin, attributes: WindowAttributes): IO[Sfc]
  def createPbufferSurface(display: Disp, config: Cfg, attributes: PBufferAttributes): IO[Sfc]
  
  private[kernel] def bindApi(api: API): IO[Unit]
  def bindGL: IO[Unit] = bindApi(EGL_OPENGL_ES_API)
  
  private[kernel] def createContext(display: Disp, config: Cfg, shareContext: Ctx, attributes: ContextAttributes): IO[Ctx]
  def primaryContext(display: Disp, config: Cfg, noContext: Ctx, attributes: ContextAttributes): IO[Ctx] = createContext(display, config, noContext, attributes)
  def secondaryContext(display: Disp, config: Cfg, primaryContext: Ctx, attributes: ContextAttributes): IO[Ctx] = createContext(display, config, primaryContext, attributes)


  def swapBuffers(display: Disp, surface: Sfc): IO[Unit]
  def makeCurrent(display: Disp, draw: Sfc, read: Sfc, context: Ctx): IO[Unit]

  def setupPrimaryContext(displayID: NDisp, windowID: NWin, noContext: Ctx): IO[Session[Disp, Cfg, Sfc, Ctx]] = for {
    display <- getDisplay(displayID)
    _ <- initialisedVersion(display)
    config <- chooseConfig(display, EGLDefaults.primaryConfig)
    surface <- createWindowSurface(display, config, windowID, EGLDefaults.window)
    _ <- bindGL
    context <- primaryContext(display, config, noContext, EGLDefaults.context)
    _ <- makeCurrent(display, surface, surface, context)
  } yield Session(display, config, surface, context)

  def setupSecondaryContext(session: Session[Disp, Cfg, Sfc, Ctx]): IO[Ctx] = for {
    _ <- initialisedVersion(session.display)
    config <- chooseConfig(session.display, EGLDefaults.secondaryConfig)
    surface <- createPbufferSurface(session.display, config, EGLDefaults.pBuffer)
    _ <- bindGL
    context <- secondaryContext(session.display, config, session.context, EGLDefaults.context)
  } yield context
}

object EGLDefaults {
  import EGL._

  val primaryConfig: ConfigAttributes = Attributes[ConfigAttrib, ConfigAttribValue](Map(
    EGL_LEVEL -> configValue(0),
    EGL_SURFACE_TYPE -> configValue(EGL_WINDOW_BIT),
    EGL_RENDERABLE_TYPE -> configValue(EGL_OPENGL_ES3_BIT),
    EGL_BLUE_SIZE -> configValue(8),
    EGL_GREEN_SIZE -> configValue(8),
    EGL_RED_SIZE -> configValue(8),
    EGL_ALPHA_SIZE -> configValue(8),
    EGL_BUFFER_SIZE -> configValue(32),
    EGL_DEPTH_SIZE -> configValue(24)
  ))

  val context: ContextAttributes = Attributes[ContextAttrib, ContextAttribValue](Map(
    EGL_CONTEXT_CLIENT_VERSION -> contextValue(3)
  ))

  val window: WindowAttributes = Attributes[WindowAttrib, WindowAttribValue](Map.empty)

  val secondaryConfig: ConfigAttributes = Attributes[ConfigAttrib, ConfigAttribValue](Map(
    EGL_SAMPLES -> configValue(1),
    EGL_SURFACE_TYPE -> configValue(EGL_PBUFFER_BIT),
    EGL_RENDERABLE_TYPE -> configValue(EGL_OPENGL_ES3_BIT),
    EGL_BLUE_SIZE -> configValue(8),
    EGL_GREEN_SIZE -> configValue(8),
    EGL_RED_SIZE -> configValue(8),
    EGL_ALPHA_SIZE -> configValue(0),
    EGL_BUFFER_SIZE -> configValue(32),
    EGL_DEPTH_SIZE -> configValue(16)
  ))
 
  val pBuffer: PBufferAttributes = Attributes[PBufferAttrib, PBufferAttribValue](Map(
    EGL_WIDTH -> pbufferValue(1),
    EGL_HEIGHT -> pbufferValue(1),
    EGL_TEXTURE_FORMAT -> pbufferValue(EGL_TEXTURE_RGBA),
    EGL_TEXTURE_TARGET -> pbufferValue(EGL_TEXTURE_2D)
  ))
}
