package iliad
package gl

import cats._
import cats.implicits._
import cats.free._
import cats.data._

case class Attributes[K <: IntConstant, V <: IntConstant](
    values: Map[K, Int Xor V]) {

  override def toString(): String = {
    val contents = values.toList.map {
      case (k, v) =>
        s"$k -> ${v.bimap(_.toString, _.toString).merge}"
    }.mkString(", ")
    s"Attributes($contents)"
  }

  def toArray: Array[Int] = {
    values.toList
      .foldLeft(List[Int](EGL_NONE.value)) { (b, a) =>
        a._1.value :: a._2.map(_.value).merge :: b
      }
      .toArray
  }
}

object Attributes {
  def apply[K <: IntConstant, V <: IntConstant](
      kvs: (K, Int Xor V)*): Attributes[K, V] =
    Attributes(Map(kvs: _*))
  def empty[K <: IntConstant, V <: IntConstant]: Attributes[K, V] = apply()
}

object EGL {

  type DSL[A] = Free[EGL, A]

  def logInterpreter: EGL ~> ReaderT[Xor[EGLError, ?], EGL14.type, ?] =
    new EGLDebugInterpreter(
      EGLInterpreter.compose(new EffectfulLogBefore[EGL](_.toString))
        .andThen(new EffectfulLogAfter))

  private def ensure[A, E](dsl: DSL[A])(p: A => Boolean,
                                        err: => E): XorT[DSL, E, A] =
    XorT(dsl.map(a => if (p(a)) a.right[E] else err.left[A]))

  val getError: DSL[Int] = EGLGetError.free

  def configAttrib(dpy: EGL14.EGLDisplay, cfg: EGL14.EGLConfig, attr: IntConfigAttrib): DSL[Int] =
    EGLGetConfigAttrib(dpy, cfg, attr).free

  private def configAttrib(
      dpy: EGL14.EGLDisplay,
      cfg: EGL14.EGLConfig,
      attr: EnumConfigAttrib,
      values: Set[ConfigAttribValue]): DSL[Int Xor ConfigAttribValue] =
    EGLGetConfigAttrib(dpy, cfg, attr).free.map { c =>
      Xor.fromOption(values.find(_.value == c), c)
    }
  

  def configAttrib(dpy: EGL14.EGLDisplay,
                   cfg: EGL14.EGLConfig,
                   attr: EnumConfigAttrib): DSL[Int Xor ConfigAttribValue] =
    configAttrib(dpy, cfg, attr, SealedEnum.values[ConfigAttribValue])

  def configAttribs(
      dpy: EGL14.EGLDisplay,
      cfg: EGL14.EGLConfig): DSL[Attributes[ConfigAttrib, ConfigAttribValue]] = {
    val is = SealedEnum
      .values[IntConfigAttrib]
      .toList
      .map(a =>
            configAttrib(dpy, cfg, a).map(
                (a: ConfigAttrib) -> _.left[ConfigAttribValue]))

    //values so that we don't create lists again and again
    val values = SealedEnum.values[ConfigAttribValue]
    val es = SealedEnum
      .values[EnumConfigAttrib]
      .toList
      .map(a => configAttrib(dpy, cfg, a, values).map((a: ConfigAttrib) -> _))
    (is ++ es).sequence.map(as => Attributes(as.toMap))
  }

  def config(
      dpy: EGL14.EGLDisplay,
      attrs: Attributes[ConfigAttrib, ConfigAttribValue]): DSL[Option[EGL14.EGLConfig]] = {
    EGLChooseConfig(dpy, attrs, 1).free.map(_.headOption)
  }

  def property(dpy: EGL14.EGLDisplay, property: DisplayProperty): DSL[String] =
    EGLQueryString(dpy, property).free

  def properties(dpy: EGL14.EGLDisplay): DSL[Map[DisplayProperty, String]] = {
    val ps = SealedEnum.values[DisplayProperty].toList
    ps.map(p => property(dpy, p).map(p -> _)).sequence.map(_.toMap)
  }

  def display(nDisp: EGL14.EGLNativeDisplayType): DSL[EGLGetDisplayError.type Xor EGL14.EGLDisplay] =
    (ensure(EGLGetDisplay(nDisp).free)(dd =>
                dd != null && dd != EGL14.EGL_NO_DISPLAY, EGLGetDisplayError)
    ).value

  def context(dpy: EGL14.EGLDisplay,
              cfg: EGL14.EGLConfig,
              attrs: Attributes[ContextAttrib, ContextAttribValue])
    : DSL[EGLCreateContextError Xor EGL14.EGLContext] =
    (ensure(EGLCreateContext(dpy, cfg, EGL14.EGL_NO_CONTEXT, attrs).free)(c =>
                  c != null && c != EGL14.EGL_NO_CONTEXT, EGLCreateContextError(attrs))
    ).value
  def windowSurface(dpy: EGL14.EGLDisplay,
                    cfg: EGL14.EGLConfig,
                    nw: EGL14.EGLNativeWindowType,
                    attribs: Attributes[WindowAttrib, WindowAttribValue])
    : DSL[EGLCreateSurfaceError Xor EGL14.EGLSurface] =
    (ensure(EGLCreateWindowSurface(dpy, cfg, nw, attribs).free)(
                s => s != null && s != EGL14.EGL_NO_SURFACE,
                EGLCreateSurfaceError(attribs))
    ).value

  def pbufferSurface(dpy: EGL14.EGLDisplay, cfg: EGL14.EGLConfig, attribs: Attributes[PBufferAttrib, PBufferAttribValue]): DSL[EGLCreatePBufferSurfaceError Xor EGL14.EGLSurface] =
    (ensure(EGLCreatePBufferSurface(dpy, cfg, attribs).free)(
                s => s != null && s != EGL14.EGL_NO_SURFACE,
                EGLCreatePBufferSurfaceError(attribs))
    ).value


  def swapBuffers(dpy: EGL14.EGLDisplay,
                  sfc: EGL14.EGLSurface): DSL[EGLSwapBuffersError.type Xor Boolean] =
    ensure(EGLSwapBuffers(dpy, sfc).free)(identity, EGLSwapBuffersError).value
  def makeCurrent(dpy: EGL14.EGLDisplay,
                  draw: EGL14.EGLSurface,
                  read: EGL14.EGLSurface,
                  ctx: EGL14.EGLContext): DSL[EGLMakeCurrentError.type Xor Boolean] =
    ensure(EGLMakeCurrent(dpy, draw, read, ctx).free)(
        identity,
        EGLMakeCurrentError).value

  def initialise(ndpy: EGL14.EGLNativeDisplayType): DSL[EGLError Xor EGL14.EGLDisplay] =
    (for {
      dpy <- XorT(display(ndpy)).leftWiden[EGLError]
      _ <- XorT.right[DSL, EGLError, (Int, Int)](EGLInitialize(dpy).free)
      _ <- ensure(EGLBindAPI(EGL_OPENGL_ES_API).free)(
              identity,
              EGLBindAPIError).leftWiden[EGLError]
    } yield dpy).value

  def swapInterval(dpy: EGL14.EGLDisplay,
                   interval: Int): DSL[EGLSwapIntervalError Xor Boolean] =
    ensure(EGLSwapInterval(dpy, interval).free)(
        identity,
        EGLSwapIntervalError(interval)).value

  def destroySurface(dpy: EGL14.EGLDisplay, sfc: EGL14.EGLSurface): DSL[EGLDestroySurfaceError Xor Boolean] =
    ensure(EGLDestroySurface(dpy, sfc).free)(
        identity,
      EGLDestroySurfaceError(dpy, sfc)).value

  def destroyContext(dpy: EGL14.EGLDisplay, ctx: EGL14.EGLContext): DSL[EGLDestroyContextError Xor Boolean] =
    ensure(EGLDestroyContext(dpy, ctx).free)(
        identity,
      EGLDestroyContextError(dpy, ctx)).value
}

trait EGL[A]

case class EGLChooseConfig(
    dpy: EGL14.EGLDisplay,
    attrs: Attributes[ConfigAttrib, ConfigAttribValue],
    count: Int)
    extends EGL[List[EGL14.EGLConfig]]
case class EGLQueryString(dpy: EGL14.EGLDisplay, name: DisplayProperty) extends EGL[String]
case class EGLCreateContext(
    dpy: EGL14.EGLDisplay,
    cfg: EGL14.EGLConfig,
    ctx: EGL14.EGLContext,
    attrs: Attributes[ContextAttrib, ContextAttribValue])
    extends EGL[EGL14.EGLContext]
case class EGLGetDisplay(nDisp: EGL14.EGLNativeDisplayType)
    extends EGL[EGL14.EGLDisplay]
case class EGLInitialize(disp: EGL14.EGLDisplay)
    extends EGL[(Int, Int)]
case class EGLCreateWindowSurface(
    dpy: EGL14.EGLDisplay,
    cfg: EGL14.EGLConfig,
    win: EGL14.EGLNativeWindowType,
    attribs: Attributes[WindowAttrib, WindowAttribValue])
    extends EGL[EGL14.EGLSurface]
case class EGLCreatePBufferSurface(dpy: EGL14.EGLDisplay, cfg: EGL14.EGLConfig, 
  attribs: Attributes[PBufferAttrib, PBufferAttribValue]) extends EGL[EGL14.EGLSurface]
case class EGLSwapBuffers(dpy: EGL14.EGLDisplay, sfc: EGL14.EGLSurface)
    extends EGL[Boolean]
case class EGLBindAPI(api: EGLAPI) extends EGL[Boolean]
case class EGLGetConfigAttrib(disp: EGL14.EGLDisplay,
                                         config: EGL14.EGLConfig,
                                         attrib: ConfigAttrib)
    extends EGL[Int]
case class EGLMakeCurrent(dpy: EGL14.EGLDisplay,
                                          draw: EGL14.EGLSurface,
                                          read: EGL14.EGLSurface,
                                          ctx: EGL14.EGLContext) extends EGL[Boolean]
case object EGLGetError extends EGL[Int]
case class EGLSwapInterval(dpy: EGL14.EGLDisplay, interval: Int) extends EGL[Boolean]
case class EGLDestroyContext(dpy: EGL14.EGLDisplay, ctx: EGL14.EGLContext) extends EGL[Boolean]
case class EGLDestroySurface(dpy: EGL14.EGLDisplay, sfc: EGL14.EGLSurface) extends EGL[Boolean]
