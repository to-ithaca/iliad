package iliad
package gl

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

sealed trait EGLError
case object EGLGetDisplayError extends EGLError
case object EGLBindAPIError extends EGLError
case class EGLCallFailedError(msg: String) extends EGLError
case object EGLSwapBuffersError extends EGLError
case object EGLMakeCurrentError extends EGLError
case class EGLCreateContextError(msg: String) extends EGLError
case class EGLCreateSurfaceError(msg: String) extends EGLError
case class EGLSwapIntervalError(msg: String) extends EGLError
case class EGLConfigError(msg: String) extends EGLError

import CatsExtra._

final class EGLPRG[NDisp, NWin, Disp, Cfg, Sfc, Ctx] {

  type DSL[A] = Free[EGL[NDisp, NWin, Disp, Cfg, Sfc, Ctx, ?], A]

  private def fix[A](egl: EGL[NDisp, NWin, Disp, Cfg, Sfc, Ctx, A]): DSL[A] =
    egl.free
  private def ensure[A, E](dsl: DSL[A])(p: A => Boolean,
                                        err: => E): XorT[DSL, E, A] =
    XorT(dsl.map(a => if (p(a)) a.right[E] else err.left[A]))

  val getError: DSL[Int] = fix(EGLGetError)

  def configAttrib(dpy: Disp, cfg: Cfg, attr: IntConfigAttrib): DSL[Int] =
    fix(EGLGetConfigAttrib(dpy, cfg, attr))

  private def configAttrib(
      dpy: Disp,
      cfg: Cfg,
      attr: EnumConfigAttrib,
      values: Set[ConfigAttribValue]): DSL[Int Xor ConfigAttribValue] = {
    fix[Int](EGLGetConfigAttrib(dpy, cfg, attr)).map { c =>
      Xor.fromOption(values.find(_.value == c), c)
    }
  }

  def configAttrib(dpy: Disp,
                   cfg: Cfg,
                   attr: EnumConfigAttrib): DSL[Int Xor ConfigAttribValue] =
    configAttrib(dpy, cfg, attr, SealedEnum.values[ConfigAttribValue])

  def configAttribs(
      dpy: Disp,
      cfg: Cfg): DSL[Attributes[ConfigAttrib, ConfigAttribValue]] = {
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
      dpy: Disp,
      attrs: Attributes[ConfigAttrib, ConfigAttribValue]): DSL[Option[Cfg]] = {
    fix(EGLChooseConfig(dpy, attrs, 1)).map(_.headOption)
  }

  def property(dpy: Disp, property: DisplayProperty): DSL[String] =
    fix(EGLQueryString(dpy, property))

  def properties(dpy: Disp): DSL[Map[DisplayProperty, String]] = {
    val ps = SealedEnum.values[DisplayProperty].toList
    ps.map(p => property(dpy, p).map(p -> _)).sequence.map(_.toMap)
  }

  val noContext: DSL[Ctx] = fix(EGL_NO_CONTEXT())
  val noDisplay: DSL[Disp] = fix(EGL_NO_DISPLAY())
  val noSurface: DSL[Sfc] = fix(EGL_NO_SURFACE())
  val defaultDisplay: DSL[NDisp] = fix(EGL_DEFAULT_DISPLAY())

  def display(nDisp: NDisp): DSL[EGLGetDisplayError.type Xor Disp] =
    (for {
      nd <- XorT.right(fix[Disp](EGL_NO_DISPLAY()))
      d <- ensure(fix[Disp](EGLGetDisplay(nDisp)))(dd =>
                dd != null && dd != nd, EGLGetDisplayError)
    } yield d).value

  def context(dpy: Disp,
              cfg: Cfg,
              attrs: Attributes[ContextAttrib, ContextAttribValue])
    : DSL[EGLCreateContextError Xor Ctx] =
    (for {
      nc <- XorT.right(noContext)
      ctx <- ensure(fix(EGLCreateContext(dpy, cfg, nc, attrs)))(
                c => c != null && c != nc,
                EGLCreateContextError(s"Failed with attributes $attrs"))
    } yield ctx).value

  def windowSurface(dpy: Disp,
                    cfg: Cfg,
                    nw: NWin,
                    attribs: Attributes[WindowAttrib, WindowAttribValue])
    : DSL[EGLCreateSurfaceError Xor Sfc] =
    (for {
      ns <- XorT.right(noSurface)
      sfc <- ensure(fix[Sfc](EGLCreateWindowSurface(dpy, cfg, nw, attribs)))(
                s => s != null && s != ns,
                EGLCreateSurfaceError(s"Failed with attributes: $attribs"))
    } yield sfc).value

  def swapBuffers(dpy: Disp,
                  sfc: Sfc): DSL[EGLSwapBuffersError.type Xor Boolean] =
    ensure(fix(EGLSwapBuffers(dpy, sfc)))(identity, EGLSwapBuffersError).value
  def makeCurrent(dpy: Disp,
                  draw: Sfc,
                  read: Sfc,
                  ctx: Ctx): DSL[EGLMakeCurrentError.type Xor Boolean] =
    ensure(fix(EGLMakeCurrent(dpy, draw, read, ctx)))(
        identity,
        EGLMakeCurrentError).value

  def initialise(ndpy: NDisp): DSL[EGLError Xor Disp] =
    (for {
      nod <- XorT.right(noDisplay)
      dpy <- XorT(display(ndpy)).leftWiden[EGLError]
      _ <- XorT.right[DSL, EGLError, (Int, Int)](fix(EGLInitialize(dpy)))
      _ <- ensure(fix[Boolean](EGLBindAPI(EGL_OPENGL_ES_API)))(
              identity,
              EGLBindAPIError).leftWiden[EGLError]
    } yield dpy).value

  def swapInterval(dpy: Disp,
                   interval: Int): DSL[EGLSwapIntervalError Xor Boolean] =
    ensure(fix(EGLSwapInterval(dpy, interval)))(
        identity,
        EGLSwapIntervalError(s"Failed to set interval to $interval")).value
}

trait EGL[+NDisp, +NWin, +Disp, +Cfg, +Sfc, +Ctx, A]

case class EGLChooseConfig[Disp, Cfg](
    dpy: Disp,
    attrs: Attributes[ConfigAttrib, ConfigAttribValue],
    count: Int)
    extends EGL[Nothing, Nothing, Disp, Cfg, Nothing, Nothing, List[Cfg]]
case class EGLQueryString[Disp](dpy: Disp, name: DisplayProperty)
    extends EGL[Nothing, Nothing, Disp, Nothing, Nothing, Nothing, String]
case class EGLCreateContext[Disp, Cfg, Ctx](
    dpy: Disp,
    cfg: Cfg,
    ctx: Ctx,
    attrs: Attributes[ContextAttrib, ContextAttribValue])
    extends EGL[Nothing, Nothing, Disp, Cfg, Nothing, Ctx, Ctx]
case class EGLGetDisplay[NDisp, Disp](nDisp: NDisp)
    extends EGL[NDisp, Nothing, Disp, Nothing, Nothing, Nothing, Disp]
case class EGLInitialize[Disp](disp: Disp)
    extends EGL[Nothing, Nothing, Disp, Nothing, Nothing, Nothing, (Int, Int)]
case class EGLCreateWindowSurface[NWin, Disp, Cfg, Sfc](
    dpy: Disp,
    cfg: Cfg,
    win: NWin,
    attribs: Attributes[WindowAttrib, WindowAttribValue])
    extends EGL[Nothing, NWin, Disp, Cfg, Sfc, Nothing, Sfc]
case class EGLSwapBuffers[Disp, Sfc](dpy: Disp, sfc: Sfc)
    extends EGL[Nothing, Nothing, Disp, Nothing, Sfc, Nothing, Boolean]
case class EGLBindAPI(api: EGLAPI)
    extends EGL[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Boolean]
case class EGLGetConfigAttrib[Disp, Cfg](disp: Disp,
                                         config: Cfg,
                                         attrib: ConfigAttrib)
    extends EGL[Nothing, Nothing, Disp, Cfg, Nothing, Nothing, Int]

case class EGLMakeCurrent[Disp, Sfc, Ctx](dpy: Disp,
                                          draw: Sfc,
                                          read: Sfc,
                                          ctx: Ctx)
    extends EGL[Nothing, Nothing, Disp, Nothing, Sfc, Ctx, Boolean]
case object EGLGetError
    extends EGL[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Int]

case class EGLSwapInterval[Disp](dpy: Disp, interval: Int)
    extends EGL[Nothing, Nothing, Disp, Nothing, Nothing, Nothing, Boolean]

case class EGL_NO_CONTEXT[Ctx]()
    extends EGL[Nothing, Nothing, Nothing, Nothing, Nothing, Ctx, Ctx]
case class EGL_NO_DISPLAY[Disp]()
    extends EGL[Nothing, Nothing, Disp, Nothing, Nothing, Nothing, Disp]
case class EGL_DEFAULT_DISPLAY[NDisp]()
    extends EGL[NDisp, Nothing, Nothing, Nothing, Nothing, Nothing, NDisp]
case class EGL_NO_SURFACE[Sfc]()
    extends EGL[Nothing, Nothing, Nothing, Nothing, Sfc, Nothing, Sfc]
