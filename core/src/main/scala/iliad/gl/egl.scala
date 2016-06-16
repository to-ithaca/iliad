package iliad
package gl

import cats.implicits._
import cats.free._
import cats.data._

case class Attributes[K <: IntConstant, V <: IntConstant](value: Map[K, Int Xor V]) {

  override def toString() = {
    val contents = value.toList.map { case (k, v) =>
      s"$k -> ${v.bimap(_.toString, _.toString).merge}"
    }.mkString(", ")
    s"Attributes($contents)"
  }

  def toArray: Array[Int] = {
    value.toList.foldLeft(List[Int](EGL_NONE.value)) { (b, a) =>
      a._2.map(_.value).merge :: a._1.value :: b
    }.reverse.toArray
  }
}

import CatsExtras._


trait EGLDSL[NDisp, NWin, Disp, Cfg, Sfc, Ctx] {

  type DSL[A] = Free[EGL[NDisp, NWin, Disp, Cfg, Sfc, Ctx, ?], A]

  private def fix[A](egl: EGL[NDisp, NWin, Disp, Cfg, Sfc, Ctx, A]): DSL[A] = egl.free
  private def ensure[A](dsl: DSL[A])(p: A => Boolean, err: => String): XorT[DSL, String, A] =
    XorT(dsl.map(a => if(p(a)) a.right[String] else err.left[A]))

  val getError: DSL[Int] = fix(EGLGetError)

  def config(dpy: Disp, attrs: Attributes[ConfigAttrib, ConfigAttribValue]): DSL[Option[Cfg]] = {
    fix(EGLChooseConfig(dpy, attrs, 1)).map { case (cfgs, count) =>
      if(count > 0) Some(cfgs.head) else None
    }
  }

  def property(dpy: Disp, property: DisplayProperty): DSL[String] =
    fix(EGLQueryString(dpy, property))

  val noContext: DSL[Ctx] = fix(EGL_NO_CONTEXT())
  val noDisplay: DSL[Disp] = fix(EGL_NO_DISPLAY())
  val defaultDisplay: DSL[Disp] = for {
    nd <- fix[NDisp](EGL_DEFAULT_DISPLAY())
    d <- fix[Disp](EGLGetDisplay(nd))
  } yield d  

  def context(dpy: Disp, cfg: Cfg, attrs: Attributes[ContextAttrib, ContextAttribValue]): DSL[Ctx] = for {
    nc <- noContext
    ctx <- fix(EGLCreateContext(dpy, cfg, nc, attrs))
  } yield ctx

  def swapBuffers(dpy: Disp, sfc: Sfc): DSL[Boolean] = fix(EGLSwapBuffers(dpy, sfc))
  def makeCurrent(dpy: Disp, draw: Sfc, read: Sfc, ctx: Ctx): DSL[Boolean]

  def initialize(ndpy: NDisp): DSL[String Xor Disp] = (for {
    nod <- noDisplay.liftT[XorT[?[_], String, ?]]
    dpy <- ensure(fix[Disp](EGLGetDisplay(ndpy)))(_ != nod, "could not get EGLDisplay from native display")
    _   <- fix[(Int, Int)](EGLInitialize(dpy)).liftT[XorT[?[_], String, ?]]
    _   <- ensure(fix[Boolean](EGLBindAPI(EGL_OPENGL_ES_API)))(identity, "unable to bind GLES API")
  } yield dpy).value

//  def attribute(display: cfg: Cfg): DSL[Attributes[ContextAttrib, ContextAttribValue]] = 
}

trait EGL[+NDisp, +NWin, +Disp, +Cfg, +Sfc, +Ctx, A]

case class EGLChooseConfig[Disp, Cfg](dpy: Disp, attrs: Attributes[ConfigAttrib, ConfigAttribValue], count: Int)
    extends EGL[Nothing, Nothing, Disp, Cfg, Nothing, Nothing, (List[Cfg], Int)]
case class EGLQueryString[Disp](dpy: Disp, name: DisplayProperty)
    extends EGL[Nothing, Nothing, Disp, Nothing, Nothing, Nothing, String]
case class EGLCreateContext[Disp, Cfg, Ctx](dpy: Disp, cfg: Cfg, ctx: Ctx, attrs: Attributes[ContextAttrib, ContextAttribValue])
    extends EGL[Nothing, Nothing, Disp, Cfg, Nothing, Ctx, Ctx]
case class EGLGetDisplay[NDisp, Disp](nDisp: NDisp)
    extends EGL[NDisp, Nothing, Disp, Nothing, Nothing, Nothing, Disp]
case class EGLInitialize[Disp](disp: Disp)
    extends EGL[Nothing, Nothing, Disp, Nothing, Nothing, Nothing, (Int, Int)]
case class EGLCreateWindowSurface[NWin, Disp, Cfg, Sfc](dpy: Disp, cfg: Cfg, win: NWin, attribs: Attributes[WindowAttrib, WindowAttribValue])
    extends EGL[Nothing, NWin, Disp, Cfg, Sfc, Nothing, Sfc]
case class EGLSwapBuffers[Disp, Sfc](dpy: Disp, sfc: Sfc)
    extends EGL[Nothing, Nothing, Disp, Nothing, Sfc, Nothing, Boolean]
case class EGLBindAPI(api: EGLAPI) extends EGL[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Boolean]
case class EGLMakeCurrent[Disp, Sfc, Ctx](dpy: Disp, draw: Sfc, read: Sfc, ctx: Ctx)
    extends EGL[Nothing, Nothing, Disp, Nothing, Sfc, Ctx, Boolean]
case object EGLGetError extends EGL[Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Int]
case class EGL_NO_CONTEXT[Ctx]() extends EGL[Nothing, Nothing, Nothing, Nothing, Nothing, Ctx, Ctx]
case class EGL_NO_DISPLAY[Disp]() extends EGL[Nothing, Nothing, Disp, Nothing, Nothing, Nothing, Disp]
case class EGL_DEFAULT_DISPLAY[NDisp]() extends EGL[NDisp, Nothing, Nothing, Nothing, Nothing, Nothing, NDisp]
