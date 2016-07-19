package iliad
package gl

import scala.reflect.ClassTag

import iliad.kernel.platform.EGL14Library

import cats._
import cats.data._
import cats.implicits._

import com.typesafe.scalalogging._

object EGLInterpreter {

  def logInterpreter[NDisp, NWin, Disp, Cfg, Sfc, Ctx](
      implicit ct: ClassTag[Cfg])
    : EGL[NDisp, NWin, Disp, Cfg, Sfc, Ctx, ?] ~> ReaderT[
        Xor[EGLError, ?],
        EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
        ?] =
    new EGLDebugInterpreter(
        new EGLInterpreter[NDisp, NWin, Disp, Cfg, Sfc, Ctx]
          .compose(
              new EffectfulLogBefore[EGL[NDisp, NWin, Disp, Cfg, Sfc, Ctx, ?]](
                  _.toString))
          .andThen(new EffectfulLogAfter))
}

final class EffectfulLogBefore[F[_]](showK: F[_] => String)
    extends LazyLogging
    with (F ~> F) {
  def apply[A](fa: F[A]): F[A] = {
    logger.debug(showK(fa))
    fa
  }
}

final class EffectfulLogAfter[F[_]: Functor]
    extends LazyLogging
    with (F ~> F) {
  def apply[A](fa: F[A]): F[A] = fa.map { a =>
    logger.debug(s"result $a")
    a
  }
}

final class EGLDebugInterpreter[NDisp, NWin, Disp, Cfg: ClassTag, Sfc, Ctx](
    interpret: EGL[NDisp, NWin, Disp, Cfg, Sfc, Ctx, ?] ~> Reader[
        EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
        ?])
    extends (EGL[NDisp, NWin, Disp, Cfg, Sfc, Ctx, ?] ~> ReaderT[
        Xor[EGLError, ?],
        EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
        ?]) {

  private val lift: Id ~> Xor[EGLError, ?] = new (Id ~> Xor[EGLError, ?]) {
    def apply[A](fa: Id[A]): Xor[EGLError, A] = fa.right
  }

  private val _errorCodes: Set[EGLErrorCode] = SealedEnum.values[EGLErrorCode]

  private def onError(method: String)(value: Int): EGLError Xor Unit =
    if (value == EGL_SUCCESS.value) ().right
    else
      _errorCodes.find(_.value == value) match {
        case Some(code) => EGLCallFailedError(method, code).left
        case None => EGLCallFailedUnknownError(method, value).left
      }

  private def debug(method: String)
    : ReaderT[Xor[EGLError, ?],
              EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
              Unit] =
    interpret(EGLGetError).transform(lift).mapF(_.flatMap(onError(method)))

  def apply[A](fa: EGL[NDisp, NWin, Disp, Cfg, Sfc, Ctx, A])
    : ReaderT[Xor[EGLError, ?],
              EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
              A] =
    for {
      a <- interpret(fa).transform(lift)
      _ <- debug(fa.toString())
    } yield a
}

final class EGLInterpreter[NDisp, NWin, Disp, Cfg: ClassTag, Sfc, Ctx]
    extends (EGL[NDisp, NWin, Disp, Cfg, Sfc, Ctx, ?] ~> Reader[
        EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
        ?]) {

  private def reader[A](
      f: EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx] => A)
    : Reader[EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx], A] = Reader(f)

  def apply[A](egl: EGL[NDisp, NWin, Disp, Cfg, Sfc, Ctx, A])
    : Reader[EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx], A] =
    egl match {
      case EGLGetError => reader(_.eglGetError)
      case EGLChooseConfig(dpy, attrs, count) =>
        reader { lib =>
          val s = Buffer.capacity[Int](1)
          val cfgs = new Array[Cfg](count)
          lib.eglChooseConfig(dpy, attrs.toArray, cfgs, count, s)
          val size = s.get()
          cfgs.take(size).toList
        }
      case EGLQueryString(disp, p) => reader(_.eglQueryString(disp, p.value))
      case EGLCreateContext(disp, cfg, sc, attribs) =>
        //explicit cast because type isn't inferred
        reader(_.eglCreateContext(disp, cfg, sc, attribs.toArray))
          .asInstanceOf[Reader[
                  EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
                  A]]
      case EGLBindAPI(api) => reader(_.eglBindAPI(api.value))
      case EGLCreateWindowSurface(disp, cfg, nw, attribs) =>
        //explicit cast because type isn't inferred
        reader(_.eglCreateWindowSurface(disp, cfg, nw, attribs.toArray))
          .asInstanceOf[Reader[
                  EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
                  A]]
      case EGLGetDisplay(nDisp) =>
        //explicit cast because type isn't inferred
        reader(_.eglGetDisplay(nDisp)).asInstanceOf[Reader[
                EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
                A]]
      case EGLSwapBuffers(disp, sfc) => reader(_.eglSwapBuffers(disp, sfc))
      case EGLMakeCurrent(disp, draw, read, ctx) =>
        reader(_.eglMakeCurrent(disp, draw, read, ctx))
      case EGLInitialize(disp) =>
        reader { lib =>
          val mj = Buffer.capacity[Int](1)
          val mn = Buffer.capacity[Int](1)
          lib.eglInitialize(disp, mj, mn)
          (mj.get(), mn.get())
        }
      case EGLGetConfigAttrib(dpy, cfg, attr) =>
        reader { lib =>
          val value = Buffer.capacity[Int](1)
          lib.eglGetConfigAttrib(dpy, cfg, attr.value, value)
          value.get()
        }
      case EGLSwapInterval(dpy, interval) =>
        reader(_.eglSwapInterval(dpy, interval))
      //explit cast because type isn't inferred
      case EGL_DEFAULT_DISPLAY() =>
        reader(_.EGL_DEFAULT_DISPLAY).asInstanceOf[Reader[
                EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
                A]]
      case EGL_NO_DISPLAY() =>
        reader(_.EGL_NO_DISPLAY).asInstanceOf[Reader[
                EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
                A]]
      case EGL_NO_CONTEXT() =>
        reader(_.EGL_NO_CONTEXT).asInstanceOf[Reader[
                EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
                A]]
      case EGL_NO_SURFACE() =>
        reader(_.EGL_NO_SURFACE).asInstanceOf[Reader[
                EGL14Library.Aux[NDisp, NWin, Disp, Cfg, Sfc, Ctx],
                A]]
    }
}
