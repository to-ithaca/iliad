package iliad
package gl

import scala.reflect.ClassTag

import cats._
import cats.data._
import cats.implicits._

import com.typesafe.scalalogging._

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

final class EGLDebugInterpreter(interpret: EGL ~> Reader[EGL14.type,?])
    extends (EGL ~> ReaderT[
        Xor[EGLError, ?],
        EGL14.type,
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
              EGL14.type,
              Unit] =
    interpret(EGLGetError).transform(lift).mapF(_.flatMap(onError(method)))

  def apply[A](fa: EGL[A])
    : ReaderT[Xor[EGLError, ?],
              EGL14.type,
              A] =
    for {
      a <- interpret(fa).transform(lift)
      _ <- debug(fa.toString())
    } yield a
}

object EGLInterpreter
    extends (EGL ~> Reader[
        EGL14.type,
        ?]) {

  def apply[A](egl: EGL[A]): Reader[EGL14.type, A] =
    egl match {
      case EGLGetError => Reader(_.eglGetError)
      case EGLChooseConfig(dpy, attrs, count) =>
        Reader { lib =>
          val s = Buffer.int(1)
          val cfgs = new Array[EGL14.EGLConfig](count)
          lib.eglChooseConfig(dpy, attrs.toArray, cfgs, count, s)
          val size = s.get()
          cfgs.take(size).toList
        }
      case EGLQueryString(disp, p) => Reader(_.eglQueryString(disp, p.value))
      case EGLCreateContext(disp, cfg, sc, attribs) =>
        Reader(_.eglCreateContext(disp, cfg, sc, attribs.toArray))
      case EGLBindAPI(api) => Reader(_.eglBindAPI(api.value))
      case EGLCreateWindowSurface(disp, cfg, nw, attribs) =>
        Reader(_.eglCreateWindowSurface(disp, cfg, nw, attribs.toArray))
      case EGLGetDisplay(nDisp) =>
        Reader(_.eglGetDisplay(nDisp))
      case EGLSwapBuffers(disp, sfc) => Reader(_.eglSwapBuffers(disp, sfc))
      case EGLMakeCurrent(disp, draw, read, ctx) =>
        Reader(_.eglMakeCurrent(disp, draw, read, ctx))
      case EGLInitialize(disp) =>
        Reader { lib =>
          val mj = Buffer.int(1)
          val mn = Buffer.int(1)
          lib.eglInitialize(disp, mj, mn)
          (mj.get(), mn.get())
        }
      case EGLGetConfigAttrib(dpy, cfg, attr) =>
        Reader { lib =>
          val value = Buffer.int(1)
          lib.eglGetConfigAttrib(dpy, cfg, attr.value, value)
          value.get()
        }
      case EGLSwapInterval(dpy, interval) =>
        Reader(_.eglSwapInterval(dpy, interval))
    }
}
