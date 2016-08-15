package iliad.kernel

import scala.reflect._

import iliad.kernel._

import android.os.SystemClock
import android.app.Activity
import android.app.Fragment
import android.os.Bundle
import android.util.Log
import android.view._
import android.graphics.Point
import android.content.Context
import android.support.v4.view.GestureDetectorCompat

import fs2._
import fs2.util._
import fs2.async.mutable._

import com.typesafe.scalalogging._

import cats._
import cats.data._
import cats.implicits._

trait AndroidDependencies extends GLDependencies with IliadApp {

  type NativeDisplay = iliad.kernel.EGL14.EGLNativeDisplayType
  type NativeWindow = iliad.kernel.EGL14.EGLNativeWindowType
  type NativePixmap = iliad.kernel.EGL14.EGLNativePixmapType
  type EGLConfig = iliad.kernel.EGL14.EGLConfig
  type EGLSurface = iliad.kernel.EGL14.EGLSurface
  type EGLDisplay = iliad.kernel.EGL14.EGLDisplay
  type EGLContext = iliad.kernel.EGL14.EGLContext

  val configClassTag = classTag[iliad.kernel.EGL14.EGLConfig]

  val EGL14 = iliad.kernel.EGL14
  val GLES30 = iliad.kernel.GLES30

  val MatrixLib = AndroidMatrixLibrary
}

trait AndroidVSync extends LazyLogging {

  private implicit val S: Strategy = Strategy.fromFixedDaemonPool(1, "vsync")

  private val _choreographer = Choreographer.getInstance

  private def _vsync(s: Signal[Task, Long]): Unit =
    _choreographer.postFrameCallback {
      new Choreographer.FrameCallback() {
        def doFrame(frameTimeNanos: Long): Unit = {
          val millis = frameTimeNanos / 1000000L + 
          System.currentTimeMillis - SystemClock.uptimeMillis
          s.set(System.currentTimeMillis).unsafeAttemptRun.toXor match {
            case Xor.Right(_) =>
            case Xor.Left(err) =>
              throw new Error(
                s"Failed to set vsync signal at time $millis. ${err.getMessage}"
              )
          }
          _choreographer.postFrameCallback(this)
        }
      }
    }

  def vsync: fs2.Stream[Task, Long] =
    Stream.eval(async.signalOf[Task, Long](0L)).flatMap { s =>
      _vsync(s)
      s.discrete
    }
}

trait AndroidBootstrap extends Activity with AndroidEventHandler
    with AndroidDependencies with AndroidVSync with LazyLogging with View.OnLayoutChangeListener {
  app: IliadApp =>

  val pageSize: Int = 1024

  def mainXML: Int
  def fragmentXML: Int
  def subFragment: Int
  def subView: Int

  var _width: Int = _
  var _height: Int = _
  def width: Int = _width
  def height: Int = _height

  override def onCreate(savedInstanceState: Bundle) {
    logger.info("AndroidBootstrap.onCreate: creating activity")
    super.onCreate(savedInstanceState)
    setContentView(mainXML)

    detector = new GestureDetectorCompat(this, this)
    detector.setOnDoubleTapListener(this)

    recogniser = EventRecogniser.Blank

    if (savedInstanceState == null) {
      val transaction = getFragmentManager.beginTransaction()
      val fragment = new AndroidFragment(this, subView, fragmentXML, session)
      transaction.replace(subFragment, fragment)
      transaction.commit()
    }
  }

   override def onLayoutChange(v: View, left: Int, top: Int, right: Int, bottom: Int,
     oldLeft: Int, oldTop: Int, oldRight: Int, oldBottom: Int) {
     _width = right - left
     _height = bottom - top
     logger.info(s"width and height are $width $height")
   }

  override def onCreateOptionsMenu(menu: Menu): Boolean = true
  override def onPrepareOptionsMenu(menu: Menu): Boolean = true
  override def onOptionsItemSelected(item: MenuItem): Boolean = true
}

final class AndroidFragment(app: AndroidBootstrap, subView: Int, fragmentXML: Int,
  session: BlockingPromise[(SurfaceHolder, Int)]) extends Fragment with LazyLogging {

  override def onCreateView(inflater: LayoutInflater ,container: ViewGroup, savedInstanceState: Bundle) = {
    logger.info("AndroidFragment.onCreateView: creating view")
    inflater.inflate(fragmentXML, container, false)
  }

  override def onViewCreated(v: View, savedInstanceState: Bundle): Unit = {
    logger.info("AndroidFragment.onViewCreated: created view. Running app")
    val view = v.findViewById(subView).asInstanceOf[SurfaceView]
    view.addOnLayoutChangeListener(app)
    session.set((view.getHolder(), EGL14.EGL_DEFAULT_DISPLAY))
    app.run()
  }
}
