package iliad.android

import iliad.kernel._
import iliad.kernel.utils.vectord._

import android.app.Activity
import android.os.Bundle
import android.util.Log
import android.view._
import android.graphics.Point
import android.support.v4.view.GestureDetectorCompat
import android.content.Context

trait AndroidBootstrap extends Activity with AndroidEventHandler { app: IliadApp =>

  def view: Int

  var detector: GestureDetectorCompat = _

  var _screenSize:Vec2f = _
  def screenSize: Vec2f = _screenSize

  override def onCreate(savedInstanceState: Bundle) {
    Log.println(Log.INFO,"MAIN ACTIVITY","!!!!!!CREATING!!!!!!!!!!!")

    super.onCreate(savedInstanceState)
    setContentView(view)

    val wm: WindowManager = getSystemService(Context.WINDOW_SERVICE).asInstanceOf[WindowManager];
    val display = wm.getDefaultDisplay();
    val size = new Point()
    display.getSize(size)
    _screenSize = v"${size.x.toFloat} ${size.y.toFloat}"

    detector = new GestureDetectorCompat(this, this)
    detector.setOnDoubleTapListener(this)

    app.run()
  }

  override def onStart() = {
    super.onStart()
  }

  override def onTouchEvent(event: MotionEvent): Boolean = {
    detector.onTouchEvent(event)      
    super.onTouchEvent(event)
  }

  override def onCreateOptionsMenu(menu: Menu): Boolean =true
  override def onPrepareOptionsMenu(menu: Menu): Boolean = true
  override def onOptionsItemSelected(item: MenuItem): Boolean = true
}
