package iliad

import cats._
import cats.data._
import cats.implicits._

import scodec.bits._

import java.io.{FileInputStream, InputStream, FileNotFoundException}

#+desktop
object ResourceLoader {

  def loadFile(name: String): Xor[FileNotFoundException, BitVector] =
    try {
      val stream: InputStream = new FileInputStream(name)
      val r = BitVector.fromInputStream(stream).force
      stream.close()
      r.right
    } catch {
      case e: FileNotFoundException => e.left
    }
}
#-desktop

#+android
import android.content.res.Resources

object ResourceLoader {

  var resources: Resources = _

  def loadFile(name: String): Xor[FileNotFoundException, BitVector] =
    try {
      val stream: InputStream = resources.getAssets().open(name)
      val r = BitVector.fromInputStream(stream).force
      println(s"bitvector is ${r}")
      stream.close()
      r.right
    } catch {
      case e: FileNotFoundException => e.left
    }
}
#-android
