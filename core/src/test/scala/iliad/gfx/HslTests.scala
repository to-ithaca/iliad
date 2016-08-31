package iliad
package gfx

import iliad._
import iliad.algebra._
import iliad.algebra.syntax.vector._

import org.scalatest._
import org.scalatest.prop._

import scala.math._

import org.scalacheck._


final class BoolOps(val b: Boolean) extends AnyVal {
  def toDouble: Double = if(b) 1.0 else 0.0
}

object HSL {

  implicit def toBoolOps(b: Boolean): BoolOps = new BoolOps(b)

  def convert(c: Vec3d): Vec3d = {
    val r = c.x
    val g = c.y
    val b = c.z
    val m = min(min(r, g), b)
    val M = max(max(r, g), b)
    val chroma = M - m
    val hue0 = if(chroma == 0.0) 0.0 else
              (M == r).toDouble * ( ((g - b)/chroma) %  6.0) +
               (M == g).toDouble * ( (b - r)/chroma + 2.0) +
               (M == b).toDouble * ( (r - g)/chroma + 4.0)

    val hue = normalizeHue(hue0 / 6.0)

    val lightness = (M + m) / 2.0
    val saturation = if(chroma == 0.0) 0.0 else chroma / (1.0 - abs(2.0 * lightness - 1.0))

    v"$hue $saturation $lightness"
  }

  def toRGB(hsl: Vec3d): Vec3d = {
    val h = hsl.x
    val s = hsl.y
    val l = hsl.z
    val chroma = s * (1.0 - abs(2.0 * l - 1.0))
    val hue0 = h * 6.0
    val x = chroma * (1.0 - abs(hue0 % 2.0 - 1.0))

    val rgb0 =
    if      (hue0 >= 0.0 && hue0 <= 1.0) v"$chroma $x 0.0"
    else if (hue0 > 1.0 && hue0 <= 2.0) v"$x $chroma 0.0"
    else if (hue0 > 2.0 && hue0 <= 3.0) v"0.0 $chroma $x"
    else if (hue0 > 3.0 && hue0 <= 4.0) v"0.0 $x $chroma"
    else if (hue0 > 4.0 && hue0 <= 5.0) v"$x 0.0 $chroma"
    else if (hue0 > 5.0 && hue0 <= 6.0) v"$chroma 0.0 $x"
    else v"0.0 0.0 0.0"

    val m = l - 0.5 * chroma
    rgb0.map(v => v + m)
  }


  def shiftHue(prev: Double, offset: Double): Double =
    normalizeHue(prev + offset)

  def shiftSaturation(prev: Double, percentage: Double) =
    prev * percentage

  private def normalizeHue(h: Double): Double = 
    (h % 1.0 + 1.0) % 1.0
}

class HSLTests extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {

  test("hsl should be zero for black") {
    val colour = v"0.0 0.0 0.0"
    val hsl = HSL.convert(colour)
    hsl should === (v"0.0 0.0 0.0")
  }

  test("white has a lightness of 1.0") {
    val colour = v"1.0 1.0 1.0"
    val hsl = HSL.convert(colour)
    hsl should === (v"0.0 0.0 1.0")
  }

  test("red has a saturation of 1.0") {
    val colour = v"1.0 0.0 0.0"
    val hsl = HSL.convert(colour)
    hsl.y should === (1.0 +- 0.0001)
  }

  test("green has a hue of 0.3") {
    val colour = v"0.0 1.0 0.0"
    val hsl = HSL.convert(colour)
    hsl.x should === (0.3333 +- 0.001)
  }

  test("blue has a hue of 0.6") {
    val colour = v"0.0 0.0 1.0"
    val hsl = HSL.convert(colour)
    hsl.x should === (0.666 +- 0.001)
  }

  test("shifted hue should always be in the range of 0.0 to 1.0") {
    val hue = 0.5
    forAll(Gen.choose(-100.0, 100.0)) { offset => 
      val h1 = HSL.shiftHue(hue, offset)
      h1 should (be <= 1.0 and be >= 0.0)
    }
  }

  test("there is a mapping from rgb to hsl") {
    val gen = Gen.choose(0.0, 1.0)
    forAll(gen, gen, gen) { (r, g, b) =>
      val v = v"$r $g $b"
      val v1 = HSL.toRGB(HSL.convert(v))
      v.x should ===(v1.x +- 0.001)
      v.y should ===(v1.y +- 0.001)
      v.z should ===(v1.z +- 0.001)
    }
  }

  test("a hue of 0.0 corresponds to red") {
    val v = v"0.0 1.0 0.5"
    val rgb = HSL.toRGB(v)
    rgb.x should ===(1.0 +- 0.001)
    rgb.y should ===(0.0 +- 0.001)
    rgb.z should ===(0.0 +- 0.001)
  }
}
