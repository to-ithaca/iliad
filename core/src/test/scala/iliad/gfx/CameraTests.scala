package iliad
package gfx

import iliad.syntax.vectord._
import iliad.arbitrary._
import iliad.test.implicits._

import spire.algebra.{Trig, Sign}
import spire.implicits._

import cats.implicits._

import org.scalatest._
import org.scalatest.prop._

class CameraTests extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  val camera = Camera(
    position = v"0f 0f 0f",
    pointAt = v"0f 1f 0f",
    up = v"0f 0f 1f",
    near = 0.01f, 
    far = 10f,
    aspect = 1f, 
    fov = Math.PI.toFloat / 3f)

  val horizontalBound = Trig[Float].tan(camera.fov) * (camera.sightDistance)
  val verticalBound = horizontalBound * camera.aspect

  test("a point between the near and far clip planes should always be on screen") { 
    forAll(boundedArbitrary(camera.near, camera.far).arbitrary) { d => 
      val point = camera.position + d *: camera.direction
      val screenZ = camera.peek(point).z
      assert(screenZ >= -1f && screenZ <= 1f)
    }
  }

  test("a point on the near clip plane should be at the front of the screen") {
    val p = camera.position + camera.near *: camera.direction
    val screenZ = camera.peek(p).z
    screenZ should equal (-1f +- 0.01f)
  }

  test("a point on the far clip plane should be at the back of the screen") {
    val p = camera.position + camera.far *: camera.direction
    val screenZ = camera.peek(p).z
    screenZ should equal (1f +- 0.01f)
  }

  test("a point behind the near clip plane should be off screen") {
    forAll(boundedArbitrary(camera.near - 100f, camera.near).arbitrary) { d =>
      val point = camera.position + d *: camera.direction
      val screenZ = camera.peek(point).z
      assert(screenZ <= -1f || screenZ >= 1f)
    }
  }

  test("a point in front of the far clip plane should be in front of the screen") {
    forAll(boundedArbitrary(camera.far, camera.far + 100f).arbitrary) { d =>
      val point = camera.position + d *: camera.direction
      val screenZ = camera.peek(point).z
      assert(screenZ >= 1f)
    }
  }

  test("a point within the horizontal fov is on screen") {
    val b = horizontalBound
    forAll(boundedArbitrary(-b, b).arbitrary) { x => 
      val p = camera.position + camera.far *: camera.direction + x *: camera.xAxis
      val screenX = camera.peek(p).x
      assert(screenX >= -1f && screenX <= 1f)
    }
  }

  test("a point on the left hand side of the camera should be on the left hand side of the screen") {
    val b = horizontalBound
    forAll(boundedArbitrary(-b, 0f).arbitrary) { x => 
      val p = camera.position + camera.far *: camera.direction + x *: camera.xAxis
      val screenX = camera.peek(p).x
      assert(screenX >= -1f && screenX <= 0f)
    }
  }
 
  test("a point on the right hand side of the camera should be on the right hand side of the screen") {
    val b = horizontalBound
    forAll(boundedArbitrary(0f, b).arbitrary) { x => 
      val p = camera.position + camera.far *: camera.direction + x *: camera.xAxis
      val screenX = camera.peek(p).x
      assert(screenX >= 0f && screenX <= 1f)
    }
  }

  test("a point to the left of the fov is to the left of the screen") {
    val b = - horizontalBound
    forAll(boundedArbitrary(b - 10f, b).arbitrary) { x => 
      val p = camera.position + camera.far *: camera.direction + x *: camera.xAxis
      val screenX = camera.peek(p).x
      assert(screenX <= -1f)
    }
  }

  test("a point to the right of the fov is to the right of the screen") {
    val b = horizontalBound
    forAll(boundedArbitrary(b, b + 10f).arbitrary) { x => 
      val p = camera.position + camera.far *: camera.direction + x *: camera.xAxis
      val screenX = camera.peek(p).x
      assert(screenX >= 1f)
    }
  }

  test("a point within the vertical fov is on screen") {
    val b = verticalBound
    forAll(boundedArbitrary(-b, b).arbitrary) { z => 
      val p = camera.position + camera.far *: camera.direction + z *: camera.zAxis
      val screenY = camera.peek(p).y
      assert(screenY >= -1f && screenY <= 1f)
    }
  }

  test("a point on the bottom side of the camera should be on the bottom side of the screen") {
    val b = verticalBound
    forAll(boundedArbitrary(-b, 0f).arbitrary) { z => 
      val p = camera.position + camera.far *: camera.direction + z *: camera.zAxis
      val screenY = camera.peek(p).y
      assert(screenY >= -1f && screenY <= 0f)
    }
  }
 
  test("a point on the top side of the camera should be on the top side of the screen") {
    val b = verticalBound
    forAll(boundedArbitrary(0f, b).arbitrary) { z => 
      val p = camera.position + camera.far *: camera.direction + z *: camera.zAxis
      val screenY = camera.peek(p).y
      assert(screenY >= 0f && screenY <= 1f)
    }
  }

  test("a point below of the fov is below of the screen") {
    val b = - verticalBound
    forAll(boundedArbitrary(b - 10f, b).arbitrary) { z => 
      val p = camera.position + camera.far *: camera.direction + z *: camera.zAxis
      val screenY = camera.peek(p).y
      assert(screenY <= -1f)
    }
  }

  test("a point above of the fov is above of the screen") {
    val b = verticalBound
    forAll(boundedArbitrary(b, b + 10f).arbitrary) { z => 
      val p = camera.position + camera.far *: camera.direction + z *: camera.zAxis
      val screenY = camera.peek(p).y
      assert(screenY >= 1f)
    }
  }

  test("screenToWorld is the reverse of peek") { 
    val hb = boundedArbitrary(-horizontalBound, horizontalBound).arbitrary
    val vb = boundedArbitrary(-verticalBound, verticalBound).arbitrary
    forAll(hb, vb) { (x, z) => 
      val p = camera.position + 
      camera.far *: camera.direction + 
      z *: camera.zAxis + 
      x *: camera.xAxis
      val screenCoords = camera.peek(p)
      val p1 = camera.screenToWorld(screenCoords)
      p1.x should equal (p.x +- 0.1f)
      p1.y should equal (p.y +- 0.1f)
      p1.z should equal (p.z +- 0.1f)
    }
  }
}

class CameraFunctionTests extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  val camera = Camera(
    position = v"0f -1f 0f",
    pointAt = v"0f 0f 0f",
    up = v"0f 0f 1f",
    near = 0.01f, 
    far = 10f,
    aspect = 1f, 
    fov = Math.PI.toFloat / 3f)

  def radius(c: Camera[Float]): Float = (c.position - c.pointAt).norm

  val speed = 2f * Math.PI.toFloat
  val t0 = 100L
  val λ = 1f / 3000f
  test("camera should pan around z with a constant radius") {
    val f = Camera.panAroundZ(speed, Sign.Positive)(t0, camera)
    val expectedR = radius(camera)
    forAll(boundedArbitrary(t0, t0 + 10000L).arbitrary) { t => 
      radius(f(t)) should equal (expectedR +- 0.1f)
    }
  }

  test("camera should pan around z with a constant z") {
    val f = Camera.panAroundZ(speed, Sign.Positive)(t0, camera)
    forAll(boundedArbitrary(t0, t0 + 10000L).arbitrary) { t =>
      f(t).position.z should equal (camera.position.z+- 0.1f)
    }
  }

  test("camera should pan around z with the same starting position") {
    val f = Camera.panAroundZ(speed, Sign.Positive)(t0, camera)
    assert(f(t0).position === camera.position)
  }

  test("camera should scroll around z with a constant radius") {
    val f = Camera.scrollAroundZ(speed, Sign.Positive, λ)(t0, camera)
    val expectedR = radius(camera)
    forAll(boundedArbitrary(t0, t0 + 10000L).arbitrary) { t => 
      radius(f(t)) should equal (expectedR +- 0.1f)
    }
  }
  
  test("camera should scroll around z with a constant z") {
    val f = Camera.scrollAroundZ(speed, Sign.Positive, λ)(t0, camera)
    forAll(boundedArbitrary(t0, t0 + 10000L).arbitrary) { t =>
      f(t).position.z should equal (camera.position.z+- 0.1f)
    }
  }

  test("camera should scroll around z with the same start position") {
    val f = Camera.scrollAroundZ(speed, Sign.Positive, λ)(t0, camera)
    assert(f(t0).position === camera.position)
  }
}

