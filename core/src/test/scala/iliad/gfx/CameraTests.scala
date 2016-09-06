package iliad
package gfx

import iliad.algebra._
import iliad.algebra.syntax.vector._

import spire.algebra._
import spire.implicits._

import cats.implicits._

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._

class CameraTests extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  val width = Trig[Float].tan(Math.PI.toFloat / 3f)

  val camera = Camera(
    position = v"0f 0f 0f",
    focalPoint = v"0f 1f 0f",
    up = v"0f 0f 1f",
    near = 0.01f, 
    far = 10f,
    aspect = 1f, 
    width = width)

  val horizontalBound = width
  val verticalBound = width * camera.aspect

  test("the camera is pointing forwards") {
    camera.direction should ===(v"0f 1f 0f")
  }

  test("Camera#nearOffset is the distance of the near clip plane from the position") {
    camera.nearOffset should ===(1.01f)
  }

  test("Camera#farOffset is the distance of the far clip plane from the position") {
    camera.farOffset should ===(11f)
  }

  test("translation matrix is orthogonal") {
    camera.translate.isOrtho should be(true)
  }

  test("rotation matrix is orthogonal") {
    camera.rotate.isOrtho should be(true)
  }

  test("invese matrix is orthogonal") {
    camera.invertX.isOrtho should be(true)
  }

  test("transformation matrix is orthogonal") {
    (camera.rotate * camera.translate) * v"0f 2f 0f 1f" should ===(v"0f 0f 2f 1f")
  }

  test("a point between the near and far clip planes should always be on screen") { 
    forAll(Gen.choose(camera.nearOffset, camera.farOffset)) { d => 
      val point = camera.position + d *: camera.direction
      val screenZ = camera.peek(point).z
      assert(screenZ >= -1f && screenZ <= 1f)
    }
  }

  test("a point on the near clip plane should be at the front of the screen") {
    val p = camera.position + (camera.nearOffset *: camera.direction)
    val screenZ = camera.peek(p).z
    screenZ should equal (-1f +- 0.01f)
  }

  test("a point on the far clip plane should be at the back of the screen") {
    val p = camera.position + camera.farOffset *: camera.direction
    val screenZ = camera.peek(p).z
    screenZ should equal (1f +- 0.01f)
  }

  test("a point behind the near clip plane should be off screen") {
    forAll(Gen.choose(camera.nearOffset - 100f, camera.nearOffset)) { d =>
      val point = camera.position + d *: camera.direction
      val screenZ = camera.peek(point).z
      assert(screenZ <= -1f || screenZ >= 1f)
    }
  }

  test("a point in front of the far clip plane should be in front of the screen") {
    forAll(Gen.choose(camera.farOffset, camera.farOffset + 100f)) { d =>
      val point = camera.position + d *: camera.direction
      val screenZ = camera.peek(point).z
      assert(screenZ >= 1f)
    }
  }

  test("a point within the horizontal fov is on screen") {
    forAll(Gen.choose(-horizontalBound, horizontalBound)) { x => 
      val p = camera.focalPoint + x *: camera.xAxis
      val screenX = camera.peek(p).x
      assert(screenX >= -1f && screenX <= 1f)
    }
  }

  test("a point on the left hand side of the camera should be on the left hand side of the screen") {
    forAll(Gen.choose(-horizontalBound, 0f)) { x => 
      val p = camera.focalPoint + x *: camera.xAxis
      val screenX = camera.peek(p).x
      assert(screenX >= -1f && screenX <= 0f)
    }
  }
 
  test("a point on the right hand side of the camera should be on the right hand side of the screen") {
    forAll(Gen.choose(0f, horizontalBound)) { x => 
      val p = camera.focalPoint + x *: camera.xAxis
      val screenX = camera.peek(p).x
      assert(screenX >= 0f && screenX <= 1f)
    }
  }

  test("a point to the left of the fov is to the left of the screen") {
    forAll(Gen.choose(- horizontalBound - 10f, - horizontalBound)) { x => 
      val p = camera.focalPoint + x *: camera.xAxis
      val screenX = camera.peek(p).x
      assert(screenX <= -1f)
    }
  }

  //TODO: add scalatest support for >= (x +- dx)
  test("a point to the right of the fov is to the right of the screen") {
    forAll(Gen.choose(horizontalBound, horizontalBound + 10f)) { x => 
      val p = camera.focalPoint + x *: camera.xAxis
      val screenX = camera.peek(p).x
      screenX should be >= 1f
    }
  }

  test("a point within the vertical fov is on screen") {
    forAll(Gen.choose(-verticalBound, verticalBound)) { z => 
      val p = camera.focalPoint + z *: camera.zAxis
      val screenY = camera.peek(p).y
      assert(screenY >= -1f && screenY <= 1f)
    }
  }

  test("a point on the bottom side of the camera should be on the bottom side of the screen") {
    forAll(Gen.choose(-verticalBound, 0f)) { z => 
      val p = camera.focalPoint + z *: camera.zAxis
      val screenY = camera.peek(p).y
      assert(screenY >= -1f && screenY <= 0f)
    }
  }
 
  test("a point on the top side of the camera should be on the top side of the screen") {
    forAll(Gen.choose(0f, verticalBound)) { z => 
      val p = camera.focalPoint + z *: camera.zAxis
      val screenY = camera.peek(p).y
      assert(screenY >= 0f && screenY <= 1f)
    }
  }

  test("a point below of the fov is below of the screen") {
    forAll(Gen.choose(- verticalBound - 10f, - verticalBound)) { z => 
      val p = camera.focalPoint + z *: camera.zAxis
      val screenY = camera.peek(p).y
      assert(screenY <= -1f)
    }
  }

  test("a point above of the fov is above of the screen") {
    forAll(Gen.choose(verticalBound, verticalBound + 10f)) { z => 
      val p = camera.focalPoint + z *: camera.zAxis
      val screenY = camera.peek(p).y
      assert(screenY >= 1f)
    }
  }

  test("screenToWorld is the reverse of peek") { 
    forAll(
      Gen.choose(-horizontalBound, horizontalBound),
      Gen.choose(-verticalBound, verticalBound)) { (x, z) =>
      val p = camera.position + 
      camera.farOffset *: camera.direction + 
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

  val width = Trig[Float].tan(Math.PI.toFloat / 3f)

  val camera = Camera(
    position = v"0f -1f 0f",
    focalPoint = v"0f 0f 0f",
    up = v"0f 0f 1f",
    near = 0.01f, 
    far = 10f,
    aspect = 1f, 
    width = width)

  def radius(c: Camera[Float]): Float = (c.position - c.focalPoint).norm

  val speed = 2f * Math.PI.toFloat
  val t0 = 100L
  val λ = 1f / 3000f
  val θ = Math.PI.toFloat / 6f

  test("camera functions with duration are guaranteed to receive values within that duration") {
    forAll(Gen.choose(0f, 10f), Gen.choose(100L, 1000L), Gen.choose(0L, 20L)) { (dt, t0, t) =>
      val f = CameraFunction(Some(dt), (t: Float, c: Camera[Float]) => {
        t should (be >= 0f and be <= 10f)
        c
      })
      val ff = Camera.start(t0, camera, f)
      ff(t0 + t)
    }
  }
  
  test("camera should pan around z with a constant radius") {
    val f = Camera.start(t0, camera, Camera.panAroundZ(speed, Camera.Anticlockwise))
    val expectedR = radius(camera)
    forAll(Gen.choose(t0, t0 + 10000L)) { t => 
      radius(f(t)) should equal (expectedR +- 0.1f)
    }
  }

  test("camera should pan around z with a constant z") {
    val f = Camera.start(t0, camera, Camera.panAroundZ(speed, Camera.Anticlockwise))
    forAll(Gen.choose(t0, t0 + 10000L)) { t =>
      f(t).position.z should equal (camera.position.z+- 0.1f)
    }
  }

  test("camera should pan around z with the same starting position") {
    val f = Camera.start(t0, camera, Camera.panAroundZ(speed, Camera.Anticlockwise))
    assert(f(t0).position === camera.position)
  }

  test("camera should pan around z with the correct rotation") {
    val dt = 100L
    val speed = (Math.PI.toFloat / 2f) / dt.toFloat
    val f = Camera.start(t0, camera, Camera.panAroundZ(speed, Camera.Anticlockwise))
    val p = f(t0 + dt).position
    p.x should equal (1f +- 0.0001f)
    p.y should equal (0f +- 0.0001f)
  }

  test("camera should scroll around z with a constant radius") {
    val f = Camera.start(t0, camera, Camera.scrollAroundZ(speed, Camera.Anticlockwise, λ))
    val expectedR = radius(camera)
    forAll(Gen.choose(t0, t0 + 10000L)) { t => 
      radius(f(t)) should equal (expectedR +- 0.1f)
    }
  }
  
  test("camera should scroll around z with a constant z") {
    val f = Camera.start(t0, camera, Camera.scrollAroundZ(speed, Camera.Anticlockwise, λ))
    forAll(Gen.choose(t0, t0 + 10000L)) { t =>
      f(t).position.z should equal (camera.position.z+- 0.1f)
    }
  }

  test("camera should scroll around z with the same start position") {
    val f = Camera.start(t0, camera, Camera.scrollAroundZ(speed, Camera.Anticlockwise, λ))
    assert(f(t0).position === camera.position)
  }

  test("camera should pan to point with the same starting position") {
    val f = Camera.start(t0, camera, Camera.panVerticallyBy(speed, θ))
    assert(f(t0).position === camera.position)
  }

  test("camera panTo should stop at the end point") {
    val dt = 100L
    val speed = θ / dt.toFloat
    val f = Camera.start(t0, camera, Camera.panVerticallyBy(speed, θ))
    assert(f(t0 + 2L * dt).position === f(t0 + 3L * dt).position)
  }

  test("camera panTo should pan with the same radius as the initial radius") {
    val dt = 100L
    val speed = θ / dt.toFloat
    val f = Camera.start(t0, camera, Camera.panVerticallyBy(speed, θ))
    forAll(Gen.choose(t0, t0 + dt)) { t =>
      f(t).radius should equal (camera.radius +- 0.00001f)
    }
  }

  test("camera panTo should pan towards z for positive angles") {
    val θ = Math.PI.toFloat / 4f
    val dt = 100L
    val speed = θ / dt.toFloat
    val f = Camera.start(t0, camera, Camera.panVerticallyBy(speed, θ))
    assert(f(t0 + 2L * dt).position === v"0f -1f 1f".normalize)
  }

  test("camera panTo should pan away from z for negative angles") {
    val θ = - Math.PI.toFloat / 4f
    val dt = 100L
    val speed = θ / dt.toFloat
    val f = Camera.start(t0, camera, Camera.panVerticallyBy(speed, θ))
    assert(f(t0 + 2L * dt).position === v"0f -1f -1f".normalize)
  }

  test("camera zoom should keep the world near and far coords the same") {
    val dt = 100f
    val yMax = 100f
    val pNear = camera.focalPoint + v"0f ${camera.near} 0f"
    val f = Camera.start(t0, camera, Camera.zoom(dt, yMax))

    forAll(Gen.choose(t0, t0 + 100L)) { t => 
      f(t).peek(pNear).z should equal (-1f +- 0.01f)
    }
  }

  test("camera zoom should keep the plane at focalPoint constant throughout the transformation") {
    val dt = 100f
    val yMax = 100f
    val x = 3f
    val point = camera.focalPoint + x *: camera.xAxis
    val xp = camera.peek(point).x

    val f = Camera.start(t0, camera, Camera.zoom(dt, yMax))

    forAll(Gen.choose(t0, t0 + 100L)) { t => 
      f(t).peek(point).x should equal (xp +- 0.01f)
    }
  }
}
