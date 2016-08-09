package iliad
package gfx

import iliad.{gl => GL}
import iliad.implicits._

import org.scalatest._

import cats._
import cats.data._
import cats.implicits._

class ConstructTests extends FunSuite with Matchers {

  private def validate(g: State[Graph.Constructor, Unit]): Unit = 
    Construct.validate(g) match {
      case Xor.Left(err) => throw new Error(err.toString)
      case Xor.Right(_) => 
    }

  private val exampleVertexShader: GL.VertexShader.Source = vertexShader(
      "some text here",
        Attribute[Vec3f]("position"),
        Attribute[Vec3f]("normal"),
        Attribute[Int]("index")
    )

  private val exampleFragmentShader: GL.FragmentShader.Source = fragmentShader(
    "some text here",
    Sampler.image("image")
  )

  private val exampleDraw: Draw.Constructor = draw(
      "basic-draw",
      exampleVertexShader,
      exampleFragmentShader,
      "basic-3D model",
      DrawType.Triangles,
      Dimension._3D
    )

  private val exampleClear: Clear.Constructor = clear("screen-clear")

  test("can construct a basic graph") {
    val graph: State[Graph.Constructor, Unit] = put(exampleDraw)
    validate(graph)
  }

  test("can clear framebuffers") {
    val graph: State[Graph.Constructor, Unit] = put(exampleClear)

    validate(graph)
  }

  test("can order draw calls") {
    val graph: State[Graph.Constructor, Unit] = for {
      _ <- put(exampleDraw)
      _ <- put(exampleClear)
      _ <- order(exampleClear -> exampleDraw)
    } yield ()

    validate(graph)
  }

  test("can draw offscreen") {
    val fixedViewportVertexShader: GL.VertexShader.Source = vertexShader(
      "some text here",
        Attribute[Vec3f]("position"),
        Attribute[Vec3f]("normal")
    )

    val fixedViewportFragmentShader: GL.FragmentShader.Source = fragmentShader("some text here")

    val viewport = v"256 256"
    val fixedViewportTexture = txt("fixed-viewport",
          TextureFormat.rgba,
          viewport)

    val fixedViewportDraw = offScreenDraw("fixed-viewport",
      fixedViewportVertexShader,
      fixedViewportFragmentShader,
      "basic-3D-model",
        DrawType.Triangles,
      Dimension._3D,
        GL.GL_COLOR_ATTACHMENT0 -> fixedViewportTexture
    )
    
    val fixedViewportClear = offScreenClear("fixed-viewport-clear",
      GL.GL_COLOR_ATTACHMENT0 -> fixedViewportTexture
    )

    val screenVertexShader: GL.VertexShader.Source = vertexShader(
      "some text here",
        Attribute[Vec3f]("position"))

    val screenFragmentShader: GL.FragmentShader.Source = fragmentShader(
      "some text here",
      Sampler.image("unscaledImage")
    )

    val screenDraw: Draw.Constructor = draw(
      "screen-draw",
      screenVertexShader,
      screenFragmentShader,
      "screen-square",
      DrawType.Triangles,
      Dimension._2D
    )

    val screenClear = exampleClear

    val graph: State[Graph.Constructor, Unit] = for {
      _ <- put(fixedViewportDraw)
      _ <- put(fixedViewportClear)
      _ <- order(fixedViewportClear -> fixedViewportDraw)
      _ <- put(screenDraw)
      _ <- put(screenClear)
      _ <- order(screenClear -> screenDraw)
      _ <- pipe(fixedViewportDraw -> screenDraw,
        fixedViewportTexture -> "unscaledImage")
    } yield ()

    validate(graph)
  }
}
