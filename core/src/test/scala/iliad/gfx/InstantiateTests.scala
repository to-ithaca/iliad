package iliad
package gfx

import iliad.{gl => GL}
import iliad.implicits._

import org.scalatest._

import cats._
import cats.data._
import cats.implicits._

import CatsExtra._

class InstantiateTests extends FunSuite with Matchers {

  private def validate(x: Xor[NonEmptyList[GraphicsError], Unit]): Unit = 
    x match {
      case Xor.Left(err) => throw new Error(err.toString)
      case Xor.Right(_) => 
    }

  private val exampleVertexShader: GL.VertexShader.Source = vsh(
      "some text here",
        Attribute[Vec3f]("position"),
        Attribute[Vec3f]("normal"),
        Attribute[Int]("index")
    )

  private val exampleFragmentShader: GL.FragmentShader.Source = fsh(
    "some text here",
    Sampler.image("image")
  )

  private val exampleDrawConstructor: Draw.Constructor = draw(
      "basic-draw",
      exampleVertexShader,
      exampleFragmentShader,
      "basic-3D model",
      DrawType.Triangles,
      Dimension._3D
    )

  test("can instantiate a basic node") {
    val graph: Xor[NonEmptyList[GraphicsError], Graph.Constructed] = 
      Construct.validate(put(exampleDrawConstructor))

    val animalVref = vref("animal-vdata", 
            Attribute[Vec3f]("position"),
      Attribute[Vec3f]("normal"),
      Attribute[Int]("index")
    )
    val hedgehogVData = vd(animalVref, 12 -> 36)

    val animalEref = eref("animal-edata", "elements")
    val hedgehogEData = ed(animalEref, 12 -> 24)

    val hedgehogModel = model(
      "hedgehog-model", "basic-3D-model",
      hedgehogVData, hedgehogEData)

    val hedgehogImage = png("hedgehog-spikes", v"256 256")
    val hedgehogDraw = drawInstance(
      hedgehogModel,
      exampleDrawConstructor,
      Map.empty,
      "image" -> hedgehogImage)

    val result = for {
      i <- graph.map(_.instance)
      _ <- Instantiate(List(hedgehogDraw)).run(i).leftMap(_.widen[GraphicsError])
    } yield ()

    validate(result)
  }
}
