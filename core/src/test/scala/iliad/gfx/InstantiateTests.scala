package iliad
package gfx

import iliad.{gl => GL}
import iliad.algebra._

import org.scalatest._

import cats._
import cats.data._
import cats.implicits._

import scodec._
import scodec.codecs._
import scodec.bits._

class InstantiateTests extends FunSuite with Matchers {

  private def validate(x: Xor[NonEmptyList[GraphicsError], Unit]): Unit = 
    x match {
      case Xor.Left(err) => throw new Error(err.toString)
      case Xor.Right(_) => 
    }

  private val exampleVertexShader: GL.VertexShader.Source = vertexShader(
      "some text here",
        Attribute[Vec3f]("position"),
        Attribute[Vec3f]("normal")
    )

  private val exampleFragmentShader: GL.FragmentShader.Source = fragmentShader(
    "some text here",
    Sampler.image("image")
  )

  private val exampleDrawConstructor: Draw.Constructor = draw(
      "basic-draw",
      exampleVertexShader,
      exampleFragmentShader,
      DrawType.Triangles,
      Dimension._3D
    )

  test("can instantiate a basic node") {
    val graph: Xor[NonEmptyList[GraphicsError], Graph.Constructed] = 
      Construct.validate(put(exampleDrawConstructor))

    val vb = vertexBuffer("animal-vb",             
      Attribute[Vec3f]("position"),
      Attribute[Vec3f]("normal")
    )

    val eb = elementBuffer("animal-eb")

    val vertexData: ByteVector = list(floatL).encode(List(
      0f, 0f, 0f,
      0f, 1f, 0f,

      0f, 1f, 0f,
      0f, 1f, 0f,

      1f, 0f, 0f,
      0f, 1f, 0f
    )).toOption.get.toByteVector

    val elementData: ByteVector = list(int32L).encode(List(0, 1, 2)).toOption.get.toByteVector

    val hedgehogVref = vertexRef("hedgehog-vertices", vertexData, vb)
    val hedgehogEref = elementRef("hedgehog-elements", elementData, eb)
    val hedgehogModel = model("hedgehog", hedgehogVref, hedgehogEref)

    val hedgehogImage = Texture.Image("hedgehog-spikes", GL.Texture.Format.rgba)
    val hedgehogDraw = drawInstance(
      hedgehogModel,
      exampleDrawConstructor,
      List.empty,
      "image" -> hedgehogImage)

    val result = for {
      i <- graph.map(_.instance)
      _ <- Instantiate(hedgehogDraw, Nil).run(i).leftMap(_.widen[GraphicsError])
    } yield ()

    validate(result)
  }
}
