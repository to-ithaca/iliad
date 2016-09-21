package iliad
package benchmark

import iliad.gfx._
import iliad.algebra._
import iliad.implicits._

import cats._
import cats.data._
import cats.implicits._

import spire.implicits._

import scodec._
import scodec.codecs._
import scodec.bits._

import java.nio.ByteOrder

object GLUtils {

  val screenClearCons = clear("clear-screen", v"209 199 142 255".cmap[Float] :/ 255f)
  val screenClearInstance = clearScreen(screenClearCons)

  private val vsh = vertexShader(s"""
#version 300 es
precision highp float;
in vec2 position;

void main() {
  gl_Position = vec4(position, 0.0, 1.0);
}
""",
    Attribute[Vec2f]("position"))

  private val fsh = fragmentShader(s"""
#version 300 es
precision highp float;
uniform vec3 colour;
out vec4 o_colour;
void main() {
  o_colour = vec4(colour, 1.0);
}
""", Uniform[Vec3f]("colour"))

  val drawCons = draw("benchmark-draw", vsh, fsh, DrawType.Triangles, Dimension._3D)

  def graph(): NonEmptyList[GraphicsError] Xor Graph.Constructed = Construct.validate(for {
    _ <- put(screenClearCons)
    _ <- put(drawCons)
    _ <- order(screenClearCons -> drawCons)
  } yield ())

  private val vb: VertexBuffer = vertexBuffer("vb", Attribute[Vec2f]("position"))
  private val eb: ElementBuffer = elementBuffer("eb")
  private val planePanel = Carpenter.unitPlane[Double]
  private val planeVertexRef = vertexRef("plane-2D-vertices", planePanel.vertices.size * 2 * 4, vb)
  private val planeElementRef = elementRef("plane-2D-elements", planePanel.elements.size * 4, eb)
  val plane: Model = model(planeVertexRef, planeElementRef)

  def loadGfx: scodec.Err Xor List[GFX] = {

    def native[A](big: Codec[A], little: Codec[A]): Codec[A] = ByteOrder.nativeOrder() match {
      case ByteOrder.BIG_ENDIAN => big
      case ByteOrder.LITTLE_ENDIAN => little
    }

    for {
      vs <- list(native(float, floatL)).encode(planePanel.vertices.map(_.cmap[Float]).flatMap(v =>
        List(v.x, v.y)
      )).toXor
      es <- list(native(int32, int32L)).encode(planePanel.elements).toXor
    } yield List(
      load(vsh -> fsh),
      load(planeVertexRef, vs.toByteVector),
      load(planeElementRef, es.toByteVector)
    )
  }
}

case class BasicModel(id: Int, colour: Vec3d) {
  val colourKey: ScopeProperty = ScopeProperty("colour", s"plane-$id")
}

object BasicModel {
  implicit lazy val basicModelRender: Render[BasicModel] = new Render[BasicModel] {
    private def instance(model: BasicModel) =
      drawInstance(GLUtils.plane,
        GLUtils.drawCons,
        List(model.colourKey))

    def show(i: BasicModel): List[GFX] = List(gfx.show(instance(i)))
 
    def hide(i: BasicModel): List[GFX] = List(gfx.hide(instance(i)))
  }

  implicit lazy val basicModelUpdate: Update[BasicModel] = new Update[BasicModel] {
    def update(m: BasicModel): List[GFX] = (m.colourKey -> m.colour).update
  }
}

