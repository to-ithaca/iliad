package iliad

import iliad.implicits._

import scodec._
import scodec.bits._

import shapeless._

import java.io.ByteArrayInputStream


final class Bitmap[F <: HList](dimensions: Vec2i, pixels: BitVector)

final class PNGDecoder[F <: HList] extends Decoder[Bitmap[F]] {

#+desktop
import javax.imageio.ImageIO
#-desktop

  def decode(bitVector: BitVector): Attempt[DecodeResult[Bitmap[F]]] = {
    val stream = new ByteArrayInputStream(bitVector.toByteArray)
#+desktop
    //TODO: try catch
    val image = ImageIO.read(stream)
    val pixels = image.getRaster.asInstanceOf[java.awt.image.DataBufferByte].getData()
    val dimensions = v"${image.getWidth} ${image.getHeight}"
#-desktop
    //TODO: verify format?
    val bitmap = new Bitmap[F](dimensions, BitVector(pixels))
    Attempt.successful(DecodeResult(bitmap, BitVector.empty))
  }
}
