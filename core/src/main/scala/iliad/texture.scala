package iliad

import iliad.implicits._

import scodec._
import scodec.bits._

import shapeless._

import java.io.ByteArrayInputStream

final class Bitmap[F <: HList](val dimensions: Vec2i, val pixels: BitVector)

final class PNGDecoder[F <: HList] extends Decoder[Bitmap[F]] {

#+desktop
import javax.imageio.ImageIO
#-desktop

  def decode(bitVector: BitVector): Attempt[DecodeResult[Bitmap[F]]] = {
    val stream = new ByteArrayInputStream(bitVector.toByteArray)
#+desktop
    //TODO: try catch
    val image = ImageIO.read(stream)
    val pixels = BitVector(image.getRaster.getDataBuffer()
      .asInstanceOf[java.awt.image.DataBufferByte].getData()).swizzleZYX
    val dimensions = v"${image.getWidth} ${image.getHeight}"
#-desktop
    //TODO: verify format?
    val bitmap = new Bitmap[F](dimensions, pixels)
    Attempt.successful(DecodeResult(bitmap, BitVector.empty))
  }
}
