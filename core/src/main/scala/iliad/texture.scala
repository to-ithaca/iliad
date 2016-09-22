package iliad

import iliad.algebra._
import iliad.algebra.syntax.vector._

import scodec._
import scodec.bits._

import shapeless._

import java.io.ByteArrayInputStream

import cats._
import cats.data._
import cats.implicits._

trait TextureFormat

object TextureFormat {
  trait RGB
  trait RGBA
}

final class Bitmap[F](val dimensions: Vec2i, val pixels: ByteVector)

sealed trait PNGDecoder[F] extends Decoder[Bitmap[F]]

object PNGDecoder {
  implicit val pngDecoderRGB: PNGDecoder[TextureFormat.RGB] = new PNGDecoderRGB
  implicit val pngDecoderRGBA: PNGDecoder[TextureFormat.RGBA] = new PNGDecoderRGBA
}

#+desktop
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import BufferedImage._

private final class PNGDecoderRGB extends PNGDecoder[TextureFormat.RGB] {

  def decode(bitVector: BitVector): Attempt[DecodeResult[Bitmap[TextureFormat.RGB]]] = 
    Attempt.fromXor(for {
      i <- DecodeUtil.read(bitVector)
      _ <- DecodeUtil.checkType(i, TYPE_3BYTE_BGR, "3BYTE_BGR")
    } yield DecodeUtil.bitmap[TextureFormat.RGB](i, _.swizzleZYX.reverseColumns(i.getHeight, i.getWidth * 3)))
}

private final class PNGDecoderRGBA extends PNGDecoder[TextureFormat.RGBA] {

  def decode(bitVector: BitVector): Attempt[DecodeResult[Bitmap[TextureFormat.RGBA]]] = 
    Attempt.fromXor(for {
      i <- DecodeUtil.read(bitVector)
      _ <- DecodeUtil.checkType(i, TYPE_4BYTE_ABGR, "4BYTE_ABGR")
    } yield DecodeUtil.bitmap[TextureFormat.RGBA](i, _.swizzleZYXW.reverseColumns(i.getHeight, i.getWidth * 4)))
}


private object DecodeUtil {

  def read(bitVector: BitVector): Xor[IOReadError,  BufferedImage] = {
    val stream = new ByteArrayInputStream(bitVector.toByteArray)
    val image = try { 
      ImageIO.read(stream).right
    } catch {
      case e : java.io.IOException => IOReadError(e).left
    }
    stream.close()
    image
  }

  def checkType(i: BufferedImage, t: Int, name: String): Xor[InvalidImageTypeError, Unit] =
    if(i.getType == t) ().right
    else InvalidImageTypeError(i.getType.toString, name).left

  def bitmap[F](i: BufferedImage, f: BitVector => BitVector): DecodeResult[Bitmap[F]] = {
    val data = i.getRaster.getDataBuffer()
      .asInstanceOf[java.awt.image.DataBufferByte].getData()
    val pixels = f(BitVector(data)).toByteVector
    val dimensions = v"${i.getWidth} ${i.getHeight}"
    val bitmap = new Bitmap[F](dimensions, pixels)
    DecodeResult(bitmap, BitVector.empty)
  }
}
#-desktop

#+android

import android.graphics.BitmapFactory
import android.graphics.{Bitmap => ABitmap}
import java.nio.{ByteBuffer, ByteOrder}

private final class PNGDecoderRGB extends PNGDecoder[TextureFormat.RGB] {
  def decode(bitVector: BitVector): Attempt[DecodeResult[Bitmap[TextureFormat.RGB]]] = 
  Attempt.fromXor(for {
    raw <- DecodeUtil.read(bitVector)
    i <- copy(raw)
    dims = v"${i.getWidth} ${i.getHeight}"
    b <- DecodeUtil.toByteVector(i, dims.x * dims.y * 4,
      _.dropWFromXYZW.reverseColumns(i.getHeight, i.getWidth * 3))
  } yield DecodeResult(new Bitmap[TextureFormat.RGB](dims, b), BitVector.empty))

  private def copy(i: ABitmap): Xor[IOReadError, ABitmap] = 
    Option(i.copy(ABitmap.Config.ARGB_8888, false))
      .toRightXor(IOReadError(new Error("android.graphics.Bitmap.copy returned null")))
}

private final class PNGDecoderRGBA extends PNGDecoder[TextureFormat.RGBA] {
  def decode(bitVector: BitVector): Attempt[DecodeResult[Bitmap[TextureFormat.RGBA]]] =
    Attempt.fromXor(for {
      i <- DecodeUtil.read(bitVector)
      _ <- DecodeUtil.checkType(i, ABitmap.Config.ARGB_8888)
      dims = v"${i.getWidth} ${i.getHeight}"
      b <- DecodeUtil.toByteVector(i, dims.x * dims.y * 4, 
        _.reverseColumns(i.getHeight, i.getWidth * 4))
    } yield DecodeResult(new Bitmap[TextureFormat.RGBA](dims, b), BitVector.empty))
}

private object DecodeUtil {
  def read(bitVector: BitVector): Xor[IOReadError, ABitmap] = {
    val s = new ByteArrayInputStream(bitVector.toByteArray)
    Option(BitmapFactory.decodeStream(s))
      .toRightXor(IOReadError(new Error("BitmapFactory.decodeStream returned null")))
  }

  def checkType(i: ABitmap, c: ABitmap.Config): Xor[InvalidImageTypeError, Unit] =
    if(i.getConfig == c) ().right
    else InvalidImageTypeError(i.getConfig.toString, c.toString).left

  def toByteVector(i: ABitmap, byteCount: Int, f: BitVector => BitVector): Xor[IOReadError, ByteVector] = try {
    val buffer = ByteBuffer.allocateDirect(byteCount).order(ByteOrder.nativeOrder())
    i.copyPixelsToBuffer(buffer)
    buffer.rewind()
    f(BitVector(buffer)).toByteVector.right
  } catch {
    case e: Throwable => IOReadError(e).left
  }
}
#-android

sealed trait PNGDecoderError extends Throwable
case class IOReadError(err: Throwable) extends PNGDecoderError {
  override def toString = s"Failed to read image: $err"
}
case class InvalidImageTypeError(actual: String, expected: String) extends PNGDecoderError {
  override def toString = s"Image type [ $actual ] did not equal expected type [ $expected ]"
}
