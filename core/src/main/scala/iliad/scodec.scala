package scodec

import scodec.bits._

import java.nio.ByteBuffer

import cats.data.Xor
import cats.implicits._

import scala.math._

trait ScodecInstances {
  implicit def toBitVectorOps(bitVector: BitVector): BitVectorOps = new BitVectorOps(bitVector)
  implicit def toByteVectorOps(byteVector: ByteVector): ByteVectorOps = new ByteVectorOps(byteVector)
  implicit def toAttemptOps[A](a: Attempt[A]): AttemptOps[A] = new AttemptOps(a)
  implicit def toAttemptObjectOps(a: Attempt.type): AttemptObjectOps = new AttemptObjectOps(a)
}

object ScodecExtra extends ScodecInstances
import ScodecExtra._

final class BitVectorOps(val bitVector: BitVector) extends AnyVal {
  def toDirectByteBuffer: ByteBuffer = bitVector.toByteVector.toDirectByteBuffer

  def swizzleZYX: BitVector = bitVector.toByteVector.swizzleZYX.toBitVector
  def swizzleZYXW: BitVector = bitVector.toByteVector.swizzleZYXW.toBitVector
  def dropWFromXYZW: BitVector = bitVector.toByteVector.dropWFromXYZW.toBitVector
  def reverseColumns(nrows: Int, ncols: Int): BitVector = 
    bitVector.toByteVector.reverseColumns(nrows, ncols).toBitVector
}

final class ByteVectorOps(val byteVector: ByteVector) extends AnyVal {
  def toDirectByteBuffer: ByteBuffer = {
    val buf = ByteBuffer.allocateDirect(toIntSize(byteVector.size))
    byteVector.foreachV(v => buf.put(v.toArray))
    buf.rewind()
    buf.asReadOnlyBuffer
  }

 private def toIntSize(size: Long): Int = 
   if (size <= Int.MaxValue) size.toInt else 
     throw new IllegalArgumentException(s"size must be <= Int.MaxValue but is $size")

  def swizzleZYX: ByteVector =
    ByteVector.viewAt({(i: Long) => 
      val j = i + 2 - 2 * (i % 3)
      byteVector.apply(j)
    }, byteVector.size)

  def swizzleZYXW: ByteVector =
    ByteVector.viewAt({(i: Long) => 
      val m = i % 4
      val j = if(m == 3) i else i + 2 - 2 * m
      byteVector.apply(j)
    }, byteVector.size)


  def dropWFromXYZW: ByteVector = 
    ByteVector.viewAt({(i: Long) => 
      val j = i + floor(i.toDouble / 3.0).toInt
      byteVector.apply(j)
    }, (3.0 * byteVector.size.toDouble / 4.0).toInt)

  def reverseColumns(nrows: Int, ncols: Int): ByteVector =
    ByteVector.viewAt({(i: Long) => 
      val x = i % ncols
      val y = (i - x) / ncols
      val yNext = nrows - y - 1
      val j = yNext * ncols + x
      byteVector.apply(j)
    }, byteVector.size)
}

final class AttemptOps[A](val a: Attempt[A]) extends AnyVal {
  def toXor: Xor[Err, A] = a.toEither.toXor
}

final class AttemptObjectOps(val a: Attempt.type) extends AnyVal {
  def fromXor[A, B](xor: Xor[A, B]): Attempt[B] = xor match {
    case Xor.Left(a) => Attempt.failure(Err(a.toString))
    case Xor.Right(b) => Attempt.successful(b)
  }
}
