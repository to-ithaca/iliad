package scodec

import scodec.bits._

import java.nio.ByteBuffer

trait ScodecInstances {
  implicit def toBitVectorOps(bitVector: BitVector): BitVectorOps = new BitVectorOps(bitVector)
  implicit def toByteVectorOps(byteVector: ByteVector): ByteVectorOps = new ByteVectorOps(byteVector)
}

object ScodecExtra extends ScodecInstances
import ScodecExtra._

final class BitVectorOps(val bitVector: BitVector) extends AnyVal {
  def toDirectByteBuffer: ByteBuffer = bitVector.toByteVector.toDirectByteBuffer

  def swizzleZYX: BitVector = bitVector.toByteVector.swizzleZYX.toBitVector
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
}
