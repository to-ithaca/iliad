package scodec

import scodec.bits._

import java.nio.ByteBuffer

import cats.data.Xor
import cats.implicits._

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

  //TODO: perhaps this should be a reverse, which chunks into 3, and reverses each triple?
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

final class AttemptOps[A](val a: Attempt[A]) extends AnyVal {
  def toXor: Xor[Err, A] = a.toEither.toXor
}

final class AttemptObjectOps(val a: Attempt.type) extends AnyVal {
  def fromXor[A, B](xor: Xor[A, B]): Attempt[B] = xor match {
    case Xor.Left(a) => Attempt.failure(Err(a.toString))
    case Xor.Right(b) => Attempt.successful(b)
  }
}
