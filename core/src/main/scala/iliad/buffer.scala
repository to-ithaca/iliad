package iliad

import java.nio.{ByteBuffer, ByteOrder, FloatBuffer, IntBuffer}

trait BufferAllocate[A] {
  type Out
  def apply(capacity: Int): Out
}

object BufferAllocate {
  type Aux[A, B] = BufferAllocate[A] { type Out = B }

  val floatByteSize = 4
  val intByteSize = 4

  implicit lazy val byteBuffer: Aux[Byte, ByteBuffer] =
    new BufferAllocate[Byte] {
      type Out = ByteBuffer
      def apply(capacity: Int): ByteBuffer =
        ByteBuffer.allocateDirect(capacity).order(ByteOrder.nativeOrder())
    }
  implicit lazy val intBuffer: Aux[Int, IntBuffer] = new BufferAllocate[Int] {
    type Out = IntBuffer
    def apply(capacity: Int): IntBuffer =
      byteBuffer(capacity * intByteSize).asIntBuffer()
  }
  implicit lazy val floatBuffer: Aux[Float, FloatBuffer] =
    new BufferAllocate[Float] {
      type Out = FloatBuffer
      def apply(capacity: Int): FloatBuffer =
        byteBuffer(capacity * floatByteSize).asFloatBuffer()
    }
}

trait BufferPut[A, B] {
  def apply(b: B, a: A): B
}

object BufferPut {
  type Aux[A, B] = BufferPut[A, B]

  implicit lazy val intBuffer: Aux[Int, IntBuffer] =
    new BufferPut[Int, IntBuffer] {
      def apply(b: IntBuffer, a: Int): IntBuffer = b.put(a)
    }

  implicit lazy val floatBuffer: Aux[Float, FloatBuffer] =
    new BufferPut[Float, FloatBuffer] {
      def apply(b: FloatBuffer, a: Float): FloatBuffer = b.put(a)
    }

  implicit lazy val byteBuffer: Aux[Byte, ByteBuffer] =
    new BufferPut[Byte, ByteBuffer] {
      def apply(b: ByteBuffer, a: Byte): ByteBuffer = b.put(a)
    }
}

trait BufferRewind[A] {
  def apply(a: A): A
}

object BufferRewind {
  type Aux[A] = BufferRewind[A]

  implicit lazy val intBuffer: Aux[IntBuffer] = new BufferRewind[IntBuffer] {
    def apply(a: IntBuffer): IntBuffer = a.rewind().asInstanceOf[IntBuffer]
  }

  implicit lazy val floatBuffer: Aux[FloatBuffer] =
    new BufferRewind[FloatBuffer] {
      def apply(a: FloatBuffer): FloatBuffer =
        a.rewind().asInstanceOf[FloatBuffer]
    }

  implicit lazy val byteBuffer: Aux[ByteBuffer] = new Aux[ByteBuffer] {
    def apply(a: ByteBuffer): ByteBuffer = a.rewind().asInstanceOf[ByteBuffer]
  }
}

trait BufferToArray[A] {
  type Out
  def apply(a: A): Array[Out]
}

object BufferToArray {
  type Aux[A, B] = BufferToArray[A] { type Out = B }

  implicit lazy val intBuffer: Aux[IntBuffer, Int] =
    new BufferToArray[IntBuffer] {
      type Out = Int
      def apply(a: IntBuffer): Array[Int] = {
        val dst = new Array[Int](a.capacity())
        a.get(dst)
        dst
      }
    }
}

object Buffer {

  def toArray[A](a: A)(
      implicit toArray: BufferToArray[A]): Array[toArray.Out] =
    toArray(a)

  def capacity[A](capacity: Int)(
      implicit alloc: BufferAllocate[A]): alloc.Out = alloc(capacity)
  def apply[A, B](a: A*)(implicit alloc: BufferAllocate.Aux[A, B],
                         put: BufferPut[A, B],
                         rewind: BufferRewind[B]): B = {
    val b = alloc(a.size)
    rewind(a.foldLeft(b)(put.apply))
  }
}
