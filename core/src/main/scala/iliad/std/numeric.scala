package iliad
package std

trait FloatInstances {
  implicit val sizeOfFloat: SizeOf[Float] = new SizeOf[Float] {
    def byteSize: Int = 4
  }
}

trait IntInstances {
  implicit val sizeOfInt: SizeOf[Int] = new SizeOf[Int] {
    def byteSize: Int = 4
  }
}
