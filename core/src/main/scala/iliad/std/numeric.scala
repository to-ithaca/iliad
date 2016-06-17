package iliad
package std

trait FloatInstances {
  implicit val sizeOfFloat: SizeOf[Float] = new SizeOf[Float] {
    def byteSize: Int = 4
  }

  implicit val vertexAttribTypeFloat: VertexAttribTypeConversion[Float] =
    new VertexAttribTypeConversion[Float] {
      def baseType: iliad.kernel.VertexAttribType = iliad.kernel.GL_FLOAT
    }
}

trait IntInstances {
  implicit val sizeOfInt: SizeOf[Int] = new SizeOf[Int] {
    def byteSize: Int = 4
  }
}
