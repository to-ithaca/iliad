package iliad

import scodec._
import scodec.bits._

import org.scalatest._

class ScodecTests extends FunSuite with Matchers {

  test("direct byte buffer should be direct") {
    val bitVector = BitVector(Array[Byte](1, 2, 3))
    bitVector.toDirectByteBuffer.isDirect should be(true)
  }

  test("direct byte buffer should be rewound") {
    val bitVector = BitVector(Array[Byte](1, 2, 3))
    bitVector.toDirectByteBuffer.position should be(0)
  }

  test("direct byte buffer should be readOnly") {
    val bitVector = BitVector(Array[Byte](1, 2, 3))
    bitVector.toDirectByteBuffer.isReadOnly should be(true)
  }

  test("swizzleZYX should flip 1st and 3rd values, keeping second values constant") {
    val bitVector = BitVector(Array[Byte](1, 2, 3, 4, 5, 6))
    bitVector.swizzleZYX should ===(BitVector(Array[Byte](3, 2, 1, 6, 5, 4)))
  }
}
