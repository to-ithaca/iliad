package iliad
package gl

import java.nio.{ByteBuffer, ByteOrder, IntBuffer}

private [gl] object Buffer {
  def byte(capacity: Int): ByteBuffer = 
    ByteBuffer.allocateDirect(capacity).order(ByteOrder.nativeOrder())
  def int(capacity: Int): IntBuffer = 
    byte(capacity * 4).asIntBuffer()
}
