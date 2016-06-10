package iliad

import iliad.kernel.GL

import java.nio.ByteBuffer

case class VertexData(data: ByteBuffer, numVertices: Int)
case class ElementData(data: ByteBuffer, numElements: Int)  
case class ModelData(buffer: GL.BufferInstance, vertexData: VertexData, elementData: ElementData)
