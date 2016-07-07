package iliad
package kernel

import iliad.kernel.platform.{GLES30Library => Lib}

import android.opengl.{GLES30 => GLES30Library, GLES20 => GLES20Library}

import java.nio._

@bridge[GLES20Library] trait GLES20Binding
@bridge[GLES30Library] trait GLES30Binding

object GLES30 extends Lib with GLES20Binding with GLES30Binding {

  def glGetProgramInfoLog(program: Int, bufSize: Int, length: java.nio.IntBuffer, infoLog: Buffer): Unit = {
    val info = glGetProgramInfoLog(program).toCharArray
    Array.copy(info, 0, infoLog.array(), 0, bufSize) //TODO: check that this works
  }

  def glGetShaderInfoLog(shader: Int, bufSize: Int, length: java.nio.IntBuffer, infoLog: ByteBuffer): Unit = {
    val info = glGetShaderInfoLog(shader).toCharArray
    Array.copy(info, 0, infoLog.array(), 0 , bufSize)
  }

  def glShaderSource(shader: Int,count: Int, sources: Array[String], lengths: Array[Int]): Unit = {
    (0 until count).foreach ( i =>
      glShaderSource(i + shader, sources(i))
    )
  }

  def glClearBufferfv(buffer: Int, drawbuffer: Int, value: Array[Float]): Unit =
    glClearBufferfv(buffer, drawbuffer, value, 0)

  def glClearBufferiv(buffer: Int, drawbuffer: Int, value: Array[Int]): Unit =
    glClearBufferiv(buffer, drawbuffer, value, 0)

  def glClearBufferuiv(buffer: Int, drawbuffer: Int, value: Array[Int]): Unit =
    glClearBufferuiv(buffer, drawbuffer, value, 0)
}
