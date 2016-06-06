package iliad
package kernel
package platform

import java.nio._

trait GLES30Library {
  def glBlitFramebuffer(srcX0: Int, srcY0: Int, srcX1: Int, srcY1: Int, destX0: Int, destY0: Int, destX1: Int, destY1: Int, bitMask: Int, filter: Int): Unit
  def glViewport(x: Int, y: Int, width: Int, height: Int): Unit
  def glFlush(): Unit
  def glClear(bitMask: Int): Unit
  //def glReadPixels(x: Int, y: Int, width: Int, height: Int, format: Int, `type`: Int, data: Buffer): Unit
  def glClearColor(red: Float, green: Float, blue: Float, alpha: Float): Unit
  def glEnable(cap: Int): Unit
  def glDisable(cap: Int): Unit
  def glGetError(): Int
  def glCreateShader(`type`: Int): Int
  def glShaderSource(shader: Int, count: Int, sources: Array[String], lengths: Array[Int]): Unit
  def glDeleteShader(shader: Int): Unit
  def glCompileShader(shader: Int): Unit
  def glAttachShader(program: Int, shader: Int): Unit
  def glGetShaderiv(shader: Int, pname: Int, ptr: IntBuffer): Unit
  def glGetShaderInfoLog(shader: Int, maxLength: Int, length: IntBuffer, infoLog: ByteBuffer): Unit
  def glCreateProgram(): Int
  def glUseProgram(program: Int): Unit
  def glLinkProgram(program: Int): Unit
  // def glGetProgramiv(pid: Int, name: Int, ptr: IntBuffery): Unit
  def glGenBuffers(num: Int, ptr: IntBuffer): Unit
  def glBindBuffer(target: Int, buffer: Int): Unit
  def glBufferData(target: Int, size: Int, data: Buffer, usage: Int): Unit
  def glBufferSubData(target: Int, offset: Int, size: Int, data: Buffer): Unit
  def glEnableVertexAttribArray(location: Int): Unit
  def glVertexAttribPointer(location: Int, size: Int, `type`: Int, normalized: Boolean, stride: Int, offset: Int): Unit
  def glGenFramebuffers(num: Int, ptr: IntBuffer): Unit
  def glBindFramebuffer(target: Int, framebuffer: Int): Unit
  def glFramebufferRenderbuffer(target: Int, attachment: Int, renderbufferTarget: Int, renderbuffer: Int): Unit
  def glCheckFramebufferStatus(target: Int): Int
  def glFramebufferTexture2D(target: Int, attachment: Int, texTarget: Int, texture: Int, level: Int): Unit
  def glGenRenderbuffers(num: Int, ptr: IntBuffer): Unit
  def glBindRenderbuffer(target: Int, renderbuffer: Int): Unit
  def glRenderbufferStorage(target: Int, format: Int, width: Int, height: Int): Unit
  def glBindTexture(target: Int, texture: Int): Unit
  def glGenTextures(num: Int, ptr: IntBuffer): Unit
  def glTexParameteri(target: Int, name: Int, value: Int): Unit
  def glTexImage2D(target: Int, level: Int, internalFormat: Int, width: Int, height: Int, border: Int, format: Int, `type`: Int, data: Buffer): Unit
  def glPixelStorei(name: Int, value: Int): Unit
  def glActiveTexture(texture: Int): Unit
  def glDrawArrays(mode: Int, first: Int, count: Int): Unit
  def glDrawElements(mode: Int, count: Int, `type`: Int, offset: Int): Unit
  def glDrawElements(mode: Int, count: Int, `type`: Int, indices: Buffer): Unit
  def glUniform1i(location: Int, arg0: Int): Unit
  def glUniform1f(location: Int, arg0: Float): Unit
  def glUniform1fv(location: Int, count: Int, ptr: Array[Float]): Unit
  def glUniform1iv(location: Int, count: Int, ptr: Array[Int]): Unit
  def glUniform2i(location: Int, arg0: Int, arg1: Int): Unit
  def glUniform2f(location: Int, arg0: Float, arg1: Float): Unit
  def glUniform2fv(location: Int, count: Int, ptr: Array[Float]): Unit
  def glUniform2iv(location: Int, count: Int, ptr: Array[Int]): Unit
  def glUniform3i(location: Int, arg0: Int, arg1: Int, arg2: Int): Unit
  def glUniform3f(location: Int, arg0: Float, arg1: Float, arg2: Float): Unit
  def glUniform3fv(location: Int, count: Int, ptr: Array[Float]): Unit
  def glUniform3iv(location: Int, count: Int, ptr: Array[Int]): Unit
  def glUniform4i(location: Int, arg0: Int, arg1: Int, arg2: Int, arg3: Int): Unit
  def glUniform4f(location: Int, arg0: Float, arg1: Float, arg2: Float, arg3: Float): Unit
  def glUniform4fv(location: Int, count: Int, ptr: Array[Float]): Unit
  def glUniform4iv(location: Int, count: Int, ptr: Array[Int]): Unit
  def glUniformMatrix2fv(location: Int, count: Int, transpose: Boolean, arg0: Array[Float]): Unit
  def glUniformMatrix3fv(location: Int, count: Int, transpose: Boolean, arg0: Array[Float]): Unit
  def glUniformMatrix4fv(location: Int, count: Int, transpose: Boolean, arg0: Array[Float]): Unit
  def glGetAttribLocation(pid: Int, name: String): Int
  def glGetUniformLocation(pid: Int, name: String): Int
  // def glGetIntegerv(name: Int, ptr: IntBuffer): Unit
  def glGenSamplers(num: Int, buffer: IntBuffer): Unit
  def glSamplerParameteri(sid: Int, name: Int, arg0: Int): Unit
  def glBindSampler(tid: Int, sid: Int): Unit
  def glCopyBufferSubData(readTarget: Int, writeTarget: Int, readOffset: Int, writeOffset: Int, size: Int): Unit
  def glBindVertexArray(vid: Int): Unit
  def glDrawBuffers(num: Int, buffers: IntBuffer): Unit
  def glReadBuffer(num: Int): Unit
  def glDrawElementsInstanced(mode: Int, count: Int, `type`: Int, offset: Int, primCount: Int): Unit
  def glDrawElementsInstanced(mode: Int, count: Int, `type`: Int, ptr: Buffer, primCount: Int): Unit
  def glBindAttribLocation(program: Int, index: Int, name: String): Unit
  def glBlendColor(red: Float, green: Float, blue: Float, alpha: Float): Unit
  // def glBlendEquation(mode: Int): Unit
  // def glBlendEquationSeparate(modeRGB: Int, modeAlpha: Int): Unit
  // def glBlendFunc(sfactor: Int, dfactor: Int): Unit
  // def glBlendFuncSeparate(sfactorRGB: Int, dfactorRGB: Int, sfactorAlpha: Int, dfactorAlpha: Int): Unit
  // def glClearDepthf(d: Float): Unit
  // def glClearStencil(s: Int): Unit
  // def glColorMask(red: Boolean, green: Boolean, blue: Boolean, alpha: Boolean): Unit
  // def glCompressedTexImage2D(target: Int, level: Int, internalformat: Int, width: Int, height: Int, border: Int, imageSize: Int, data: Buffer): Unit
  // def glCompressedTexSubImage2D(target: Int, level: Int, xoffset: Int, yoffset: Int, width: Int, height: Int, format: Int, imageSize: Int, data: Buffer): Unit
  // def glCopyTexImage2D(target: Int, level: Int, internalformat: Int, x: Int, y: Int, width: Int, height: Int, border: Int): Unit
  // def glCopyTexSubImage2D(target: Int, level: Int, xoffset: Int, yoffset: Int, x: Int, y: Int, width: Int, height: Int): Unit
  // def glCullFace(mode: Int): Unit
  // def glDeleteBuffers(n: Int, buffers: Array[Int]): Unit
  // def glDeleteFramebuffers(n: Int, framebuffers: Array[Int]): Unit
  // def glDeleteProgram(program: Int): Unit
  // def glDeleteRenderbuffers(n: Int, renderbuffers: Array[Int]): Unit
  // def glDeleteTextures(n: Int, textures: Array[Int]): Unit
  // def glDepthFunc(funcxbb: Int): Unit
  // def glDepthMask(flag: Boolean): Unit
  // def glDepthRangef(n: Float, f: Float): Unit
  // def glDetachShader(program: Int, shader: Int): Unit
  // def glDisableVertexAttribArray(index: Int): Unit
  // def glDrawElements(mode: Int, count: Int, `type`: Int, indices: Buffer): Unit
  // def glFinish(): Unit
  // def glFrontFace(mode: Int): Unit
  // def glGenerateMipmap(target: Int): Unit
  // def glGetActiveAttrib(program: Int, index: Int, bufSize: Int, length: IntBuffer, size: IntBuffer, `type`: IntBuffer, name: Buffer): Unit
  // def glGetActiveUniform(program: Int, index: Int, bufSize: Int, length: IntBuffer, size: IntBuffer, `type`: IntBuffer, name: Buffer): Unit
  // def glGetAttachedShaders(program: Int, maxCount: Int, count: IntBuffer, shaders: IntBuffer): Unit
  // def glGetBooleanv(pname: Int, data: IntBuffer): Unit
  // def glGetBufferParameteriv(target: Int, pname: Int, params: IntBuffer): Unit
  // def glGetFloatv(pname: Int, data: FloatBuffer): Unit
  // def glGetFramebufferAttachmentParameteriv(target: Int, attachment: Int, pname: Int, params: IntBuffer): Unit
  // def glGetProgramInfoLog(program: Int, bufSize: Int, length: IntBuffer, infoLog: Buffer): Unit
  // def glGetRenderbufferParameteriv(target: Int, pname: Int, params: IntBuffer): Unit
  // def glGetShaderInfoLog(shader: Int, bufSize: Int, length: IntBuffer, infoLog: Buffer): Unit
  // def glGetShaderPrecisionFormat(shadertype: Int, precisiontype: Int, range: IntBuffer, precision: IntBuffer): Unit
  // def glGetShaderSource(shader: Int, bufSize: Int, length: IntBuffer, source: Buffer): Unit
  // def glGetString(name: Int): String
  // def glGetTexParameterfv(target: Int, pname: Int, params: FloatBuffer): Unit
  // def glGetTexParameteriv(target: Int, pname: Int, params: IntBuffer): Unit
  // def glGetUniformfv(program: Int, location: Int, params: FloatBuffer): Unit
  // def glGetUniformiv(program: Int, location: Int, params: IntBuffer): Unit
  // def glGetVertexAttribPointerv(index: Int, pname: Int, pointer: Array[Buffer]): Unit
  // def glGetVertexAttribfv(index: Int, pname: Int, params: FloatBuffer): Unit
  // def glGetVertexAttribiv(index: Int, pname: Int, params: IntBuffer): Unit
  // def glHint(target: Int, mode: Int): Unit
  // def glIsBuffer(buffer: Int): Boolean
  // def glIsEnabled(cap: Int): Boolean
  // def glIsFramebuffer(framebuffer: Int): Boolean
  // def glIsProgram(program: Int): Boolean
  // def glIsRenderbuffer(renderbuffer: Int): Boolean
  // def glIsShader(shader: Int): Boolean
  // def glIsTexture(texture: Int): Boolean
  // def glLineWidth(width: Float): Unit
  // def glPolygonOffset(factor: Float, units: Float): Unit
  // def glReadPixels(x: Int, y: Int, width: Int, height: Int, format: Int, `type`: Int, pixels: Buffer): Unit
  // def glReleaseShaderCompiler(): Unit
  // def glSampleCoverage(value: Float, invert: Boolean): Unit
  // def glScissor(x: Int, y: Int, width: Int, height: Int): Unit
  // def glShaderBinary(count: Int, shaders: Array[Int], binaryformat: Int, binary: Buffer, length: Int): Unit
  // def glShaderSource(shader: Int, count: Int, string: Array[String], length: Array[Int]): Unit
  // def glStencilFunc(func: Int, ref: Int, mask: Int): Unit
  // def glStencilFuncSeparate(face: Int, func: Int, ref: Int, mask: Int): Unit
  // def glStencilMask(mask: Int): Unit
  // def glStencilMaskSeparate(face: Int, mask: Int): Unit
  // def glStencilOp(fail: Int, zfail: Int, zpass: Int): Unit
  // def glStencilOpSeparate(face: Int, sfail: Int, dpfail: Int, dppass: Int): Unit
  // def glTexParameterf(target: Int, pname: Int, param: Float): Unit
  // def glTexParameterfv(target: Int, pname: Int, params: Array[Float]): Unit
  // def glTexParameteriv(target: Int, pname: Int, params: Array[Int]): Unit
  // def glTexSubImage2D(target: Int, level: Int, xoffset: Int, yoffset: Int, width: Int, height: Int, format: Int, `type`: Int, pixels: Buffer): Unit
  // def glUniform1fv(location: Int, count: Int, value: Array[Float]): Unit
  // def glUniform1iv(location: Int, count: Int, value: Array[Int]): Unit
  // def glUniform2fv(location: Int, count: Int, value: Array[Float]): Unit
  // def glUniform2iv(location: Int, count: Int, value: Array[Int]): Unit
  // def glUniform3fv(location: Int, count: Int, value: Array[Float]): Unit
  // def glUniform3iv(location: Int, count: Int, value: Array[Int]): Unit
  // def glUniform4fv(location: Int, count: Int, value: Array[Float]): Unit
  // def glUniform4iv(location: Int, count: Int, value: Array[Int]): Unit
  // def glUniformMatrix2fv(location: Int, count: Int, transpose: Boolean, value: Array[Float]): Unit
  // def glUniformMatrix3fv(location: Int, count: Int, transpose: Boolean, value: Array[Float]): Unit
  // def glUniformMatrix4fv(location: Int, count: Int, transpose: Boolean, value: Array[Float]): Unit
  // def glValidateProgram(program: Int): Unit
  // def glVertexAttrib1f(index: Int, x: Float): Unit
  // def glVertexAttrib1fv(index: Int, v: Array[Float]): Unit
  // def glVertexAttrib2f(index: Int, x: Float, y: Float): Unit
  // def glVertexAttrib2fv(index: Int, v: Array[Float]): Unit
  // def glVertexAttrib3f(index: Int, x: Float, y: Float, z: Float): Unit
  // def glVertexAttrib3fv(index: Int, v: Array[Float]): Unit
  // def glVertexAttrib4f(index: Int, x: Float, y: Float, z: Float, w: Float): Unit
  // def glVertexAttrib4fv(index: Int, v: Array[Float]): Unit
  // def glVertexAttribPointer(index: Int, size: Int, `type`: Int, normalized: Boolean, stride: Int, pointer: Buffer): Unit
  // def glVertexAttribIPointer(index: Int, size: Int, `type`: Int, stride: Int, offset: Int): Unit
  // def glBeginQuery(target: Int, id: Int): Unit
  // def glBeginTransformFeedback(primitiveMode: Int): Unit
  // def glBindBufferBase(target: Int, index: Int, buffer: Int): Unit
  // def glBindBufferRange(target: Int, index: Int, buffer: Int, offset: Int, size: Int): Unit
  // def glBindTransformFeedback(target: Int, id: Int): Unit
  def glClearBufferfi(buffer: Int, drawbuffer: Int, depth: Float, stencil: Int): Unit
  def glClearBufferfv(buffer: Int, drawbuffer: Int, value: Array[Float]): Unit
  def glClearBufferiv(buffer: Int, drawbuffer: Int, value: Array[Int]): Unit
  def glClearBufferuiv(buffer: Int, drawbuffer: Int, value: Array[Int]): Unit
  // def glClientWaitSync(sync: Long, flags: Int, timeout: Long): Int
  // def glCompressedTexImage3D(target: Int, level: Int, internalformat: Int, width: Int, height: Int, depth: Int, border: Int, imageSize: Int, data: Buffer): Unit
  // def glCompressedTexSubImage3D(target: Int, level: Int, xoffset: Int, yoffset: Int, zoffset: Int, width: Int, height: Int, depth: Int, format: Int, imageSize: Int, data: Buffer): Unit
  // def glCopyTexSubImage3D(target: Int, level: Int, xoffset: Int, yoffset: Int, zoffset: Int, x: Int, y: Int, width: Int, height: Int): Unit
  // def glDeleteQueries(n: Int, ids: Array[Int]): Unit
  // def glDeleteSamplers(count: Int, samplers: Array[Int]): Unit
  // def glDeleteSync(sync: Long): Unit
  // def glDeleteTransformFeedbacks(n: Int, ids: Array[Int]): Unit
  // def glDeleteVertexArrays(n: Int, arrays: Array[Int]): Unit
  // def glDrawArraysInstanced(mode: Int, first: Int, count: Int, instancecount: Int): Unit
  // def glDrawElementsInstanced(mode: Int, count: Int, `type`: Int, indices: Buffer, instancecount: Int): Unit
  // def glDrawRangeElements(mode: Int, start: Int, end: Int, count: Int, `type`: Int, offset: Int): Unit
  // def glDrawRangeElements(mode: Int, start: Int, end: Int, count: Int, `type`: Int, indices: Buffer): Unit
  // def glEndQuery(target: Int): Unit
  // def glEndTransformFeedback(): Unit
  // def glFenceSync(condition: Int, flags: Int): Long
  // def glFlushMappedBufferRange(target: Int, offset: Int, length: Int): Unit
  // def glFramebufferTextureLayer(target: Int, attachment: Int, texture: Int, level: Int, layer: Int): Unit
  // def glGenQueries(n: Int, ids: IntBuffer): Unit
  // def glGenTransformFeedbacks(n: Int, ids: IntBuffer): Unit
  // def glGenVertexArrays(n: Int, arrays: IntBuffer): Unit
  // def glGetActiveUniformBlockName(program: Int, uniformBlockIndex: Int, bufSize: Int, legnth: IntBuffer, uniformBlockName: Buffer): Unit
  // def glGetActiveUniformBlockiv(program: Int, uniformBlockIndex: Int, pname: Int, params: IntBuffer): Unit
  // def glGetActiveUniformsiv(program: Int, uniformCount: Int, uniformIndices: Array[Int], pname: Int, params: IntBuffer): Unit
  // def glGetBufferParameteri64v(target: Int, pname: Int, params: LongBuffer): Unit
  // def glGetBufferPointerv(target: Int, pname: Int, params: Array[Buffer]): Unit
  // def glGetFragDataLocation(program: Int, name: String): Int
  // def glGetInteger64i_v(target: Int, index: Int, data: LongBuffer): Unit
  // def glGetInteger64v(pname: Int, data: LongBuffer): Unit
  // def glGetIntegeri_v(target: Int, index: Int, data: IntBuffer): Unit
  // def glGetInternalformativ(target: Int, internalformat: Int, pname: Int, bufSize: Int, params: IntBuffer): Unit
  // def glGetProgramBinary(program: Int, bufSize: Int, legnth: IntBuffer, binaryFormat: IntBuffer, binary: Buffer): Unit
  // def glGetQueryObjectuiv(id: Int, pname: Int, params: IntBuffer): Unit
  // def glGetQueryiv(target: Int, pname: Int, params: IntBuffer): Unit
  // def glGetSamplerParameterfv(sampler: Int, pname: Int, params: FloatBuffer): Unit
  // def glGetSamplerParameteriv(sampler: Int, pname: Int, params: IntBuffer): Unit
  // def glGetStringi(name: Int, index: Int): String
  // def glGetSynciv(sync: Long, pname: Int, bufSize: Int, legnth: IntBuffer, values: IntBuffer): Unit
  // def glGetTransformFeedbackVarying(program: Int, index: Int, bufSize: Int, legnth: IntBuffer, size: IntBuffer, `type`: IntBuffer, name: Buffer): Unit
  // def glGetUniformBlockIndex(program: Int, uniformBlockName: String): Int
  // def glGetUniformIndices(program: Int, uniformCount: Int, uniformNames: Array[String], uniformIndices: IntBuffer): Unit
  // def glGetUniformuiv(program: Int, location: Int, params: IntBuffer): Unit
  // def glGetVertexAttribIiv(index: Int, pname: Int, params: IntBuffer): Unit
  // def glGetVertexAttribIuiv(index: Int, pname: Int, params: IntBuffer): Unit
  // def glInvalidateFramebuffer(target: Int, numAttachments: Int, attachments: IntBuffer): Unit
  // def glInvalidateSubFramebuffer(target: Int, numAttachments: Int, attachments: IntBuffer, x: Int, y: Int, width: Int, height: Int): Unit
  // def glIsQuery(id: Int): Boolean
  // def glIsSampler(sampler: Int): Boolean
  // def glIsSync(sync: Long): Boolean
  // def glIsTransformFeedback(id: Int): Boolean
  // def glIsVertexArray(array: Int): Boolean
  // def glMapBufferRange(target: Int, offset: Int, length: Int, access: Int): Buffer
  // def glPauseTransformFeedback(): Unit
  // def glProgramBinary(program: Int, binaryFormat: Int, binary: Buffer, length: Int): Unit
  // def glProgramParameteri(program: Int, pname: Int, value: Int): Unit
  // def glRenderbufferStorageMultisample(target: Int, samples: Int, internalformat: Int, width: Int, height: Int): Unit
  // def glResumeTransformFeedback(): Unit
  // def glSamplerParameterf(sampler: Int, pname: Int, param: Float): Unit
  // def glSamplerParameterfv(sampler: Int, pname: Int, param: Array[Float]): Unit
  // def glSamplerParameteriv(sampler: Int, pname: Int, param: Array[Int]): Unit
  // def glTexImage3D(target: Int, level: Int, internalformat: Int, width: Int, height: Int, depth: Int, border: Int, format: Int, `type`: Int, pixels: Buffer): Unit
  // def glTexStorage2D(target: Int, levels: Int, internalformat: Int, width: Int, height: Int): Unit
  // def glTexStorage3D(target: Int, levels: Int, internalformat: Int, width: Int, height: Int, depth: Int): Unit
  // def glTexSubImage3D(target: Int, level: Int, xoffset: Int, yoffset: Int, zoffset: Int, width: Int, height: Int, depth: Int, format: Int, `type`: Int, pixels: Buffer): Unit
  // def glTransformFeedbackVaryings(program: Int, count: Int, varyings: Array[String], bufferMode: Int): Unit
  // def glUniform1ui(location: Int, v0: Int): Unit
  // def glUniform1uiv(location: Int, count: Int, value: Array[Int]): Unit
  // def glUniform2ui(location: Int, v0: Int, v1: Int): Unit
  // def glUniform2uiv(location: Int, count: Int, value: Array[Int]): Unit
  // def glUniform3ui(location: Int, v0: Int, v1: Int, v2: Int): Unit
  // def glUniform3uiv(location: Int, count: Int, value: Array[Int]): Unit
  // def glUniform4ui(location: Int, v0: Int, v1: Int, v2: Int, v3: Int): Unit
  // def glUniform4uiv(location: Int, count: Int, value: Array[Int]): Unit
  // def glUniformBlockBinding(program: Int, uniformBlockIndex: Int, uniformBlockBinding: Int): Unit
  // def glUniformMatrix2x3fv(location: Int, count: Int, transpose: Boolean, value: Array[Float]): Unit
  // def glUniformMatrix2x4fv(location: Int, count: Int, transpose: Boolean, value: Array[Float]): Unit
  // def glUniformMatrix3x2fv(location: Int, count: Int, transpose: Boolean, value: Array[Float]): Unit
  // def glUniformMatrix3x4fv(location: Int, count: Int, transpose: Boolean, value: Array[Float]): Unit
  // def glUniformMatrix4x2fv(location: Int, count: Int, transpose: Boolean, value: Array[Float]): Unit
  // def glUniformMatrix4x3fv(location: Int, count: Int, transpose: Boolean, value: Array[Float]): Unit
  // def glUnmapBuffer(target: Int): Boolean
  // def glVertexAttribDivisor(index: Int, divisor: Int): Unit
  // def glVertexAttribI4i(index: Int, x: Int, y: Int, z: Int, w: Int): Unit
  // def glVertexAttribI4iv(index: Int, v: Array[Int]): Unit
  // def glVertexAttribI4ui(index: Int, x: Int, y: Int, z: Int, w: Int): Unit
  // def glVertexAttribI4uiv(index: Int, v: Array[Int]): Unit
  // def glVertexAttribIPointer(index: Int, size: Int, `type`: Int, stride: Int, pointer: Buffer): Unit
  // def glWaitSync(sync: Long, flags: Int, timeout: Long): Unit
}
