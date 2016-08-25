package iliad
package gfx

import iliad.{gl => GL}

sealed trait GraphicsError extends IliadError
case class UnsetScopeError(s: UniformScope, existing: Set[UniformScope])
    extends GraphicsError {
  override def toString: String =
    s"""UnsetScopeError: Scope $s has not been set:
Unset scope: $s
Existing:
$existing
"""
}

case class UnsetUniformError(name: String, s: UniformScope)
    extends GraphicsError {
  override def toString: String =
    s"Uniform [$name] has not been set for scope [$s]"
}


sealed trait ConstructError extends GraphicsError
case class DuplicateLinkError(duplicates: Set[Set[Link]])
    extends ConstructError {
  private def msg: String =
    duplicates.map(ls => s"Group: ${ls.mkString(", ")}").mkString("\n")
  override def toString: String =
    s"""The following groups of links are duplicates - they have the same start and end nodes.
Note that only one link is allowed per start / end pair.
$msg
"""
}
case class NonUniqueNodeError(ns: Set[Set[Node.Constructed]])
    extends ConstructError {
  private def names: String =
    ns.map(_.map(_.constructor.name).head).mkString(", ")
  override def toString: String =
    s"The following groups of nodes have non-unique names: $names"
}

//FIXME: clear nodes can be off screen
case class OffScreenEndNodesError(ns: Set[Node.Constructed])
    extends ConstructError {
  private def names: String = ns.map(_.constructor.name).mkString(", ")
  override def toString: String =
    s"""The following end nodes render off screen: $names.
Note that all end nodes must render to the screen"""
}

case class PipeFromScreenError(p: Link.Pipe) extends ConstructError {
  override def toString: String =
    s"""The following pipe is from the screen: $p.
The screen has no outputs, so cannot be piped to anything"""
}
case class PipeHasUnmatchedTexturesError(p: Link.Pipe,
                                         ts: Set[Texture.Constructor])
    extends ConstructError {
  private def msg: String = ts.map(_.name).mkString(", ")
  override def toString: String =
    s"""The following pipe references textures which are not present in its end node:
Missing textures: [$msg]
Pipe: $p
"""
}

case class PipeHasUnmatchedUniformsError(p: Link.Pipe, us: Set[String])
    extends ConstructError {
  private def msg: String = us.mkString(", ")
  override def toString: String =
    s"""The following pipe references uniforms which are not present in its start node:
Missing uniforms: [$msg]
Pipe: $p
"""
}

sealed trait InstantiationError extends GraphicsError
case class NodeInstantiationError(d: Draw.Instance, e: InstantiationError)
    extends InstantiationError {

  override def toString: String =
    s"""The following draw has an error:
Error: $e
Draw: $d"""
}

case class RenderbufferMatchError(c: Renderbuffer.Constructor,
                                  i: Renderbuffer.Instance)
    extends InstantiationError {
  override def toString: String =
    s"""Renderbuffer constructor does not match instance constructor:
Constructor: $c
Instance constructor: ${i.constructor}
Instance: $i
"""
}
case class TextureMatchError(c: Texture.Constructor, i: Texture.Instance)
    extends InstantiationError {
  override def toString: String =
    s"""Texture constructor does not match instance constructor:
Constructor: $c
Instance constructor: ${i.constructor}
Instance: $i
"""
}
case class TextureRenderbufferMatchError(c: Texture.Constructor,
                                         i: Renderbuffer.Instance)
    extends InstantiationError {
  override def toString: String =
    s"""Renderbuffer instance supplied for texture constructor:
Renderbuffer instance: $i
Texture constructor: $c
"""
}
case class RenderbufferTextureMatchError(c: Renderbuffer.Constructor,
                                         i: Texture.Instance)
    extends InstantiationError {
  override def toString: String =
    s"""Texture instance supplied for renderbuffer constructor:
Texture instance: $i
Renderbuffer constructor: $c
"""
}
case class OffScreenOnScreenMatchError(i: Framebuffer.OffScreenInstance)
    extends InstantiationError {
  override def toString: String =
    s"Off screen instance supplied for on screen framebuffer: $i"
}
case class OnScreenOffScreenMatchError(c: Framebuffer.OffScreenConstructor)
    extends InstantiationError {
  override def toString: String = s"Screen supplied for off screen framebuffer: $c"
}
case class AttachmentMissingError(a: GL.FramebufferAttachment)
    extends InstantiationError {
  override def toString: String = s"Missing framebuffer attachment: $a"
}

case class NumInstanceError(instanced: Boolean, numInstances: Int)
    extends InstantiationError {
  override def toString: String = s"""Invalid number of instances supplied for draw.
Draw is instanced: $instanced
Number of instances: $numInstances.
Note that the number of instances must be non-zero and must be 1 for non-instanced draws.
"""
}

case class TextureUniformMissingError(uniform: String)
    extends InstantiationError {
  override def toString: String = s"Missing texture uniform $uniform"
}
case class AttributeMissingError(a: GL.Attribute.Constructor)
    extends InstantiationError {
  override def toString: String = s"Missing attribute: [$a]"
}

case class StartNodeMissingError(l: Link, e: Node.Instance)
    extends InstantiationError {
  override def toString: String = s"""The following pipe link is missing a start node:
Pipe link: $l
End node: $e
"""
}
