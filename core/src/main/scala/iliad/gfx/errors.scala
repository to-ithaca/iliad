package iliad
package gfx

import iliad.{gl => GL}

sealed trait GraphicsError extends IliadError
case class UnsetUniformError(d: Draw.Instance) extends GraphicsError {
  def message: String =
    s"""Uniforms have not been set for draw instance:
    $d"""
}

sealed trait ConstructError extends GraphicsError
case class DuplicateLinkError(duplicates: Set[Set[Link]])
    extends ConstructError {
  private def msg: String =
    duplicates.map(ls => s"Group: ${ls.mkString(", ")}").mkString("\n")
  def message: String =
    s"""The following groups of links are duplicates - they have the same start and end nodes.
Note that only one link is allowed per start / end pair.
$msg
"""
}
case class NonUniqueNodeError(ns: Set[Set[Node.Constructed]])
    extends ConstructError {
  private def names: String =
    ns.map(_.map(_.constructor.name).head).mkString(", ")
  def message: String =
    s"The following groups of nodes have non-unique names: $names"
}

//FIXME: clear nodes can be off screen
case class OffScreenEndNodesError(ns: Set[Node.Constructed])
    extends ConstructError {
  private def names: String = ns.map(_.constructor.name).mkString(", ")
  def message: String =
    s"""The following end nodes render off screen: $names.  
Note that all end nodes must render to the screen"""
}

case class PipeFromScreenError(p: Link.Pipe) extends ConstructError {
  def message: String =
    s"""The following pipe is from the screen: $p.  
The screen has no outputs, so cannot be piped to anything"""
}
case class PipeHasUnmatchedTexturesError(p: Link.Pipe,
                                         ts: Set[Texture.Constructor])
    extends ConstructError {
  private def msg: String = ts.map(_.name).mkString(", ")
  def message: String =
    s"""The following pipe references textures which are not present in its end node:
$p
Missing textures are $msg
"""
}

case class PipeHasUnmatchedUniformsError(p: Link.Pipe, us: Set[String])
    extends ConstructError {
  private def msg: String = us.mkString(", ")
  def message: String =
    s"""The following pipe references uniforms which are not present in its start node: 
$p
Missing uniforms are $msg
"""
}

sealed trait InstantiationError extends GraphicsError
case class NodeInstantiationError(d: Draw.Instance, e: InstantiationError)
    extends InstantiationError {
  def message: String =
    s"""The following draw has an error: $d
Error: $e"""
}

case class RenderbufferMatchError(c: Renderbuffer.Constructor,
                                  i: Renderbuffer.Instance)
    extends InstantiationError {
  def message: String =
    s"""Renderbuffer constructor does not match instance constructor:
Constructor: $c
Instance constructor: ${i.constructor}
Instance: $i
"""
}
case class TextureMatchError(c: Texture.Constructor, i: Texture.Instance)
    extends InstantiationError {
  def message: String =
    s"""Texture constructor does not match instance constructor:
Constructor: $c
Instance constructor: ${i.constructor}
Instance: $i
"""
}
case class TextureRenderbufferMatchError(c: Texture.Constructor,
                                         i: Renderbuffer.Instance)
    extends InstantiationError {
  def message: String =
    s"""Renderbuffer instance supplied for texture constructor:
Renderbuffer instance: $i
Texture constructor: $c
"""
}
case class RenderbufferTextureMatchError(c: Renderbuffer.Constructor,
                                         i: Texture.Instance)
    extends InstantiationError {
  def message: String =
    s"""Texture instance supplied for renderbuffer constructor:
Texture instance: $i
Renderbuffer constructor: $c
"""
}
case class OffScreenOnScreenMatchError(i: Framebuffer.OffScreenInstance)
    extends InstantiationError {
  def message: String =
    s"Off screen instance supplied for on screen framebuffer: $i"
}
case class OnScreenOffScreenMatchError(c: Framebuffer.OffScreenConstructor)
    extends InstantiationError {
  def message: String = s"Screen supplied for off screen framebuffer: $c"
}
case class AttachmentMissingError(a: GL.FramebufferAttachment)
    extends InstantiationError {
  def message: String = s"Missing framebuffer attachment: $a"
}

case class NumInstanceError(instanced: Boolean, numInstances: Int)
    extends InstantiationError {
  def message: String = s"""Invalid number of instances supplied for draw.
Draw is instanced: $instanced
Number of instances: $numInstances.
Note that the number of instances must be non-zero and must be 1 for non-instanced draws.
"""
}

case class TextureUniformMissingError(uniform: String)
    extends InstantiationError {
  def message: String = s"Missing texture uniform $uniform"
}
case class AttributeMissingError(a: GL.Attribute.Constructor)
    extends InstantiationError {
  def message: String = s"Missing attribute $a"
}
case class EndNodeMissingError(l: Link, s: Node.Instance)
    extends InstantiationError {
  def message: String = s"""The following link is missing an end node:
Link: $l
Start node: $s
"""
}
case class StartNodeMissingError(l: Link, e: Node.Instance)
    extends InstantiationError {
  def message: String = s"""The following link is missing a start node:
Link: $l
End node: $e
"""
}
