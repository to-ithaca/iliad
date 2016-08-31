package iliad
package gl

trait GLTextureFormat[A] {
  def format: Texture.Format
}

object GLTextureFormat {
  implicit val glTextureFormatRGBA: GLTextureFormat[TextureFormat.RGBA] = new GLTextureFormatRGBA
  implicit val glTextureFormatRGB: GLTextureFormat[TextureFormat.RGB] = new GLTextureFormatRGB
}

private final class GLTextureFormatRGBA extends GLTextureFormat[TextureFormat.RGBA] {
  val format: Texture.Format = Texture.Format.rgba
}

private final class GLTextureFormatRGB extends GLTextureFormat[TextureFormat.RGB] {
  val format: Texture.Format = Texture.Format.rgb
}
