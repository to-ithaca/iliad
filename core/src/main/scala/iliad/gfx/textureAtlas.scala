package iliad
package gfx

import cats._
import cats.data._
import cats.implicits._

import monocle._
import monocle.macros._
import monocle.function.all._
import monocle.syntax.all._
import monocle.std.map._

//TODO: reconcile this with data loading
case class LoadedImage(name: String, data: java.nio.Buffer, size: Int)
case class TextureAtlas(textures: Map[Rect[Int], LoadedImage])

object TextureAtlas extends LoadFunctions {

  lazy val empty: TextureAtlas = TextureAtlas(Map.empty)

  private val _textures: Lens[TextureAtlas, Map[Rect[Int], LoadedImage]] =
    GenLens[TextureAtlas](_.textures)

  def add(rect: Rect[Int], image: LoadedImage): State[TextureAtlas, Unit] =
    State.modify(_ &|-> _textures ^|-> at(rect) set Some(image))

  def load(t: Texture.Instance, atlas: TextureAtlas): Graphics = {
    val data = gl.Texture.GroupData(atlas.textures.mapValues(i =>
              gl.Texture.SingleData(i.data, i.size)))
    load(t, data)
  }
}
