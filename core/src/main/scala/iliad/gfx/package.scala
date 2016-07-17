package iliad

import shapeless._

package object gfx extends LoadFunctions 
    with ActionFunctions 
    with AnimationFunctions 
    with GraphFunctions {
  type Graphics = Animation :+: Load :+: Action :+: CNil
}
