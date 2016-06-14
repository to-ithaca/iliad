package iliad

import scala.languageFeature.experimental.macros
import scala.reflect.macros.blackbox

/**
  * Expands sealed traits at compile time
  */
object SealedEnum {
  def values[A]: Set[A] = macro SealedEnumMacro.values_impl[A]
}

final class SealedEnumMacro(val c: blackbox.Context) {
  import c.universe._
  def values_impl[A: c.WeakTypeTag]: Tree = {
    val t = symbolOf[A]
    if (!(t.isClass && t.asClass.isSealed)) {
      c.abort(
          c.enclosingPosition,
          "cannot get values of types which are not sealed and a class/trait")
    }
    val cses = t.asClass.knownDirectSubclasses.toList
    if (!cses.forall(_.isModuleClass)) {
      c.abort(c.enclosingPosition,
              "cannot get values of types which have non-object members")
    }
    val all = cses.map(sym => q"""${Ident(sym.name.toTermName)}""")
    q"""Set[$t](..$all)"""
  }
}
