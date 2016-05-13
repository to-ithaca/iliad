package iliad
package syntax

trait VectorContextSyntax {
  implicit class vectorContext[A](sc: StringContext) {
    def v(args: A*): Any = macro VectorContextMacro.apply_impl[A]
  }
}

object vector extends VectorContextSyntax
