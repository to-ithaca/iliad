package iliad
package syntax

trait MatrixContextSyntax {
  implicit class matrixContext[A](sc: StringContext) {
    def m(args: A*): Any = macro MatrixContextMacro.apply_impl[A]
  }
}
