package iliad
package algebra

trait MatrixContextSyntax {
  implicit class matrixContext[A](sc: StringContext) {
    def mat[A](args: A*): Any = macro MatrixContextMacro.matrix_impl[A]
  }
  implicit class orthoMatrixContext[A](sc: StringContext) {
    def ortho[A](args: A*): Any = macro MatrixContextMacro.ortho_matrix_impl[A]
  }
}
