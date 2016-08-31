package iliad
package algebra

import scala.reflect.macros.whitebox

private[iliad] final class MatrixContextMacro(val c: whitebox.Context) {

  import c.universe._

  private def table(symbols: List[EmbeddedContextMacro.Symbol])
    : List[List[EmbeddedContextMacro.Symbol]] = {
    @annotation.tailrec
    def go(symbols: List[EmbeddedContextMacro.Symbol],
           acc: List[List[EmbeddedContextMacro.Symbol]])
      : List[List[EmbeddedContextMacro.Symbol]] =
      symbols.span(_ != EmbeddedContextMacro.NewLine) match {
        case (Nil, Nil) => acc
        case (row, Nil) => acc :+ row
        case (Nil, EmbeddedContextMacro.NewLine :: vs) => go(vs, acc)
        case (row, EmbeddedContextMacro.NewLine :: vs) => go(vs, acc :+ row)
        case _ =>
          c.abort(
              c.enclosingPosition,
              s"Error splitting mpfatrix into rows. Symbols $symbols have invalid row structure.")
      }

    go(symbols, Nil)
  }

  private def width(table: List[List[EmbeddedContextMacro.Symbol]]): Int =
    table.map(_.size).distinct match {
      case w :: Nil => w
      case ws =>
        c.abort(
            c.enclosingPosition,
            s"Error extracting matrix dimensions. Rows of $table have different widths $ws.")
    }

  private def extractPArgs[A](args: List[c.Expr[A]])(
      table: List[List[EmbeddedContextMacro.Symbol]]) = {
    @annotation.tailrec
    def go(args: List[c.Expr[A]],
           raw: List[EmbeddedContextMacro.Symbol],
           previous: List[Tree]): (List[c.Expr[A]], List[Tree]) = raw match {
      case EmbeddedContextMacro.Placeholder :: xs =>
        go(args.tail, xs, args.head.tree :: previous)
      case EmbeddedContextMacro.Literal(v) :: xs =>
        go(args, xs, c.parse(v) :: previous)
      case EmbeddedContextMacro.Embedded(ctx, v) :: xs =>
        go(args,
           xs,
           q"""_root_.scala.StringContext($v).${TermName(ctx)}()""" :: previous)
      case Nil => args -> previous.reverse
      case _ =>
        c.abort(c.enclosingPosition, "unexpected tree for matrix context")
    }

    val (_, pargs) = table.foldLeft(args -> List.empty[Tree]) {
      case ((args, acc), symbols) =>
        val (remainingArgs, row) = go(args, symbols, Nil)
        remainingArgs -> (acc ::: row)
    }
    pargs
  }

  def sized_impl[A](method: Tree)(args: Seq[c.Expr[A]]): Tree = {
    c.prefix.tree match {
      case q"""$_[$_](scala.StringContext.apply(..$ps))""" =>
        val raw = ps.map((c.eval[String] _) compose (c.Expr[String] _))
        val ast = EmbeddedContextMacro.ast(raw)
        val t = table(ast)
        val m = t.size
        val n = width(t)
        val pargs = extractPArgs(args.toList)(t)
        val matrix = TermName(c.freshName())
        q"""
        {          
          val $matrix = $method($n, $m)(_root_.scala.Vector(..$pargs))
          $matrix
        }"""
    }
  }

  def matrix_impl[A](args: c.Expr[A]*): Tree = sized_impl(q"_root_.iliad.algebra.Matrix.sized")(args)
  def ortho_matrix_impl[A](args: c.Expr[A]*): Tree = sized_impl(q"_root_.iliad.algebra.OrthoMatrix.sized")(args)
}
