package iliad

import cats._
import cats.implicits._

import scala.util.parsing.combinator._

private[iliad] object EmbeddedContextMacro extends JavaTokenParsers {

  sealed trait Symbol
  case class Literal(value: String) extends Symbol
  case class Embedded(prefix: String, value: String) extends Symbol
  case object NewLine extends Symbol
  case object Placeholder extends Symbol

  override val skipWhitespace = false

  val eol: Parser[String] = "\r\n".r | "\n".r
  val space: Parser[String] = " +".r

  val string: Parser[String] =
    "\"" ~> """([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*+""".r <~ "\""
  val prefix: Parser[String] = "^[A-z]+(?=\")".r

  val embedded: Parser[Embedded] =
    (prefix ~ string) ^^ {
      case p ~ v => Embedded(p, v)
    }

  val numeric: Parser[Literal] =
    (floatingPointNumber | decimalNumber | wholeNumber) ^^ {
      case s => Literal(s)
    }

  val line: Parser[List[Symbol]] = rep(
      space.? ~> (numeric | embedded) <~ space.?)
  val multiline: Parser[List[Symbol]] =
    repsep(line, eol) ~ eol.? <~ space.? ^^ {
      case (x :: xs) ~ eol =>
        val r = xs.map(NewLine :: _).foldLeft(x)(_ <+> _)
        eol.map(_ => r <+> List(NewLine)).getOrElse(r)
    }

  def ast(raw: List[String]): List[Symbol] =
    raw
      .foldLeft(List.empty[Symbol]) { (b, a) =>
        if (a.trim.isEmpty) {
          if (a.contains("\n")) b <+> List(Placeholder, NewLine)
          else b <+> List(Placeholder)
        } else
          b <+> Placeholder :: parseAll(multiline, a).getOrElse(
              sys.error(s"Could not expand string context ast for $a"))
      }
      .tail
}

import scala.reflect.macros.whitebox

private[iliad] final class VectorContextMacro(val c: whitebox.Context) {

  import c.universe._

  def apply_impl[A](args: c.Expr[A]*): Tree = {
    c.prefix.tree match {
      case q"""$_[$_](scala.StringContext.apply(..$ps))""" =>
        val raw = ps.map((c.eval[String] _) compose (c.Expr[String] _))
        val ast = EmbeddedContextMacro.ast(raw)
        val n = ast.size

        @annotation.tailrec
        def go(args: List[c.Expr[A]],
               raw: List[EmbeddedContextMacro.Symbol],
               previous: List[Tree]): List[Tree] = raw match {
          case EmbeddedContextMacro.Placeholder :: xs =>
            go(args.tail, xs, args.head.tree :: previous)
          case EmbeddedContextMacro.Literal(v) :: xs =>
            go(args, xs, c.parse(v) :: previous)
          case EmbeddedContextMacro.Embedded(ctx, v) :: xs =>
            go(args,
               xs,
               q"""_root_.scala.StringContext($v).${TermName(ctx)}()""" :: previous)
          case Nil => previous
          case _ =>
            c.abort(c.enclosingPosition, "unexpected tree for vector context")
        }

        val pargs = go(args.toList, ast, List.empty).reverse
        val vector = TermName(c.freshName())
        q"""
            {
                import _root_.shapeless._
                val $vector = _root_.iliad.VectorD.sized($n, _root_.scala.Vector(..$pargs))
                $vector
            }
        """
    }
  }
}
