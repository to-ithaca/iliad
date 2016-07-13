package iliad

import scala.annotation.{StaticAnnotation, compileTimeOnly}

import scala.reflect.macros.whitebox

import cats.data._

@compileTimeOnly("enable macro paradise to expand macro annotations")
final class jna[T] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BridgeInstanceMacro.mkTpe
}

@compileTimeOnly("enable macro paradise to expand macro annotations")
final class bridge[T] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro BridgeMacro.mkTpe
}

import scala.reflect.macros.whitebox

trait SymbolMacro {
  val c: whitebox.Context
  import c.universe._

  lazy val objectMethods: Set[Symbol] = weakTypeOf[Object].members.toSet

  def methodSymbolParamTree(m: MethodSymbol): List[Tree] = {
    m.paramLists.flatMap(_.map(t =>
              q"""val ${t.asTerm.name}: ${t.asTerm.info}"""))
  }

  def annotatedType: Type = {
    c.prefix.tree match {
      case q"new $_[$tpt]()" =>
        c.typecheck(q"val dummy: $tpt = null; dummy").tpe
      case _ => c.abort(c.enclosingPosition, "Unexpected tree type")
    }
  }
}

final class BridgeInstanceMacro(val c: whitebox.Context) extends SymbolMacro {

  import c.universe._

  def mkMethod(t: Tree)(m: MethodSymbol): Tree = {
    val params = methodSymbolParamTree(m)
    val arguments = m.paramLists.map(_.map(_.name))
    q"""def ${m.name}(..$params): ${m.returnType} = $t.${m.name}(...$arguments)"""
  }

  def instanceTerm(instance: Type, companion: Type): TermSymbol = {
    companion.members.find(m =>
          m.isPublic && m.isTerm && m.asTerm.typeSignature == instance) match {
      case Some(term) => term.asTerm
      case None =>
        c.abort(c.enclosingPosition, "cannot find instance value to bind to")
    }
  }

  def asTermSymbol(instance: Type, term: TermSymbol): Tree = {
    instance match {
      case TypeRef(_, ClassSymbolTag(cls), _) =>
        val split = cls.fullName.split("\\.")
        split.tail.foldLeft(Xor.left[TermName, Select](TermName(split.head))) {
          (b, a) =>
            b match {
              case Xor.Right(sel) => Xor.Right(Select(sel, TermName(a)))
              case Xor.Left(tm) => Xor.Right(Select(Ident(tm), TermName(a)))
            }
        } match {
          case Xor.Left(tm) => q"""$tm.$term"""
          case Xor.Right(sel) => q"""$sel.$term"""
        }
    }
  }

  def mkMethods(instance: Type, companion: Type): Tree = {

    val term = instanceTerm(instance, companion)
    val t = asTermSymbol(instance, term)

    mkTrees(instance)(
        m =>
          m.isMethod && m.isPublic &&
            !m.isConstructor && !deprecated(m) && !objectMethods.contains(m),
        s => mkMethod(t)(s.asMethod)
    )
  }

  def mkTrees(companion: Type)(f: Symbol => Boolean, g: Symbol => Tree): Tree =
    q"""..${companion.members.filter(f).map(g)}"""
  def deprecated(s: Symbol): Boolean = s.annotations exists { a =>
    c.typecheck(a.tree).tpe.typeSymbol == symbolOf[Deprecated]
  }
  def errorMsg(s: TypeName): String =
    s"Cannot find Static for interface to bind to for ${s.decodedName}"

  def addMethods(name: TypeName)(newTpeGen: Tree => Tree): Tree = {
    val tpe = annotatedType
    val companion = tpe.companion
    newTpeGen(q"""..${mkMethods(tpe, companion)}""")
  }

  def mkTpe(annottees: Expr[Any]*): Tree = {
    annottees.map(_.tree) match {
      case List(q"""abstract trait $name extends $parent with ..$traits""") =>
        addMethods(name)(methods =>
              q"""trait $name extends $parent with ..$traits { ..$methods }""")
      case List(q"""abstract trait $name extends $parent""") =>
        addMethods(name)(methods =>
              q"""trait $name extends $parent { ..$methods }""")
      case _ =>
        c.abort(c.enclosingPosition, "Can only bind to empty trait")
    }
  }
}


final class BridgeMacro(val c: whitebox.Context) extends SymbolMacro {

  import c.universe._

  def mkMethod(t: Type)(m: MethodSymbol): Tree = {
    val params = methodSymbolParamTree(m)
    val arguments = m.paramLists.map(_.map(_.name))
    q"""def ${m.name}(..$params): ${m.returnType} = $t.${m.name}(...$arguments)"""
  }

  def mkMethods(companion: Type): Tree = {
    mkTrees(companion)(
      m => m.isMethod && m.isPublic && !m.isConstructor && !deprecated(m) && !objectMethods.contains(m),
      s => mkMethod(companion)(s.asMethod)
    )
  }

  def mkTrees(companion: Type)(f: Symbol => Boolean, g: Symbol => Tree): Tree =
    q"""..${companion.members.filter(f).map(g)}"""
  def deprecated(s: Symbol): Boolean = s.annotations exists { a =>
    c.typecheck(a.tree).tpe.typeSymbol == symbolOf[Deprecated] }
  def errorMsg(s: TypeName): String =
    s"Cannot find Static for interface to bind to for ${s.decodedName}"

  def addMethods(name: TypeName)(newTpeGen: Tree => Tree): Tree = {
    val tpe = annotatedType
    val companion = tpe.companion
    newTpeGen(q"""..${mkMethods(companion)}""")
  }

  def mkTpe(annottees: Expr[Any]*): Tree = {
    annottees.map(_.tree) match {
      case List(q"""abstract trait $name extends $parent with ..$traits""") =>
        addMethods(name)(methods => q"""trait $name extends $parent with ..$traits { ..$methods }""")
      case List(q"""abstract trait $name extends $parent""") =>
        addMethods(name)(methods => q"""trait $name extends $parent { ..$methods }""")
      case _ =>
        c.abort(c.enclosingPosition, "Can only bind to empty trait")
    }
  }
}
