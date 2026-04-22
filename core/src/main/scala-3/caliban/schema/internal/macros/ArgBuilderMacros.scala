package caliban.schema.internal.macros

import caliban.schema.ArgBuilder
import hearth.MacroCommonsScala3
import hearth.ScalaVersion

import scala.quoted.*

final private[schema] class ArgBuilderMacros(q: Quotes)
    extends MacroCommonsScala3(using q),
      ArgBuilderAnnotationSupportScala3,
      ArgBuilderMacrosImpl {

  override protected def summonArgBuilderExpr[A: Type](excluded: UntypedMethod*): Either[String, Expr[ArgBuilder[A]]] =
    if (implicitly[Ordering[ScalaVersion]].gteq(Environment.currentScalaVersion, ScalaVersion(3, 7, 0)))
      Type[ArgBuilder[A]].summonExprIgnoring(excluded: _*).toEither
    else
      fallbackSummonArgBuilderExpr[A](excluded*)

  private def fallbackSummonArgBuilderExpr[A: Type](excluded: UntypedMethod*): Either[String, Expr[ArgBuilder[A]]] = {
    import quotes.reflect.*

    val excludedSymbols = excluded.iterator.map(_.symbol).toSet

    def resolvedSymbol(term: Term): Symbol = term match {
      case Inlined(_, _, inner) => resolvedSymbol(inner)
      case Block(_, inner)      => resolvedSymbol(inner)
      case Typed(inner, _)      => resolvedSymbol(inner)
      case Apply(fun, _)        => resolvedSymbol(fun)
      case TypeApply(fun, _)    => resolvedSymbol(fun)
      case _                    => term.symbol
    }

    Type[ArgBuilder[A]].summonExpr.toEither.flatMap { expr =>
      val symbol = resolvedSymbol(expr.asTerm)
      if (symbol != Symbol.noSymbol && excludedSymbols.contains(symbol))
        Left(s"Ignored implicit value of type ${Type[ArgBuilder[A]].prettyPrint} found")
      else
        Right(expr)
    }
  }
}

private[schema] object ArgBuilderMacros {

  def deriveTypeClassImpl[A: Type](using q: Quotes): Expr[ArgBuilder[A]] =
    new ArgBuilderMacros(q).deriveTypeClass[A]
}
