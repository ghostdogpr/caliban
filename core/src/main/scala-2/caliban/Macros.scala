package caliban

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

import caliban.parsing.Parser

object Macros {

  /**
   * Verifies at compile-time that the given string is a valid GraphQL document.
   * @param document a string representing a GraphQL document.
   */
  def gqldoc(document: String): String = macro MacrosInternal.queryLiteral

  private class MacrosInternal(val c: blackbox.Context) {
    import c.universe._
    def queryLiteral(document: c.Expr[String]): c.Expr[String] =
      document.tree match {
        case Literal(Constant(s: String)) =>
          Parser.check(s).fold(document)(e => c.abort(c.enclosingPosition, s"GraphQL document is invalid: $e"))
        case _                            =>
          c.abort(c.enclosingPosition, s"This macro can only be used with string literals.")
      }
  }
}
