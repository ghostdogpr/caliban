package caliban

import scala.quoted._

import caliban.parsing.Parser

object Macros {

  /**
   * Verifies at compile-time that the given string is a valid GraphQL document.
   * @param document a string representing a GraphQL document.
   */
  inline def gqldoc(inline document: String): String = ${ gqldocImpl('document) }

  private def gqldocImpl(document: Expr[String])(using Quotes): Expr[String] = {
    import quotes.reflect.report
    document.value.fold(report.errorAndAbort("This macro can only be used with string literals."))(
      Parser.check(_).fold(document)(e => report.errorAndAbort(s"GraphQL document is invalid: $e"))
    )
  }
}
