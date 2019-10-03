package caliban.parsing

import contextual._

object QueryInterpolator extends Interpolator {

  type Output = String

  def contextualize(interpolation: StaticInterpolation): Seq[ContextType] = {
    val lit @ Literal(_, queryString) = interpolation.parts.head
    Parser.check(queryString).foreach(error => interpolation.abort(lit, 0, s"Not a valid GraphQL query: $error"))

    Nil
  }

  def evaluate(interpolation: RuntimeInterpolation): String = interpolation.literals.head

  implicit class QueryStringContext(sc: StringContext) {
    val query = Prefix(QueryInterpolator, sc)
  }
}
