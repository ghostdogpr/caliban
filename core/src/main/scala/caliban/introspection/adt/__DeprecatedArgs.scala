package caliban.introspection.adt

import caliban.schema.Annotations.GQLDefault

case class __DeprecatedArgs(@GQLDefault("false") includeDeprecated: Boolean = false) {
  private[caliban] def filter[A](values: List[A])(isDeprecated: A => Boolean): List[A] =
    if (includeDeprecated) values else values.filterNot(isDeprecated)
}

object __DeprecatedArgs {
  val include: __DeprecatedArgs = __DeprecatedArgs(true)
}
