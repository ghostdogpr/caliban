package caliban.introspection.adt

import caliban.schema.Annotations.GQLDefault

case class __DeprecatedArgs(@GQLDefault("false") includeDeprecated: Boolean = false)

object __DeprecatedArgs {
  val include: __DeprecatedArgs = __DeprecatedArgs(true)
}
