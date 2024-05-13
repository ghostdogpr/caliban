package caliban.introspection.adt

case class __DeprecatedArgs(includeDeprecated: Option[Boolean] = None)

object __DeprecatedArgs {
  val include: __DeprecatedArgs = __DeprecatedArgs(Some(true))
}
