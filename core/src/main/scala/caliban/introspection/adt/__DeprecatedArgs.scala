package caliban.introspection.adt

import scala.runtime.AbstractFunction1

case class __DeprecatedArgs(includeDeprecated: Option[Boolean] = None)

// NOTE: This object extends AbstractFunction1 to maintain binary compatibility for Scala 2.13.
// TODO: Remove inheritance in the next major version
object __DeprecatedArgs extends AbstractFunction1[Option[Boolean], __DeprecatedArgs] {
  val include: __DeprecatedArgs = __DeprecatedArgs(Some(true))

  def apply(v: Option[Boolean] = None): __DeprecatedArgs = new __DeprecatedArgs(v)
}
