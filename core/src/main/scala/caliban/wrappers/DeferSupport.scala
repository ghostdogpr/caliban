package caliban.wrappers

import caliban.execution.Feature
import caliban.introspection.adt.{ __Directive, __DirectiveLocation, __InputValue }
import caliban.schema.Types
import caliban.{ GraphQL, GraphQLAspect }
import zio.Trace
import zio.stacktracer.TracingImplicits.disableAutoTrace

object DeferSupport {
  private[caliban] val deferDirective = __Directive(
    "defer",
    Some(""),
    Set(__DirectiveLocation.FRAGMENT_SPREAD, __DirectiveLocation.INLINE_FRAGMENT),
    _ =>
      List(__InputValue("if", None, () => Types.boolean, None), __InputValue("label", None, () => Types.string, None)),
    isRepeatable = false
  )

  val defer = new GraphQLAspect[Nothing, Any] {
    override def apply[R](gql: GraphQL[R]): GraphQL[R] =
      gql.withAdditionalDirectives(List(deferDirective)).enable(Feature.Defer)
  }
}
