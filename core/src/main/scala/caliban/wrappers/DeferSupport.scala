package caliban.wrappers

import caliban.execution.Feature
import caliban.introspection.adt.{ __Directive, __DirectiveLocation, __InputValue }
import caliban.schema.Types
import caliban.{ GraphQL, GraphQLAspect }

object DeferSupport {
  private[caliban] val defer = __Directive(
    "defer",
    Some(""),
    Set(__DirectiveLocation.FRAGMENT_SPREAD, __DirectiveLocation.INLINE_FRAGMENT),
    List(__InputValue("if", None, () => Types.boolean, None), __InputValue("label", None, () => Types.string, None))
  )

  private[caliban] val stream = __Directive(
    "stream",
    Some(""),
    Set(__DirectiveLocation.FIELD),
    List(
      __InputValue("if", None, () => Types.boolean, None),
      __InputValue("label", None, () => Types.string, None),
      __InputValue("initialCount", None, () => Types.int, None)
    )
  )

  val deferSupport = new GraphQLAspect[Nothing, Any] {
    override def apply[R](gql: GraphQL[R]): GraphQL[R] =
      gql.withAdditionalDirectives(List(defer)).enable(Feature.Defer)
  }

  val streamSupport = new GraphQLAspect[Nothing, Any] {
    override def apply[R](gql: GraphQL[R]): GraphQL[R] =
      gql.withAdditionalDirectives(List(stream)).enable(Feature.Stream)
  }

}
