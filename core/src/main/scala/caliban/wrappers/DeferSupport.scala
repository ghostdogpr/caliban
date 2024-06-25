package caliban.wrappers

import caliban.GraphQLAspect
import caliban.execution.Feature
import caliban.introspection.adt.__Directive

object DeferSupport {
  @deprecated("Use IncrementalDelivery.deferDirective instead", "2.9.0")
  private[caliban] val deferDirective: __Directive = IncrementalDelivery.deferDirective

  @deprecated("Use IncrementalDelivery.aspect(Feature.Defer) instead", "2.9.0")
  val defer: GraphQLAspect[Nothing, Any] = IncrementalDelivery.aspect(Feature.Defer)
}
