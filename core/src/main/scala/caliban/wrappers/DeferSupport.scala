package caliban.wrappers

import caliban.GraphQLAspect
import caliban.execution.Feature
import caliban.introspection.adt.__Directive

object DeferSupport {
  @deprecated("Use Feature.Defer.directives instead", "2.9.0")
  private[caliban] val deferDirective: __Directive = Feature.Defer.directives.head

  @deprecated("Use IncrementalDelivery.defer instead", "2.9.0")
  val defer: GraphQLAspect[Nothing, Any] = IncrementalDelivery.defer
}
