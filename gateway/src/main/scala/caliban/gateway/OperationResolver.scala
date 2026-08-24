package caliban.gateway

import caliban.GraphQLRequest
import zio.ZIO

/**
 * Resolves canonical GraphQL text for an incoming request before parsing and validation.
 */
final class OperationResolver[-R] private[gateway] (
  private[gateway] val resolve: GraphQLRequest => ZIO[R, Throwable, String],
  private[gateway] val cacheable: Boolean
)

object OperationResolver {

  def apply[R](resolve: GraphQLRequest => ZIO[R, Throwable, String]): OperationResolver[R] =
    new OperationResolver(resolve, cacheable = true)

  /**
   * Creates a resolver whose operations must bypass operation caches.
   */
  def uncached[R](resolve: GraphQLRequest => ZIO[R, Throwable, String]): OperationResolver[R] =
    new OperationResolver(resolve, cacheable = false)
}
