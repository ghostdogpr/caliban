package caliban.gateway

import caliban.GraphQLRequest
import zio.ZIO

/**
 * Resolves canonical GraphQL text for an incoming request before parsing and validation.
 */
final class OperationResolver[-R] private[gateway] (
  private[gateway] val resolve: GraphQLRequest => ZIO[R, Throwable, String],
  private[gateway] val cacheBehavior: OperationHookCacheBehavior
)

object OperationResolver {

  /**
   * Creates a resolver whose stable discriminator can participate in operation cache keys.
   */
  def stable[R](
    discriminator: String
  )(resolve: GraphQLRequest => ZIO[R, Throwable, String]): OperationResolver[R] =
    new OperationResolver(resolve, OperationHookCacheBehavior.Stable(discriminator))

  /**
   * Creates a resolver whose operations must bypass operation caches.
   */
  def uncached[R](resolve: GraphQLRequest => ZIO[R, Throwable, String]): OperationResolver[R] =
    new OperationResolver(resolve, OperationHookCacheBehavior.Bypass)
}

private[gateway] sealed trait OperationHookCacheBehavior

private[gateway] object OperationHookCacheBehavior {
  final case class Stable(discriminator: String) extends OperationHookCacheBehavior
  case object Bypass                             extends OperationHookCacheBehavior
}
