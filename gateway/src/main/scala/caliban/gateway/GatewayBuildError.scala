package caliban.gateway

import scala.util.control.NoStackTrace

/**
 * Indicates that a [[Gateway]] could not be built.
 */
final class GatewayBuildError private[gateway] (message: String) extends NoStackTrace {
  override def getMessage: String = message
}

private[gateway] object GatewayBuildError {
  def apply(message: String): GatewayBuildError = new GatewayBuildError(message)
}
