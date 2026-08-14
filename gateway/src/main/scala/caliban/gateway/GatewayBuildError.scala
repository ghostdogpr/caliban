package caliban.gateway

import scala.util.control.NoStackTrace

/**
 * Indicates that a [[Gateway]] could not be built.
 */
final class GatewayBuildError private[gateway] (private[gateway] val diagnostics: List[String]) extends NoStackTrace {
  override def getMessage: String = diagnostics.mkString("\n")
}

private[gateway] object GatewayBuildError {
  def apply(message: String): GatewayBuildError           = new GatewayBuildError(message :: Nil)
  def apply(diagnostics: List[String]): GatewayBuildError = new GatewayBuildError(diagnostics)
}
