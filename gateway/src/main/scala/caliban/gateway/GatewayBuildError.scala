package caliban.gateway

import scala.util.control.NoStackTrace

/**
 * Indicates that a [[Gateway]] could not be built.
 */
final class GatewayBuildError private[gateway] () extends NoStackTrace
