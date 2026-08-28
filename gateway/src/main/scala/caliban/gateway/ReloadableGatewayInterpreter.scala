package caliban.gateway

import zio.{ Trace, UIO }

/**
 * A stable interpreter whose acquired schemas are refreshed within its owning scope.
 */
trait ReloadableGatewayInterpreter[-R] extends GatewayInterpreter[R] {

  /**
   * A bounded diagnostic for the latest failed refresh, cleared after a successful check.
   * Remote messages, schemas, response bodies and exception causes are never retained.
   */
  def lastReloadFailure(implicit trace: Trace): UIO[Option[String]]
}
