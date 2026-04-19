package caliban.federation.v2x

import caliban.federation.connect.{
  BatchSettings,
  Connect,
  ConnectHTTP,
  ConnectorErrors,
  HTTPHeaderMapping,
  JSONSelection,
  Source
}
import caliban.schema.Annotations.GQLDirective

// IMPORTANT: Skips FederationV2_10 because we want to override the types from it.
trait FederationDirectivesV2_11 extends FederationDirectivesV2_9 {
  case class GQLConnect(
    http: ConnectHTTP,
    selection: JSONSelection,
    source: Option[String] = None,
    entity: Option[Boolean] = None,
    batch: Option[BatchSettings] = None,
    errors: Option[ConnectorErrors] = None
  ) extends GQLDirective(Connect(http, selection, source, entity))

  case class GQLSource(
    name: String,
    baseURL: String,
    headers: List[HTTPHeaderMapping] = Nil
  ) extends GQLDirective(Source(name, baseURL, headers))
}
