package caliban.federation.v2x.connect

case class ConnectorErrors(
  message: Option[JSONSelection],
  extensions: Option[JSONSelection]
)
