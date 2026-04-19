package caliban.federation.connect

case class ConnectorErrors(
  message: Option[JSONSelection],
  extensions: Option[JSONSelection]
)
