package caliban

/**
 * Decouples [[AkkaHttpAdapter]] from specific json backend implementations
 */
trait WSMessage {
  def id: String
  def messageType: String
  def operationName: Option[String]
  def query: Option[String]
}
