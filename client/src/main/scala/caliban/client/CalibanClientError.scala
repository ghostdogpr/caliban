package caliban.client

/**
 * The base type for all Caliban Client errors.
 */
sealed trait CalibanClientError extends Throwable with Product with Serializable {
  override def getMessage: String = toString
  def getFullMessage: String      = toString
}

object CalibanClientError {

  /**
   * An error while communicating with the backend (e.g. HTTP code 4xx or 5xx)
   */
  case class CommunicationError(msg: String, innerThrowable: Option[Throwable] = None) extends CalibanClientError {
    override def toString: String = s"Communication Error: $msg${innerThrowable.fold("")(t => " " + t.toString)}"
  }

  /**
   * An error while parsing the response from the backend
   */
  case class DecodingError(msg: String, innerThrowable: Option[Throwable] = None) extends CalibanClientError {
    override def toString: String = s"Decoding Error: $msg${innerThrowable.fold("")(t => " " + t.toString)}"
  }

  /**
   * A GraphQL error returned by the backend (e.g. failure during query parsing, validation or execution)
   */
  case class ServerError(errors: List[GraphQLResponseError]) extends CalibanClientError {

    override def toString: String = s"Server Error: ${errors.map(_.print).mkString("\n")}"

    override def getFullMessage = s"Server Error: ${errors.map(_.fullPrint).mkString("\n")}"

  }
}
