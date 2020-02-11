package caliban.client

/**
 * The base type for all Caliban Client errors.
 */
sealed trait CalibanClientError extends Throwable with Product with Serializable {
  def msg: String
}

object CalibanClientError {

  case class CommunicationError(msg: String, innerThrowable: Option[Throwable] = None) extends CalibanClientError {
    override def toString: String = s"Communication Error: $msg ${innerThrowable.fold("")(_.toString)}"
  }

  case class DecodingError(msg: String, innerThrowable: Option[Throwable] = None) extends CalibanClientError {
    override def toString: String = s"Decoding Error: $msg ${innerThrowable.fold("")(_.toString)}"
  }

}
