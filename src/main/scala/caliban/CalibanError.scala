package caliban

sealed trait CalibanError extends Throwable {
  def msg: String
  def innerThrowable: Option[Throwable]
}

object CalibanError {

  case class ParsingError(msg: String, innerThrowable: Option[Throwable] = None)   extends CalibanError
  case class ExecutionError(msg: String, innerThrowable: Option[Throwable] = None) extends CalibanError

}
