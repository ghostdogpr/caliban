package caliban

sealed trait CalibanError extends Throwable {
  def msg: String
}

object CalibanError {

  case class ParsingError(msg: String, innerThrowable: Option[Throwable] = None) extends CalibanError {
    override def toString: String = s"""Parsing error: $msg ${innerThrowable.map(_.toString).getOrElse("")}"""
  }
  case class ValidationError(msg: String, explanatoryText: String) extends CalibanError {
    override def toString: String = s"""Validation error: $msg $explanatoryText"""
  }
  case class ExecutionError(msg: String, innerThrowable: Option[Throwable] = None) extends CalibanError {
    override def toString: String = s"""Execution error: $msg ${innerThrowable.map(_.toString).getOrElse("")}"""
  }

}
