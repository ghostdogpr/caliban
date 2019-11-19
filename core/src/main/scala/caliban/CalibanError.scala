package caliban

/**
 * The base type for all Caliban errors.
 */
sealed trait CalibanError extends Throwable {
  def msg: String
}

object CalibanError {

  /**
   * Describes an error that happened while parsing a query.
   */
  case class ParsingError(msg: String, innerThrowable: Option[Throwable] = None) extends CalibanError {
    override def toString: String = s"""Parsing error: $msg ${innerThrowable.map(_.toString).getOrElse("")}"""
  }

  /**
   * Describes an error that happened while validating a query.
   */
  case class ValidationError(msg: String, explanatoryText: String) extends CalibanError {
    override def toString: String = s"""Validation error: $msg $explanatoryText"""
  }

  /**
   * Describes an error that happened while executing a query.
   */
  case class ExecutionError(msg: String, fieldName: Option[String] = None, innerThrowable: Option[Throwable] = None)
      extends CalibanError {
    override def toString: String = {
      val field = fieldName.fold("")(f => s" on field '$f'")
      val inner = innerThrowable.fold("")(e => s"\n${e.toString}")
      s"Execution error$field: $msg$inner"
    }
  }
}
