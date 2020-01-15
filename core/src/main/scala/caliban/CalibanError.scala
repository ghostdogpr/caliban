package caliban

import caliban.parsing.adt.LocationInfo

/**
 * The base type for all Caliban errors.
 */
sealed trait CalibanError extends Throwable with Product with Serializable {
  def msg: String
}

object CalibanError {

  /**
   * Describes an error that happened while parsing a query.
   */
  case class ParsingError(msg: String, innerThrowable: Option[Throwable] = None) extends CalibanError {
    override def toString: String = s"""Parsing error: $msg ${innerThrowable.fold("")(_.toString)}"""
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
  case class ExecutionError(
    msg: String,
    path: List[Either[String, Int]] = Nil,
    locationInfo: Option[LocationInfo] = None,
    innerThrowable: Option[Throwable] = None
  ) extends CalibanError {
    override def toString: String = {
      val pathString = path.map {
        case Left(value)  => value
        case Right(value) => value.toString
      }.mkString(" > ")
      val field = if (pathString.isEmpty) "" else s" on field '$pathString'"
      val inner = innerThrowable.fold("")(e => s" with ${e.toString}")
      s"Execution error$field: $msg$inner"
    }
  }
}
