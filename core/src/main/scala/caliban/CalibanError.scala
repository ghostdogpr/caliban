package caliban

import caliban.ResponseValue.ObjectValue
import caliban.interop.circe.{ IsCirceDecoder, IsCirceEncoder }
import caliban.parsing.adt.LocationInfo

/**
 * The base type for all Caliban errors.
 */
sealed trait CalibanError extends Throwable with Product with Serializable {
  def msg: String
  override def getMessage: String = msg
}

object CalibanError extends CalibanErrorJsonCompat {

  /**
   * Describes an error that happened while parsing a query.
   */
  case class ParsingError(
    msg: String,
    locationInfo: Option[LocationInfo] = None,
    innerThrowable: Option[Throwable] = None,
    extensions: Option[ObjectValue] = None
  ) extends CalibanError {
    override def toString: String    = s"Parsing Error: $msg ${innerThrowable.fold("")(_.toString)}"
    override def getCause: Throwable = innerThrowable.orNull
  }

  /**
   * Describes an error that happened while validating a query.
   */
  case class ValidationError(
    msg: String,
    explanatoryText: String,
    locationInfo: Option[LocationInfo] = None,
    extensions: Option[ObjectValue] = None
  ) extends CalibanError {
    override def toString: String = s"ValidationError Error: $msg"
  }

  /**
   * Describes an error that happened while executing a query.
   */
  case class ExecutionError(
    msg: String,
    path: List[Either[String, Int]] = Nil,
    locationInfo: Option[LocationInfo] = None,
    innerThrowable: Option[Throwable] = None,
    extensions: Option[ObjectValue] = None
  ) extends CalibanError {
    override def toString: String    = s"Execution Error: $msg ${innerThrowable.fold("")(_.toString)}"
    override def getCause: Throwable = innerThrowable.orNull
  }

  implicit def circeEncoder[F[_]](implicit ev: IsCirceEncoder[F]): F[CalibanError] =
    caliban.interop.circe.json.ErrorCirce.errorValueEncoder.asInstanceOf[F[CalibanError]]

  implicit def circeDecoder[F[_]](implicit ev: IsCirceDecoder[F]): F[CalibanError] =
    caliban.interop.circe.json.ErrorCirce.errorValueEncoder.asInstanceOf[F[CalibanError]]
}
