package caliban

import caliban.ResponseValue.ObjectValue
import caliban.interop.circe.IsCirceEncoder
import caliban.interop.play.IsPlayJsonWrites
import caliban.interop.zio.IsZIOJsonEncoder
import caliban.parsing.adt.LocationInfo

/**
 * The base type for all Caliban errors.
 */
sealed trait CalibanError extends Throwable with Product with Serializable {
  def msg: String
  override def getMessage: String = msg
}

object CalibanError {

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

  implicit def playJsonWrites[F[_]](implicit ev: IsPlayJsonWrites[F]): F[CalibanError] =
    caliban.interop.play.json.ErrorPlayJson.errorValueWrites.asInstanceOf[F[CalibanError]]

  implicit def zioJsonEncoder[F[_]](implicit ev: IsZIOJsonEncoder[F]): F[CalibanError] =
    caliban.interop.zio.ErrorZioJson.errorValueEncoder.asInstanceOf[F[CalibanError]]
}
