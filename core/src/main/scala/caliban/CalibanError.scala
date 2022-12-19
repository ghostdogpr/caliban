package caliban

import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, StringValue }
import caliban.interop.circe.{ IsCirceDecoder, IsCirceEncoder }
import caliban.interop.jsoniter.IsJsoniterCodec
import caliban.interop.zio.{ IsZIOJsonDecoder, IsZIOJsonEncoder }
import caliban.parsing.adt.LocationInfo

/**
 * The base type for all Caliban errors.
 */
sealed trait CalibanError extends Throwable with Product with Serializable {
  def msg: String
  override def getMessage: String = msg

  def toResponseValue: ResponseValue
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
    override def toString: String      = s"Parsing Error: $msg ${innerThrowable.fold("")(_.toString)}"
    override def getCause: Throwable   = innerThrowable.orNull
    def toResponseValue: ResponseValue =
      ObjectValue(
        List(
          "message"    -> Some(StringValue(s"Parsing Error: $msg")),
          "locations"  -> locationInfo.map(li => ListValue(List(li.toResponseValue))),
          "extensions" -> extensions
        ).collect { case (name, Some(v)) => name -> v }
      )
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
    override def toString: String      = s"ValidationError Error: $msg"
    def toResponseValue: ResponseValue =
      ObjectValue(
        List(
          "message"    -> Some(StringValue(msg)),
          "locations"  -> locationInfo.map(li => ListValue(List(li.toResponseValue))),
          "extensions" -> extensions
        ).collect { case (name, Some(v)) => name -> v }
      )
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
    override def toString: String      = s"Execution Error: $msg ${innerThrowable.fold("")(_.toString)}"
    override def getCause: Throwable   = innerThrowable.orNull
    def toResponseValue: ResponseValue =
      ObjectValue(
        List(
          "message"    -> Some(StringValue(msg)),
          "locations"  -> locationInfo.map(li => ListValue(List(li.toResponseValue))),
          "path"       -> Some(path).collect {
            case p if p.nonEmpty =>
              ListValue(
                p.map {
                  case Left(value)  => StringValue(value)
                  case Right(value) => IntValue(value)
                }
              )
          },
          "extensions" -> extensions
        ).collect { case (name, Some(v)) => name -> v }
      )
  }

  implicit def circeEncoder[F[_]](implicit ev: IsCirceEncoder[F]): F[CalibanError] =
    caliban.interop.circe.json.ErrorCirce.errorValueEncoder.asInstanceOf[F[CalibanError]]
  implicit def circeDecoder[F[_]](implicit ev: IsCirceDecoder[F]): F[CalibanError] =
    caliban.interop.circe.json.ErrorCirce.errorValueDecoder.asInstanceOf[F[CalibanError]]

  implicit def zioJsonEncoder[F[_]](implicit ev: IsZIOJsonEncoder[F]): F[CalibanError] =
    caliban.interop.zio.ErrorZioJson.errorValueEncoder.asInstanceOf[F[CalibanError]]
  implicit def zioJsonDecoder[F[_]](implicit ev: IsZIOJsonDecoder[F]): F[CalibanError] =
    caliban.interop.zio.ErrorZioJson.errorValueDecoder.asInstanceOf[F[CalibanError]]

  implicit def jsoniterCodec[F[_]](implicit ev: IsJsoniterCodec[F]): F[CalibanError] =
    caliban.interop.jsoniter.ErrorJsoniter.errorValueCodec.asInstanceOf[F[CalibanError]]
}
