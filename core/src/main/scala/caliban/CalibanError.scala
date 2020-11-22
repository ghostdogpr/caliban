package caliban

import caliban.ResponseValue.ObjectValue
import caliban.Value.{ IntValue, StringValue }
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
    ErrorCirce.errorValueEncoder.asInstanceOf[F[CalibanError]]

  implicit def playJsonWrites[F[_]](implicit ev: IsPlayJsonWrites[F]): F[CalibanError] =
    ErrorPlayJson.errorValueWrites.asInstanceOf[F[CalibanError]]

  implicit def zioJsonEncoder[F[_]](implicit ev: IsZIOJsonEncoder[F]): F[CalibanError] =
    ErrorZioJson.errorValueEncoder.asInstanceOf[F[CalibanError]]
}

private object ErrorCirce {
  import io.circe._
  import io.circe.syntax._

  private def locationToJson(li: LocationInfo): Json =
    Json.obj("line" -> li.line.asJson, "column" -> li.column.asJson)

  val errorValueEncoder: Encoder[CalibanError] = Encoder.instance[CalibanError] {
    case CalibanError.ParsingError(msg, locationInfo, _, extensions) =>
      Json
        .obj(
          "message" -> s"Parsing Error: $msg".asJson,
          "locations" -> Some(locationInfo).collect {
            case Some(li) => Json.arr(locationToJson(li))
          }.asJson,
          "extensions" -> (extensions: Option[ResponseValue]).asJson.dropNullValues
        )
        .dropNullValues
    case CalibanError.ValidationError(msg, _, locationInfo, extensions) =>
      Json
        .obj(
          "message" -> msg.asJson,
          "locations" -> Some(locationInfo).collect {
            case Some(li) => Json.arr(locationToJson(li))
          }.asJson,
          "extensions" -> (extensions: Option[ResponseValue]).asJson.dropNullValues
        )
        .dropNullValues
    case CalibanError.ExecutionError(msg, path, locationInfo, _, extensions) =>
      Json
        .obj(
          "message" -> msg.asJson,
          "locations" -> Some(locationInfo).collect {
            case Some(li) => Json.arr(locationToJson(li))
          }.asJson,
          "path" -> Some(path).collect {
            case p if p.nonEmpty =>
              Json.fromValues(p.map {
                case Left(value)  => value.asJson
                case Right(value) => value.asJson
              })
          }.asJson,
          "extensions" -> (extensions: Option[ResponseValue]).asJson.dropNullValues
        )
        .dropNullValues
  }

}

private object ErrorPlayJson {
  import play.api.libs.json._

  private final case class ErrorDTO(
    message: String,
    extensions: Option[ResponseValue],
    locations: Option[LocationInfo],
    path: Option[JsArray]
  )

  implicit val locationInfoWrites: Writes[LocationInfo] =
    Json.writes[LocationInfo].transform((v: JsValue) => Json.arr(v))

  private implicit val errorDTOWrites = Json.writes[ErrorDTO]

  val errorValueWrites: Writes[CalibanError] = errorDTOWrites.contramap[CalibanError] {
    case CalibanError.ParsingError(msg, locationInfo, _, extensions) =>
      ErrorDTO(s"Parsing Error: $msg", extensions, locationInfo, None)

    case CalibanError.ValidationError(msg, _, locationInfo, extensions) =>
      ErrorDTO(msg, extensions, locationInfo, None)

    case CalibanError.ExecutionError(msg, path, locationInfo, _, extensions) =>
      ErrorDTO(
        msg,
        extensions,
        locationInfo,
        Some(path).collect {
          case p if p.nonEmpty =>
            JsArray(p.map {
              case Left(value)  => JsString(value)
              case Right(value) => JsNumber(value)
            })
        }
      )
  }

}

private object ErrorZioJson {
  import zio.json._
  import zio.json.internal.Write

  private def locationToResponse(li: LocationInfo): ResponseValue =
    ResponseValue.ListValue(
      List(ResponseValue.ObjectValue(List("line" -> IntValue(li.line), "column" -> IntValue(li.column))))
    )

  private[caliban] def errorToResponseValue(e: CalibanError): ResponseValue =
    ResponseValue.ObjectValue(e match {
      case CalibanError.ParsingError(msg, locationInfo, _, extensions) =>
        List(
          "message"                                               -> StringValue(s"Parsing Error: $msg")
        ) ++ (extensions: Option[ResponseValue]).map("extensions" -> _) ++
          locationInfo.map(locationToResponse).map("locations"    -> _)
      case CalibanError.ValidationError(msg, _, locationInfo, extensions) =>
        List(
          "message"                                               -> StringValue(msg)
        ) ++ (extensions: Option[ResponseValue]).map("extensions" -> _) ++
          locationInfo.map(locationToResponse).map("locations"    -> _)
      case CalibanError.ExecutionError(msg, path, locationInfo, _, extensions) =>
        List(
          "message"                                               -> StringValue(msg)
        ) ++ (extensions: Option[ResponseValue]).map("extensions" -> _) ++
          locationInfo.map(locationToResponse).map("locations"    -> _) ++
          Some(path).collect {
            case p if p.nonEmpty =>
              "path" -> ResponseValue.ListValue(p.map {
                case Left(value)  => StringValue(value)
                case Right(value) => IntValue(value)
              })
          }
    })

  val errorValueEncoder: JsonEncoder[CalibanError] = (a: CalibanError, indent: Option[Int], out: Write) =>
    ValueZIOJson.responseValueEncoder.unsafeEncode(
      errorToResponseValue(a),
      indent,
      out
    )
}
