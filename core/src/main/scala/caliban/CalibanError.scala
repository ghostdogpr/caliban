package caliban

import caliban.ResponseValue.ObjectValue
import caliban.interop.circe.IsCirceEncoder
import caliban.interop.jsoniter.IsJsoniterCodec
import caliban.interop.play.IsPlayJsonWrites
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

  implicit def jsoniterEncoder[F[_]](implicit ev: IsJsoniterCodec[F]): F[CalibanError] =
    ErrorJsoniter.errorValueEncoder.asInstanceOf[F[CalibanError]]

  implicit def playJsonWrites[F[_]](implicit ev: IsPlayJsonWrites[F]): F[CalibanError] =
    ErrorPlayJson.errorValueWrites.asInstanceOf[F[CalibanError]]
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

private object ErrorJsoniter {
  import com.github.plokhotnyuk.jsoniter_scala.core._
  import com.github.plokhotnyuk.jsoniter_scala.macros._
  import caliban.interop.jsoniter.JsonValueEncoder

  implicit val eitherEncoder: JsonValueEncoder[Either[Int, String]] =
    new JsonValueEncoder[Either[Int, String]] {
      def encodeValue(x: Either[Int, String], out: JsonWriter): Unit =
        x.fold(out.writeVal, out.writeVal)
    }

  private final case class ErrorDTO(
    message: String,
    extensions: Option[ResponseValue],
    locations: List[LocationInfo],
    path: Option[List[Either[String, Int]]]
  )

  private val errorValueDTOCodec: JsonValueCodec[ErrorDTO] = JsonCodecMaker.make

  val errorValueEncoder: JsonValueEncoder[CalibanError] = new JsonValueEncoder[CalibanError] {
    def encodeValue(x: CalibanError, out: JsonWriter): Unit = {
      val errorDTO = x match {
        case CalibanError.ParsingError(msg, locationInfo, _, extensions) =>
          ErrorDTO(s"Parsing Error: $msg", extensions, locationInfo.toList, None)
        case CalibanError.ValidationError(msg, _, locationInfo, extensions) =>
          ErrorDTO(msg, extensions, locationInfo.toList, None)
        case CalibanError.ExecutionError(msg, path, locationInfo, _, extensions) =>
          ErrorDTO(
            msg,
            extensions,
            locationInfo.toList,
            Some(path).collect {
              case p if p.nonEmpty => p
            }
          )
      }
      errorValueDTOCodec.encodeValue(errorDTO, out)
    }
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
