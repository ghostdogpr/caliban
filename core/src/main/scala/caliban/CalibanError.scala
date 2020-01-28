package caliban

import caliban.interop.circe.IsCirceEncoder
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
  case class ParsingError(
    msg: String,
    locationInfo: Option[LocationInfo] = None,
    innerThrowable: Option[Throwable] = None
  ) extends CalibanError

  /**
   * Describes an error that happened while validating a query.
   */
  case class ValidationError(msg: String, explanatoryText: String, locationInfo: Option[LocationInfo] = None)
      extends CalibanError

  /**
   * Describes an error that happened while executing a query.
   */
  case class ExecutionError(
    msg: String,
    path: List[Either[String, Int]] = Nil,
    locationInfo: Option[LocationInfo] = None,
    innerThrowable: Option[Throwable] = None
  ) extends CalibanError

  implicit def circeEncoder[F[_]](implicit ev: IsCirceEncoder[F]): F[CalibanError] =
    ErrorCirce.errorValueEncoder.asInstanceOf[F[CalibanError]]
}

private object ErrorCirce {
  import io.circe._
  import io.circe.syntax._

  private def locationToJson(li: LocationInfo): Json =
    Json.obj("line" -> li.line.asJson, "column" -> li.column.asJson)

  val errorValueEncoder: Encoder[CalibanError] = Encoder.instance[CalibanError] {
    case CalibanError.ParsingError(msg, locationInfo, _) =>
      Json
        .obj(
          "message" -> s"Parsing Error: $msg".asJson,
          "locations" -> Some(locationInfo).collect {
            case Some(li) => Json.arr(locationToJson(li))
          }.asJson
        )
        .dropNullValues
    case CalibanError.ValidationError(msg, _, locationInfo) =>
      Json
        .obj(
          "message" -> msg.asJson,
          "locations" -> Some(locationInfo).collect {
            case Some(li) => Json.arr(locationToJson(li))
          }.asJson
        )
        .dropNullValues
    case CalibanError.ExecutionError(msg, path, locationInfo, _) =>
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
          }.asJson
        )
        .dropNullValues
  }

}
