package caliban.interop.play

import caliban.Value.{ BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue }
import caliban.introspection.adt.__Type
import caliban.parsing.adt.LocationInfo
import caliban.schema.Step.QueryStep
import caliban.schema.Types.makeScalar
import caliban.schema.{ ArgBuilder, PureStep, Schema, Step }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue, ResponseValue, Value }
import play.api.libs.json.{ JsPath, JsValue, Json, JsonValidationError, Reads, Writes }
import play.api.libs.functional.syntax._
import zio.ZIO
import zio.query.ZQuery

import scala.util.Try

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark play-json dependency as optional and keep Writes defined in the companion object.
 */
private[caliban] trait IsPlayJsonWrites[F[_]]
private[caliban] object IsPlayJsonWrites {
  implicit val isPlayJsonWrites: IsPlayJsonWrites[Writes] = null
}

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark play-json dependency as optional and keep Reads defined in the companion object.
 */
private[caliban] trait IsPlayJsonReads[F[_]]
private[caliban] object IsPlayJsonReads {
  implicit val isPlayJsonReads: IsPlayJsonReads[Reads] = null
}

object json {
  implicit val jsonSchema: Schema[Any, JsValue]    = new Schema[Any, JsValue] {
    private def parse(value: JsValue) =
      implicitly[Reads[ResponseValue]]
        .reads(value)
        .asEither
        .left
        .map(parsingException)

    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = makeScalar("Json")
    override def resolve(value: JsValue): Step[Any]                        =
      QueryStep(ZQuery.fromEffect(ZIO.fromEither(parse(value))).map(PureStep))
  }
  implicit val jsonArgBuilder: ArgBuilder[JsValue] = (input: InputValue) => Right(Json.toJson(input))

  private[caliban] def parsingException(
    errs: scala.collection.Seq[(JsPath, scala.collection.Seq[JsonValidationError])]
  ) =
    new Throwable(s"Couldn't decode json: $errs")

  private[caliban] object ValuePlayJson {
    import play.api.libs.json._

    val valueWrites: Writes[Value] = Writes {
      case NullValue           => JsNull
      case v: IntValue         =>
        v match {
          case IntValue.IntNumber(value)    => JsNumber(BigDecimal(value))
          case IntValue.LongNumber(value)   => JsNumber(BigDecimal(value))
          case IntValue.BigIntNumber(value) => JsNumber(BigDecimal(value))
        }
      case v: FloatValue       =>
        v match {
          case FloatValue.FloatNumber(value)      => JsNumber(BigDecimal(value.toDouble))
          case FloatValue.DoubleNumber(value)     => JsNumber(BigDecimal(value))
          case FloatValue.BigDecimalNumber(value) => JsNumber(value)
        }
      case StringValue(value)  => JsString(value)
      case BooleanValue(value) => JsBoolean(value)
      case EnumValue(value)    => JsString(value)
    }

    private def jsonToInputValue(json: JsValue): InputValue =
      json match {
        case JsObject(fields)  => InputValue.ObjectValue(fields.map { case (k, v) => k -> jsonToInputValue(v) }.toMap)
        case JsArray(elements) => InputValue.ListValue(elements.toList.map(jsonToInputValue))
        case JsString(value)   => StringValue(value)
        case JsNumber(value)   =>
          Try(value.toIntExact)
            .map(IntValue.apply)
            .getOrElse(FloatValue(value))

        case b: JsBoolean => BooleanValue(b.value)
        case JsNull       => NullValue
      }

    val inputValueReads: Reads[InputValue]   = Reads(json => JsSuccess(jsonToInputValue(json)))
    val inputValueWrites: Writes[InputValue] = Writes {
      case value: Value                   => valueWrites.writes(value)
      case InputValue.ListValue(values)   => JsArray(values.map(inputValueWrites.writes))
      case InputValue.ObjectValue(fields) =>
        JsObject(fields.map { case (k, v) => k -> inputValueWrites.writes(v) })
      case InputValue.VariableValue(name) => JsString(name)
    }

    private def jsonToResponseValue(json: JsValue): ResponseValue =
      json match {
        case JsObject(fields)  => responseObjectValueFromFields(fields)
        case JsArray(elements) => ResponseValue.ListValue(elements.toList.map(jsonToResponseValue))
        case JsString(value)   => StringValue(value)
        case JsNumber(value)   =>
          Try(value.toIntExact)
            .map(IntValue.apply)
            .getOrElse(FloatValue(value))

        case b: JsBoolean => BooleanValue(b.value)
        case JsNull       => NullValue
      }

    def responseObjectValueFromFields(fields: scala.collection.Map[String, JsValue]) =
      ResponseValue.ObjectValue(fields.map { case (k, v) =>
        k -> jsonToResponseValue(v)
      }.toList)

    val responseObjectValueReads: Reads[ResponseValue.ObjectValue] =
      Reads(json =>
        json match {
          case JsObject(fields) => JsSuccess(responseObjectValueFromFields(fields))
          case _                => JsError("not a json object")
        }
      )

    val responseValueReads: Reads[ResponseValue] =
      Reads(json => JsSuccess(jsonToResponseValue(json)))

    val responseValueWrites: Writes[ResponseValue] = Writes {
      case value: Value                      => valueWrites.writes(value)
      case ResponseValue.ListValue(values)   => JsArray(values.map(responseValueWrites.writes))
      case ResponseValue.ObjectValue(fields) =>
        JsObject(fields.map { case (k, v) => k -> responseValueWrites.writes(v) })
      case s: ResponseValue.StreamValue      => JsString(s.toString)
    }

    val responseObjectValueWrites: Writes[ResponseValue.ObjectValue] = Writes { v =>
      JsObject(v.fields.map { case (k, v) => k -> responseValueWrites.writes(v) })
    }
  }

  private[caliban] object ErrorPlayJson {
    import play.api.libs.json._

    private final case class ErrorDTO(
      message: String,
      extensions: Option[ResponseValue.ObjectValue],
      locations: Option[List[LocationInfo]],
      path: Option[JsArray]
    )

    implicit val locationInfoWrites: Writes[LocationInfo] =
      Json.writes[LocationInfo].transform((v: JsValue) => Json.arr(v))

    implicit val responseObjectValueWrites: Writes[ResponseValue.ObjectValue] = ValuePlayJson.responseObjectValueWrites
    private implicit val errorDTOWrites                                       = Json.writes[ErrorDTO]

    val errorValueWrites: Writes[CalibanError] = errorDTOWrites.contramap[CalibanError] {
      case CalibanError.ParsingError(msg, locationInfo, _, extensions) =>
        ErrorDTO(s"Parsing Error: $msg", extensions, locationInfo.map(List(_)), None)

      case CalibanError.ValidationError(msg, _, locationInfo, extensions) =>
        ErrorDTO(msg, extensions, locationInfo.map(List(_)), None)

      case CalibanError.ExecutionError(msg, path, locationInfo, _, extensions) =>
        ErrorDTO(
          msg,
          extensions,
          locationInfo.map(List(_)),
          Some(path).collect {
            case p if p.nonEmpty =>
              JsArray(p.map {
                case Left(value)  => JsString(value)
                case Right(value) => JsNumber(value)
              })
          }
        )
    }

    private implicit val locationInfoReads: Reads[LocationInfo]                     = Json.reads[LocationInfo]
    private implicit val responseObjectValueReads: Reads[ResponseValue.ObjectValue] =
      ValuePlayJson.responseObjectValueReads
    private implicit val errorDTOReads: Reads[ErrorDTO]                             = Json.reads[ErrorDTO]
    val errorValueReads: Reads[CalibanError]                                        = Json
      .reads[ErrorDTO]
      .map(e =>
        CalibanError.ExecutionError(
          msg = e.message,
          path = e.path
            .getOrElse(JsArray())
            .value
            .toList
            .map {
              case JsString(s)  => Left(s)
              case JsNumber(bd) => Right(bd.toInt)
              case _            => throw new Exception("invalid json")
            },
          locationInfo = e.locations.flatMap(_.lift(0)),
          None,
          e.extensions
        )
      )
  }

  private[caliban] object GraphQLResponsePlayJson {
    import play.api.libs.json._
    import play.api.libs.json.Json.toJson

    val graphQLResponseWrites: Writes[GraphQLResponse[Any]] = Writes {
      case GraphQLResponse(data, Nil, None)                => Json.obj("data" -> data)
      case GraphQLResponse(data, Nil, Some(extensions))    =>
        Json.obj("data" -> data, "extensions" -> extensions.asInstanceOf[ResponseValue])
      case GraphQLResponse(data, errors, None)             =>
        Json.obj("data" -> data, "errors" -> JsArray(errors.map(handleError)))
      case GraphQLResponse(data, errors, Some(extensions)) =>
        Json.obj(
          "data"       -> data,
          "errors"     -> JsArray(errors.map(handleError)),
          "extensions" -> extensions.asInstanceOf[ResponseValue]
        )
    }

    private def handleError(err: Any): JsValue =
      err match {
        case ce: CalibanError => toJson(ce)
        case _                => Json.obj("message" -> err.toString)
      }

    implicit val errorReads                                        = ErrorPlayJson.errorValueReads
    val graphQLResponseReads: Reads[GraphQLResponse[CalibanError]] =
      (JsPath \ "data")
        .read[ResponseValue]
        .and(
          (JsPath \ "errors")
            .read[List[CalibanError]]
        )
        .tupled
        .map({ case (data, errors) =>
          GraphQLResponse[CalibanError](
            data = data,
            errors = errors
          )
        })
  }

  private[caliban] object GraphQLRequestPlayJson {
    import play.api.libs.json._

    val graphQLRequestReads: Reads[GraphQLRequest]   = Json.reads[GraphQLRequest]
    val graphQLRequestWrites: Writes[GraphQLRequest] = Json.writes[GraphQLRequest]
  }

}
