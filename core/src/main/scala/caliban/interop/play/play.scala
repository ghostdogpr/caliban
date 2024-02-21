package caliban.interop.play

import caliban.Value._
import caliban._
import caliban.introspection.adt.__Type
import caliban.parsing.adt.LocationInfo
import caliban.schema.Types.makeScalar
import caliban.schema.{ ArgBuilder, Schema, Step }
import play.api.libs.functional.syntax._
import play.api.libs.json._

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
    override def resolve(value: JsValue): Step[Any]                        = Step.fromEither(parse(value))
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
        case obj: JsObject     => responseObjectValueFromFields(obj.fields.toList)
        case JsArray(elements) => ResponseValue.ListValue(elements.toList.map(jsonToResponseValue))
        case JsString(value)   => StringValue(value)
        case JsNumber(value)   =>
          Try(value.toIntExact)
            .map(IntValue.apply)
            .getOrElse(FloatValue(value))

        case b: JsBoolean => BooleanValue(b.value)
        case JsNull       => NullValue
      }

    def responseObjectValueFromFields(fields: List[(String, JsValue)]): ResponseValue.ObjectValue =
      ResponseValue.ObjectValue(fields.map { case (k, v) =>
        k -> jsonToResponseValue(v)
      })

    val responseObjectValueReads: Reads[ResponseValue.ObjectValue] =
      Reads {
        case obj: JsObject => JsSuccess(responseObjectValueFromFields(obj.fields.toList))
        case _             => JsError("not a json object")
      }

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

    val errorValueWrites: Writes[CalibanError] =
      Writes(e => ValuePlayJson.responseValueWrites.writes(e.toResponseValue))

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
              case JsString(s)  => PathValue.Key(s)
              case JsNumber(bd) => PathValue.Index(bd.toInt)
              case _            => throw new Exception("invalid json")
            },
          locationInfo = e.locations.flatMap(_.headOption),
          None,
          e.extensions
        )
      )
  }

  private[caliban] object GraphQLResponsePlayJson {
    import play.api.libs.json._

    val graphQLResponseWrites: Writes[GraphQLResponse[Any]] =
      Writes(r => ValuePlayJson.responseValueWrites.writes(r.toResponseValue))

    implicit val errorReads: Reads[CalibanError]                   = ErrorPlayJson.errorValueReads
    val graphQLResponseReads: Reads[GraphQLResponse[CalibanError]] =
      (JsPath \ "data")
        .read[ResponseValue]
        .and(
          (JsPath \ "errors")
            .readWithDefault[List[CalibanError]](Nil)
        )
        .tupled
        .map { case (data, errors) =>
          GraphQLResponse[CalibanError](
            data = data,
            errors = errors
          )
        }
  }

  private[caliban] object GraphQLRequestPlayJson {
    import play.api.libs.json._

    val graphQLRequestReads: Reads[GraphQLRequest]   = Json.reads[GraphQLRequest]
    val graphQLRequestWrites: Writes[GraphQLRequest] = Json.writes[GraphQLRequest]
  }

  private[caliban] object GraphQLWSInputPlayJson {
    import play.api.libs.json._

    val graphQLWSInputReads: Reads[GraphQLWSInput]   = Json.reads[GraphQLWSInput]
    val graphQLWSInputWrites: Writes[GraphQLWSInput] = Json.writes[GraphQLWSInput]
  }

  private[caliban] object GraphQLWSOutputPlayJson {
    import play.api.libs.json._

    val graphQLWSOutputReads: Reads[GraphQLWSOutput]   = Json.reads[GraphQLWSOutput]
    val graphQLWSOutputWrites: Writes[GraphQLWSOutput] = Json.writes[GraphQLWSOutput]
  }

}
