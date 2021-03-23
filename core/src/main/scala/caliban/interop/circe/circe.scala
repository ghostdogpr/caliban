package caliban.interop.circe

import caliban.Value.{ BooleanValue, EnumValue, FloatValue, IntValue, NullValue, StringValue }
import caliban.introspection.adt.__Type
import caliban.parsing.adt.LocationInfo
import caliban.schema.Step.QueryStep
import caliban.schema.Types.makeScalar
import caliban.schema.{ ArgBuilder, PureStep, Schema, Step }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse, InputValue, ResponseValue, Value }
import io.circe._
import zio.ZIO
import zio.query.ZQuery

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark circe dependency as optional and keep Encoders defined in the companion object.
 */
private[caliban] trait IsCirceEncoder[F[_]]
private[caliban] object IsCirceEncoder {
  implicit val isCirceEncoder: IsCirceEncoder[Encoder] = null
}

/**
 * This class is an implementation of the pattern described in https://blog.7mind.io/no-more-orphans.html
 * It makes it possible to mark circe dependency as optional and keep Decoders defined in the companion object.
 */
private[caliban] trait IsCirceDecoder[F[_]]
private[caliban] object IsCirceDecoder {
  implicit val isCirceDecoder: IsCirceDecoder[Decoder] = null
}

object json {
  implicit val jsonSchema: Schema[Any, Json]    = new Schema[Any, Json] {
    override def toType(isInput: Boolean, isSubscription: Boolean): __Type = makeScalar("Json")
    override def resolve(value: Json): Step[Any]                           =
      QueryStep(ZQuery.fromEffect(ZIO.fromEither(Decoder[ResponseValue].decodeJson(value))).map(PureStep))
  }
  implicit val jsonArgBuilder: ArgBuilder[Json] = (input: InputValue) => Right(Encoder[InputValue].apply(input))

  private[caliban] object ValueCirce {
    import io.circe._
    val valueEncoder: Encoder[Value]                           = Encoder
      .instance[Value]({
        case NullValue           => Json.Null
        case v: IntValue         =>
          v match {
            case IntValue.IntNumber(value)    => Json.fromInt(value)
            case IntValue.LongNumber(value)   => Json.fromLong(value)
            case IntValue.BigIntNumber(value) => Json.fromBigInt(value)
          }
        case v: FloatValue       =>
          v match {
            case FloatValue.FloatNumber(value)      => Json.fromFloatOrNull(value)
            case FloatValue.DoubleNumber(value)     => Json.fromDoubleOrNull(value)
            case FloatValue.BigDecimalNumber(value) => Json.fromBigDecimal(value)
          }
        case StringValue(value)  => Json.fromString(value)
        case BooleanValue(value) => Json.fromBoolean(value)
        case EnumValue(value)    => Json.fromString(value)
      })
    private def jsonToInputValue(json: Json): InputValue       =
      json.fold(
        NullValue,
        BooleanValue,
        number =>
          number.toBigInt.map(IntValue.apply) orElse
            number.toBigDecimal.map(FloatValue.apply) getOrElse
            FloatValue(number.toDouble),
        StringValue,
        array => InputValue.ListValue(array.toList.map(jsonToInputValue)),
        obj => InputValue.ObjectValue(obj.toMap.map { case (k, v) => k -> jsonToInputValue(v) })
      )
    val inputValueDecoder: Decoder[InputValue]                 = Decoder.instance(hcursor => Right(jsonToInputValue(hcursor.value)))
    val inputValueEncoder: Encoder[InputValue]                 = Encoder
      .instance[InputValue]({
        case value: Value                   => valueEncoder.apply(value)
        case InputValue.ListValue(values)   => Json.arr(values.map(inputValueEncoder.apply): _*)
        case InputValue.ObjectValue(fields) =>
          Json.obj(fields.map { case (k, v) => k -> inputValueEncoder.apply(v) }.toList: _*)
        case InputValue.VariableValue(name) => Json.fromString(name)
      })
    private def jsonToResponseValue(json: Json): ResponseValue =
      json.fold(
        NullValue,
        BooleanValue,
        number =>
          number.toBigInt.map(IntValue.apply) orElse
            number.toBigDecimal.map(FloatValue.apply) getOrElse
            FloatValue(number.toDouble),
        StringValue,
        array => ResponseValue.ListValue(array.toList.map(jsonToResponseValue)),
        obj => ResponseValue.ObjectValue(obj.toList.map { case (k, v) => k -> jsonToResponseValue(v) })
      )
    val responseValueDecoder: Decoder[ResponseValue]           =
      Decoder.instance(hcursor => Right(jsonToResponseValue(hcursor.value)))
    val responseValueEncoder: Encoder[ResponseValue]           = Encoder
      .instance[ResponseValue]({
        case value: Value                      => valueEncoder.apply(value)
        case ResponseValue.ListValue(values)   => Json.arr(values.map(responseValueEncoder.apply): _*)
        case ResponseValue.ObjectValue(fields) =>
          Json.obj(fields.map { case (k, v) => k -> responseValueEncoder.apply(v) }: _*)
        case s: ResponseValue.StreamValue      => Json.fromString(s.toString)
      })
  }

  private[caliban] object ErrorCirce {
    import io.circe._
    import io.circe.syntax._

    private def locationToJson(li: LocationInfo): Json =
      Json.obj("line" -> li.line.asJson, "column" -> li.column.asJson)

    val errorValueEncoder: Encoder[CalibanError]       = Encoder.instance[CalibanError] {
      case CalibanError.ParsingError(msg, locationInfo, _, extensions)         =>
        Json
          .obj(
            "message"    -> s"Parsing Error: $msg".asJson,
            "locations"  -> Some(locationInfo).collect { case Some(li) =>
              Json.arr(locationToJson(li))
            }.asJson,
            "extensions" -> (extensions: Option[ResponseValue]).asJson.dropNullValues
          )
          .dropNullValues
      case CalibanError.ValidationError(msg, _, locationInfo, extensions)      =>
        Json
          .obj(
            "message"    -> msg.asJson,
            "locations"  -> Some(locationInfo).collect { case Some(li) =>
              Json.arr(locationToJson(li))
            }.asJson,
            "extensions" -> (extensions: Option[ResponseValue]).asJson.dropNullValues
          )
          .dropNullValues
      case CalibanError.ExecutionError(msg, path, locationInfo, _, extensions) =>
        Json
          .obj(
            "message"    -> msg.asJson,
            "locations"  -> Some(locationInfo).collect { case Some(li) =>
              Json.arr(locationToJson(li))
            }.asJson,
            "path"       -> Some(path).collect {
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

  private[caliban] object GraphQLResponseCirce {
    import io.circe._
    import io.circe.syntax._
    val graphQLResponseEncoder: Encoder[GraphQLResponse[Any]] = Encoder
      .instance[GraphQLResponse[Any]] {
        case GraphQLResponse(data, Nil, None)                => Json.obj("data" -> data.asJson)
        case GraphQLResponse(data, Nil, Some(extensions))    =>
          Json.obj("data" -> data.asJson, "extensions" -> extensions.asInstanceOf[ResponseValue].asJson)
        case GraphQLResponse(data, errors, None)             =>
          Json.obj("data" -> data.asJson, "errors" -> Json.fromValues(errors.map(handleError)))
        case GraphQLResponse(data, errors, Some(extensions)) =>
          Json.obj(
            "data"       -> data.asJson,
            "errors"     -> Json.fromValues(errors.map(handleError)),
            "extensions" -> extensions.asInstanceOf[ResponseValue].asJson
          )
      }

    private def handleError(err: Any): Json =
      err match {
        case ce: CalibanError => ce.asJson
        case _                => Json.obj("message" -> Json.fromString(err.toString))
      }

  }

  private[caliban] object GraphQLRequestCirce {
    import io.circe._
    val graphQLRequestDecoder: Decoder[GraphQLRequest] = (c: HCursor) =>
      for {
        query         <- c.downField("query").as[Option[String]]
        operationName <- c.downField("operationName").as[Option[String]]
        variables     <- c.downField("variables").as[Option[Map[String, InputValue]]]
        extensions    <- c.downField("extensions").as[Option[Map[String, InputValue]]]
      } yield GraphQLRequest(query, operationName, variables, extensions)

  }

}
