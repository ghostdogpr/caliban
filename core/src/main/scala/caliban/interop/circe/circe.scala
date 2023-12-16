package caliban.interop.circe

import caliban.Value._
import caliban._
import caliban.introspection.adt.__Type
import caliban.parsing.adt.LocationInfo
import caliban.schema.Types.makeScalar
import caliban.schema.{ ArgBuilder, Schema, Step }
import io.circe._

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
    override def resolve(value: Json): Step[Any]                           = Step.fromEither(Decoder[ResponseValue].decodeJson(value))
  }
  implicit val jsonArgBuilder: ArgBuilder[Json] = (input: InputValue) => Right(Encoder[InputValue].apply(input))

  private[caliban] object ValueCirce {
    import io.circe._

    private def jsonToInputValue(json: Json): InputValue       =
      json.fold(
        NullValue,
        BooleanValue.apply,
        number =>
          number.toBigInt.map(IntValue.apply) orElse
            number.toBigDecimal.map(FloatValue.apply) getOrElse
            FloatValue(number.toDouble),
        StringValue.apply,
        array => InputValue.ListValue(array.toList.map(jsonToInputValue)),
        obj => InputValue.ObjectValue(obj.toMap.map { case (k, v) => k -> jsonToInputValue(v) })
      )
    val inputValueDecoder: Decoder[InputValue]                 = Decoder.instance(hcursor => Right(jsonToInputValue(hcursor.value)))
    val inputValueEncoder: Encoder[InputValue]                 = Encoder
      .instance[InputValue] {
        case NullValue                          => Json.Null
        case IntValue.IntNumber(value)          => Json.fromInt(value)
        case IntValue.LongNumber(value)         => Json.fromLong(value)
        case IntValue.BigIntNumber(value)       => Json.fromBigInt(value)
        case FloatValue.FloatNumber(value)      => Json.fromFloatOrNull(value)
        case FloatValue.DoubleNumber(value)     => Json.fromDoubleOrNull(value)
        case FloatValue.BigDecimalNumber(value) => Json.fromBigDecimal(value)
        case StringValue(value)                 => Json.fromString(value)
        case BooleanValue(value)                => Json.fromBoolean(value)
        case EnumValue(value)                   => Json.fromString(value)
        case InputValue.ListValue(values)       => Json.arr(values.map(inputValueEncoder.apply): _*)
        case InputValue.ObjectValue(fields)     =>
          Json.obj(fields.map { case (k, v) => k -> inputValueEncoder.apply(v) }.toList: _*)
        case InputValue.VariableValue(name)     => Json.fromString(name)
      }
    private def jsonToResponseValue(json: Json): ResponseValue =
      json.fold(
        NullValue,
        BooleanValue.apply,
        number =>
          number.toBigInt.map(IntValue.apply) orElse
            number.toBigDecimal.map(FloatValue.apply) getOrElse
            FloatValue(number.toDouble),
        StringValue.apply,
        array => ResponseValue.ListValue(array.toList.map(jsonToResponseValue)),
        obj => objToResponseValue(obj)
      )

    val responseObjectValueDecoder: Decoder[ResponseValue.ObjectValue] =
      Decoder[JsonObject].map(obj => objToResponseValue(obj))

    private def objToResponseValue(obj: JsonObject) = ResponseValue.ObjectValue(obj.toList.map { case (k, v) =>
      k -> jsonToResponseValue(v)
    })

    val responseValueDecoder: Decoder[ResponseValue] =
      Decoder.instance(hcursor => Right(jsonToResponseValue(hcursor.value)))
    val responseValueEncoder: Encoder[ResponseValue] = Encoder
      .instance[ResponseValue] {
        case NullValue                          => Json.Null
        case IntValue.IntNumber(value)          => Json.fromInt(value)
        case IntValue.LongNumber(value)         => Json.fromLong(value)
        case IntValue.BigIntNumber(value)       => Json.fromBigInt(value)
        case FloatValue.FloatNumber(value)      => Json.fromFloatOrNull(value)
        case FloatValue.DoubleNumber(value)     => Json.fromDoubleOrNull(value)
        case FloatValue.BigDecimalNumber(value) => Json.fromBigDecimal(value)
        case StringValue(value)                 => Json.fromString(value)
        case BooleanValue(value)                => Json.fromBoolean(value)
        case EnumValue(value)                   => Json.fromString(value)
        case ResponseValue.ListValue(values)    => Json.arr(values.map(responseValueEncoder.apply): _*)
        case ResponseValue.ObjectValue(fields)  =>
          Json.obj(fields.map { case (k, v) => k -> responseValueEncoder.apply(v) }: _*)
        case s: ResponseValue.StreamValue       => Json.fromString(s.toString)
      }
  }

  private[caliban] object ErrorCirce {
    import io.circe._
    import io.circe.syntax._

    val errorValueEncoder: Encoder[CalibanError] = Encoder.instance[CalibanError](_.toResponseValue.asJson)

    private implicit val locationInfoDecoder: Decoder[LocationInfo] = Decoder.instance(cursor =>
      for {
        column <- cursor.downField("column").as[Int]
        line   <- cursor.downField("line").as[Int]
      } yield LocationInfo(column, line)
    )

    private implicit val pathEitherDecoder: Decoder[Either[String, Int]] = Decoder.instance { cursor =>
      (cursor.as[String].toOption, cursor.as[Int].toOption) match {
        case (Some(s), _) => Right(Left(s))
        case (_, Some(n)) => Right(Right(n))
        case _            => Left(DecodingFailure("failed to decode as string or int", cursor.history))
      }
    }

    implicit val objectValueDecoder: Decoder[ResponseValue.ObjectValue] = ValueCirce.responseObjectValueDecoder

    implicit val errorValueDecoder: Decoder[CalibanError] = Decoder.instance(cursor =>
      for {
        message    <- cursor.downField("message").as[String]
        path       <- cursor.downField("path").as[Option[List[Either[String, Int]]]]
        locations  <- cursor.downField("locations").downArray.as[Option[LocationInfo]]
        extensions <- cursor.downField("extensions").as[Option[ResponseValue.ObjectValue]]
      } yield CalibanError.ExecutionError(
        message,
        path.getOrElse(Nil),
        locations,
        None,
        extensions
      )
    )
  }

  private[caliban] object GraphQLResponseCirce {
    import io.circe._
    import io.circe.syntax._

    val graphQLResponseEncoder: Encoder[GraphQLResponse[Any]] =
      Encoder.instance[GraphQLResponse[Any]](_.toResponseValue.asJson)

    implicit val graphQLResponseDecoder: Decoder[GraphQLResponse[CalibanError]] =
      Decoder.instance(cursor =>
        for {
          data   <- cursor
                      .downField("data")
                      .as[ResponseValue]
          errors <- cursor
                      .downField("errors")
                      .as[Option[List[CalibanError]]]
        } yield GraphQLResponse[CalibanError](
          data = data,
          errors = errors.getOrElse(List()),
          extensions = None
        )
      )
  }

  private[caliban] object GraphQLRequestCirce {
    import io.circe._
    import io.circe.syntax._

    val graphQLRequestDecoder: Decoder[GraphQLRequest] = (c: HCursor) =>
      for {
        query         <- c.downField("query").as[Option[String]]
        operationName <- c.downField("operationName").as[Option[String]]
        variables     <- c.downField("variables").as[Option[Map[String, InputValue]]]
        extensions    <- c.downField("extensions").as[Option[Map[String, InputValue]]]
      } yield GraphQLRequest(query, operationName, variables, extensions)

    implicit val graphQLRequestEncoder: Encoder[GraphQLRequest] =
      Encoder.instance[GraphQLRequest](r =>
        Json.obj(
          "query"         -> r.query.asJson,
          "operationName" -> r.operationName.asJson,
          "variables"     -> r.variables.asJson,
          "extensions"    -> r.extensions.asJson
        )
      )
  }

  private[caliban] object GraphQLWSInputCirce {
    import io.circe._
    import io.circe.syntax._

    implicit val graphQLWSInputEncoder: Encoder[GraphQLWSInput] =
      Encoder.instance[GraphQLWSInput](r =>
        Json.obj(
          "id"      -> r.id.asJson,
          "type"    -> r.`type`.asJson,
          "payload" -> r.payload.asJson
        )
      )

    implicit val graphQLWSInputDecoder: Decoder[GraphQLWSInput] =
      Decoder.instance(cursor =>
        for {
          t       <- cursor.downField("type").as[String]
          id      <- cursor.downField("id").as[Option[String]]
          payload <- cursor.downField("payload").as[Option[InputValue]]
        } yield GraphQLWSInput(`type` = t, id = id, payload = payload)
      )
  }

  private[caliban] object GraphQLWSOutputCirce {
    import io.circe._
    import io.circe.syntax._

    implicit val graphQLWSOutputEncoder: Encoder[GraphQLWSOutput] =
      Encoder.instance[GraphQLWSOutput](r =>
        Json.fromJsonObject(
          r.payload
            .foldLeft(
              JsonObject(
                "id"   -> r.id.asJson,
                "type" -> r.`type`.asJson
              )
            )((obj, v) => obj.add("payload", v.asJson))
        )
      )

    implicit val graphQLWSOutputDecoder: Decoder[GraphQLWSOutput] =
      Decoder.instance(cursor =>
        for {
          t       <- cursor.downField("type").as[String]
          id      <- cursor.downField("id").as[Option[String]]
          payload <- cursor.downField("payload").as[Option[ResponseValue]]
        } yield GraphQLWSOutput(`type` = t, id = id, payload = payload)
      )
  }
}
