package caliban.gateway.internal.composition

import caliban.{ GraphQLRequest, InputValue, ResponseValue }
import caliban.gateway.SupergraphAcquisitionError.InvalidUplinkResponse
import caliban.gateway.SupergraphAcquisitionError.InvalidUplinkResponse._
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ FloatValue, IntValue, NullValue, StringValue }

private[gateway] object ApolloUplinkClient {

  sealed trait RouterConfig

  object RouterConfig {
    final case class Success(id: String, supergraphSDL: Option[String], minDelaySeconds: Double) extends RouterConfig
    final case class Failed(code: String, message: String)                                       extends RouterConfig
  }

  val OperationName = "SupergraphSdl"

  val Query: String =
    s"""query $OperationName($$apiKey: String!, $$ref: String!, $$ifAfterId: ID) {
       |  routerConfig(apiKey: $$apiKey, ref: $$ref, ifAfterId: $$ifAfterId) {
       |    __typename
       |    ... on RouterConfigResult {
       |      id
       |      supergraphSDL
       |      minDelaySeconds
       |    }
       |    ... on FetchError {
       |      code
       |      message
       |    }
       |    ... on Unchanged {
       |      id
       |      minDelaySeconds
       |    }
       |  }
       |}""".stripMargin

  def request(apiKey: String, ref: String, ifAfterId: Option[String]): GraphQLRequest =
    GraphQLRequest(
      query = Some(Query),
      operationName = Some(OperationName),
      variables = Some(
        Map[String, InputValue](
          "apiKey"    -> StringValue(apiKey),
          "ref"       -> StringValue(ref),
          "ifAfterId" -> ifAfterId.fold[InputValue](NullValue)(StringValue(_))
        )
      )
    )

  def decode(response: ResponseValue): Either[InvalidUplinkResponse.Reason, RouterConfig] =
    response match {
      case envelope: ObjectValue =>
        val hasErrors = envelope.getOrNull("errors") match {
          case ListValue(values) => values.nonEmpty
          case _                 => false
        }
        envelope.getOrNull("data") match {
          case data: ObjectValue =>
            data.getOrNull("routerConfig") match {
              case value: ObjectValue => if (hasErrors) Left(MissingRouterConfig) else routerConfig(value)
              case _                  => Left(MissingRouterConfig)
            }
          case _                 => Left(if (hasErrors) MissingData else DecodingFailed)
        }
      case _                     => Left(DecodingFailed)
    }

  private def routerConfig(value: ObjectValue): Either[InvalidUplinkResponse.Reason, RouterConfig] =
    string(value, "__typename").toRight(DecodingFailed).flatMap {
      case "RouterConfigResult" =>
        for {
          id    <- string(value, "id").toRight(MissingId)
          sdl   <- string(value, "supergraphSDL").toRight(MissingSupergraphSdl)
          delay <- number(value, "minDelaySeconds").toRight(DecodingFailed)
        } yield RouterConfig.Success(id, Some(sdl), delay)
      case "FetchError"         =>
        for {
          code    <- string(value, "code").toRight(DecodingFailed)
          message <- string(value, "message").toRight(DecodingFailed)
        } yield RouterConfig.Failed(code, message)
      case "Unchanged"          =>
        for {
          id    <- string(value, "id").toRight(MissingId)
          delay <- number(value, "minDelaySeconds").toRight(DecodingFailed)
        } yield RouterConfig.Success(id, None, delay)
      case _                    => Left(UnknownTypename)
    }

  private def string(value: ObjectValue, field: String): Option[String] =
    value.getOrNull(field) match {
      case StringValue(result) => Some(result)
      case _                   => None
    }

  private def number(value: ObjectValue, field: String): Option[Double] =
    value.getOrNull(field) match {
      case result: IntValue   => Some(result.toBigInt.toDouble)
      case result: FloatValue => Some(result.toDouble)
      case _                  => None
    }
}
