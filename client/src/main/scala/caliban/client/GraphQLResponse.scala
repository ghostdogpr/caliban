package caliban.client

import caliban.client.__Value.__ObjectValue
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.{ CodecMakerConfig, JsonCodecMaker }

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse(
  data: Option[__Value],
  errors: List[GraphQLResponseError] = Nil,
  extensions: Option[__ObjectValue] = None
)

object GraphQLResponse {

  // `errors` is absent from successful responses, which `makeCirceLike` rejects since jsoniter-scala 2.43.0
  implicit val jsonCodec: JsonValueCodec[GraphQLResponse] = JsonCodecMaker.make(
    CodecMakerConfig
      .withTransientEmpty(false)
      .withTransientDefault(false)
      .withTransientNone(false)
      .withDiscriminatorFieldName(None)
      .withCirceLikeObjectEncoding(true)
  )

}
