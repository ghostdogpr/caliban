package caliban.client

import caliban.client.__Value.__ObjectValue
import com.github.plokhotnyuk.jsoniter_scala.core.{ JsonReader, JsonValueCodec, JsonWriter }
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

/**
 * Represents the result of a GraphQL query, containing a data object and a list of errors.
 */
case class GraphQLResponse(
  data: Option[__Value],
  errors: List[GraphQLResponseError] = Nil,
  extensions: Option[__ObjectValue] = None
)

object GraphQLResponse {

  private val decoder: JsonValueCodec[GraphQLResponse] = JsonCodecMaker.make[GraphQLResponse]
  private val encoder: JsonValueCodec[GraphQLResponse] = JsonCodecMaker.makeCirceLike[GraphQLResponse]

  implicit val jsonCodec: JsonValueCodec[GraphQLResponse] = new JsonValueCodec[GraphQLResponse] {
    override def decodeValue(in: JsonReader, default: GraphQLResponse): GraphQLResponse =
      decoder.decodeValue(in, default)
    override def encodeValue(x: GraphQLResponse, out: JsonWriter): Unit                 = encoder.encodeValue(x, out)
    override def nullValue: GraphQLResponse                                             = decoder.nullValue
  }

}
