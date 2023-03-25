package caliban.client

import caliban.client.__Value.__ObjectValue
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
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

  implicit val jsonCodec: JsonValueCodec[GraphQLResponse] = JsonCodecMaker.makeCirceLike[GraphQLResponse]

}
