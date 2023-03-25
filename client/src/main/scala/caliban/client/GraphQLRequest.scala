package caliban.client

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

/**
 * Represents a GraphQL request, containing a query and a map of variables.
 */
case class GraphQLRequest(query: String, variables: Map[String, __Value])

object GraphQLRequest {

  implicit val jsonEncoder: JsonValueCodec[GraphQLRequest] = JsonCodecMaker.makeCirceLike[GraphQLRequest]

}
