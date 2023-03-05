package caliban.client.ws

import caliban.client.GraphQLRequest
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

case class GraphQLWSRequest(`type`: String, id: Option[String], payload: Option[GraphQLRequest])

object GraphQLWSRequest {
  implicit val graphQLWSRequestEncoder: JsonValueCodec[GraphQLWSRequest] =
    JsonCodecMaker.makeCirceLike[GraphQLWSRequest]
}
