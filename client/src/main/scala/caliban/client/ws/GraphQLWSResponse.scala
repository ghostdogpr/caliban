package caliban.client.ws

import caliban.client.__Value.__ObjectValue
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

case class GraphQLWSResponse(`type`: String, id: Option[String], payload: Option[__ObjectValue])

object GraphQLWSResponse {
  implicit val graphQLWSResponseEncoder: JsonValueCodec[GraphQLWSResponse] =
    JsonCodecMaker.makeCirceLike[GraphQLWSResponse]
}
