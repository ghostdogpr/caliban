package caliban.interop.tapir

import caliban.{ GraphQLResponse, ResponseValue, Value }
import caliban.interop.tapir.TapirAdapterSpec.FakeServerRequest
import sttp.model.{ Header, MediaType, Method, Uri }
import zio.stream.ZStream
import zio.test._

object BuildHttpResponseSpec extends ZIOSpecDefault {

  import StreamConstructor.zioStreams

  private val graphqlResponseJson = MediaType("application", "graphql-response+json")
  private val uri                 = Uri.unsafeParse("http://localhost/api/graphql")

  private def mediaTypeOf[E](accept: Header, response: GraphQLResponse[E]): MediaType = {
    val req = FakeServerRequest(Method.POST, uri, List(accept))
    TapirAdapter.buildHttpResponse[E, ZStream[Any, Throwable, Byte]](req)(response)._1
  }

  private val subscriptionResponse =
    GraphQLResponse[Nothing](
      ResponseValue.ObjectValue(List("characterDeleted" -> ResponseValue.StreamValue(ZStream.empty))),
      Nil
    )

  private val queryResponse =
    GraphQLResponse[Nothing](ResponseValue.ObjectValue(List("hello" -> Value.StringValue("world"))), Nil)

  override def spec = suite("BuildHttpResponseSpec")(
    test("prefers SSE over graphql-response+json for a subscription when both are accepted") {
      val accept = Header.accept(graphqlResponseJson, MediaType.TextEventStream)
      assertTrue(mediaTypeOf(accept, subscriptionResponse) == MediaType.TextEventStream)
    },
    test("uses graphql-response+json when SSE is not accepted") {
      assertTrue(mediaTypeOf(Header.accept(graphqlResponseJson), queryResponse) == graphqlResponseJson)
    }
  )
}
