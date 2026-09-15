package caliban.interop.tapir

import caliban.{ CalibanError, GraphQLResponse, ResponseValue, Value }
import caliban.interop.tapir.TapirAdapterSpec.FakeServerRequest
import sttp.model.{ Header, MediaType, Method, StatusCode, Uri }
import zio.ZIO
import zio.stream.ZStream
import zio.test._

import java.nio.charset.StandardCharsets.UTF_8

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
    test("complete-envelope subscriptions use SSE and preserve per-event errors") {
      val first               = GraphQLResponse(Value.NullValue, List(CalibanError.ExecutionError("event failed")))
      val next                = GraphQLResponse(ResponseValue.ObjectValue(List("event" -> Value.IntValue(2))), Nil)
      val response            =
        GraphQLResponse(ResponseValue.StreamValue(ZStream(first.toResponseValue, next.toResponseValue)), Nil)
      val req                 = FakeServerRequest(Method.POST, uri, List(Header.accept(MediaType.TextEventStream)))
      val (media, _, _, body) = TapirAdapter.buildHttpResponse[Nothing, ZStream[Any, Throwable, Byte]](req)(response)
      ZIO.fromEither(body.left.map(_ => new RuntimeException("expected SSE body"))).flatMap(_.runCollect).map { bytes =>
        val encoded = new String(bytes.toArray, UTF_8)
        assertTrue(
          media == MediaType.TextEventStream,
          encoded.contains("event failed"),
          encoded.contains("\"event\":2"),
          encoded.contains("event: complete")
        )
      }
    },
    test("incremental delivery keeps multipart framing and its initial envelope") {
      val response            = GraphQLResponse(ResponseValue.StreamValue(ZStream(queryResponse.data)), Nil, hasNext = Some(true))
      val req                 = FakeServerRequest(Method.POST, uri, List(Header.accept(MediaType.TextEventStream)))
      val (media, _, _, body) = TapirAdapter.buildHttpResponse[Nothing, ZStream[Any, Throwable, Byte]](req)(response)
      ZIO.fromEither(body.left.map(_ => new RuntimeException("expected multipart body"))).flatMap(_.runCollect).map {
        bytes =>
          val encoded = new String(bytes.toArray, UTF_8)
          assertTrue(
            media.mainType == "multipart",
            media.subType == "mixed",
            encoded.contains("\"hasNext\":true"),
            encoded.contains("\"data\":{\"hello\":\"world\"}")
          )
      }
    },
    test("JSON rejects a subscription without consuming its source") {
      val response          = GraphQLResponse(ResponseValue.StreamValue(ZStream.dieMessage("must not be consumed")), Nil)
      val req               = FakeServerRequest(Method.POST, uri, List(Header.accept(MediaType.ApplicationJson)))
      val (_, status, _, _) = TapirAdapter.buildHttpResponse[Nothing, ZStream[Any, Throwable, Byte]](req)(response)
      assertTrue(status == StatusCode.BadRequest)
    },
    test("prefers SSE over graphql-response+json for a subscription when both are accepted") {
      val accept = Header.accept(graphqlResponseJson, MediaType.TextEventStream)
      assertTrue(mediaTypeOf(accept, subscriptionResponse) == MediaType.TextEventStream)
    },
    test("uses graphql-response+json when SSE is not accepted") {
      assertTrue(mediaTypeOf(Header.accept(graphqlResponseJson), queryResponse) == graphqlResponseJson)
    }
  )
}
