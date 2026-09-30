package caliban.interop.tapir

import caliban._
import caliban.interop.tapir.TapirAdapterSpec.FakeServerRequest
import sttp.model.{ Header, MediaType, Method, StatusCode, Uri }
import zio.{ Task, Trace, UIO, ZIO }
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

  private def executed[E](accept: Header, response: UIO[GraphQLResponse[E]]) = {
    val interpreter = new GraphQLInterpreter[Any, E] {
      def check(query: String)(implicit trace: Trace)                    = ZIO.unit
      def executeRequest(request: GraphQLRequest)(implicit trace: Trace) = response
    }
    val req         = FakeServerRequest(Method.POST, uri, List(accept))
    TapirAdapter.executeHttpRequest[Any, E, ZStream[Any, Throwable, Byte]](interpreter, GraphQLRequest(), req)
  }

  private def encoded[E](accept: MediaType, response: UIO[GraphQLResponse[E]]): Task[(MediaType, String)] =
    executed(Header.accept(accept), response).flatMap { case (media, _, _, body) =>
      ZIO
        .fromEither(body.left.map(_ => new RuntimeException("expected a streamed body")))
        .flatMap(_.runCollect)
        .map(bytes => media -> new String(bytes.toArray, UTF_8))
    }

  private def subscribed[E](data: ResponseValue) =
    GraphQLResponseContext.markSubscribed.as(GraphQLResponse[E](data, Nil))

  private val subscriptionResponse =
    GraphQLResponse[Nothing](
      ResponseValue.ObjectValue(List("characterDeleted" -> ResponseValue.StreamValue(ZStream.empty))),
      Nil
    )

  private val queryResponse =
    GraphQLResponse[Nothing](ResponseValue.ObjectValue(List("hello" -> Value.StringValue("world"))), Nil)

  override def spec = suite("BuildHttpResponseSpec")(
    test("complete-envelope subscriptions use SSE and preserve per-event errors") {
      val first    = GraphQLResponse(Value.NullValue, List(CalibanError.ExecutionError("event failed")))
      val next     = GraphQLResponse(ResponseValue.ObjectValue(List("event" -> Value.IntValue(2))), Nil)
      val response =
        subscribed[CalibanError](ResponseValue.StreamValue(ZStream(first.toResponseValue, next.toResponseValue)))
      encoded(MediaType.TextEventStream, response).map { case (media, text) =>
        assertTrue(
          media == MediaType.TextEventStream,
          text.contains("event failed"),
          text.contains("\"event\":2"),
          text.contains("event: complete")
        )
      }
    },
    test("incremental delivery keeps multipart framing and its initial envelope") {
      val response = GraphQLResponse(ResponseValue.StreamValue(ZStream(queryResponse.data)), Nil, hasNext = Some(true))
      encoded(MediaType.TextEventStream, ZIO.succeed(response)).map { case (media, text) =>
        assertTrue(
          media.mainType == "multipart",
          media.subType == "mixed",
          text.contains("\"hasNext\":true"),
          text.contains("\"data\":{\"hello\":\"world\"}")
        )
      }
    },
    test("JSON rejects a subscription without consuming its source") {
      val accept                                            = Header.accept(MediaType.ApplicationJson)
      def statusOf(response: UIO[GraphQLResponse[Nothing]]) = executed(accept, response).map(_._2)
      for {
        topLevel <- statusOf(subscribed(ResponseValue.StreamValue(ZStream.dieMessage("must not be consumed"))))
        field    <- statusOf(ZIO.succeed(subscriptionResponse))
      } yield assertTrue(topLevel == StatusCode.BadRequest, field == StatusCode.BadRequest)
    },
    test("prefers SSE over graphql-response+json for a subscription when both are accepted") {
      val accept = Header.accept(graphqlResponseJson, MediaType.TextEventStream)
      assertTrue(mediaTypeOf(accept, subscriptionResponse) == MediaType.TextEventStream)
    },
    test("uses graphql-response+json when SSE is not accepted") {
      assertTrue(mediaTypeOf(Header.accept(graphqlResponseJson), queryResponse) == graphqlResponseJson)
    },
    test("picks graphql-response+json for a query and SSE for a subscription with urql's default Accept header") {
      val accept = Header(
        "Accept",
        "application/graphql-response+json, application/graphql+json, application/json, text/event-stream, multipart/mixed"
      )
      assertTrue(
        mediaTypeOf(accept, queryResponse) == graphqlResponseJson,
        mediaTypeOf(accept, subscriptionResponse) == MediaType.TextEventStream
      )
    },
    test("a top-level stream is incremental unless marked as a subscription, whatever hasNext says") {
      val response = GraphQLResponse(ResponseValue.StreamValue(ZStream(queryResponse.data)), Nil)
      assertTrue(mediaTypeOf(Header.accept(MediaType.TextEventStream), response).subType == "mixed")
    }
  )
}
