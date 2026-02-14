package caliban.federation.subgraph

import caliban.federation.subgraph.CacheInvalidator.InvalidationMethod
import sttp.capabilities.zio.ZioStreams
import sttp.client4.WebSocketStreamBackend
import sttp.client4.httpclient.zio.HttpClientZioBackend
import sttp.client4.testing.{ RecordingBackend, WebSocketStreamBackendStub }
import sttp.model.{ Header, Method, StatusCode }
import zio.Config.Secret
import zio._
import zio.test.Assertion._
import zio.test._

import java.net.URI

object CacheInvalidatorSpec extends ZIOSpecDefault {

  private val config = CacheInvalidator.Config(
    sharedSecret = Secret("secret"),
    invalidationUri = URI.create("https://localhost:8080/invalidate")
  )

  def spec = suite("CacheInvalidatorSpec")(
    test("invalidateAll sends a POST request with the correct body and headers") {
      val requests = List(
        InvalidationMethod.Subgraph("subgraph1"),
        InvalidationMethod.Type("subgraph2", "TypeA"),
        InvalidationMethod.CacheTag(List("subgraph1", "subgraph2"), "tag-1")
      )

      val stub: WebSocketStreamBackendStub[Task, ZioStreams]                      =
        HttpClientZioBackend.stub.whenAnyRequest
          .thenRespondAdjust("""{"count": 2}""", StatusCode.Ok)
      val backend: WebSocketStreamBackend[Task, ZioStreams] with RecordingBackend = RecordingBackend(stub)
      type RequestAndResponse = backend.RequestAndResponse

      (for {
        _            <- ZIO.serviceWithZIO[CacheInvalidator](_.invalidateAll(requests))
        interactions <- ZIO.succeed(backend.allInteractions)
      } yield assertTrue(interactions.size == 1) &&
        assert(interactions)(
          hasFirst(
            hasField[RequestAndResponse, Method]("method", _._1.method, equalTo(Method.POST)) &&
              hasField[RequestAndResponse, Seq[Header]](
                "method",
                _._1.headers,
                contains(Header("Authorization", "secret"))
              ) &&
              hasField[RequestAndResponse, String](
                "body",
                _._1.body.show,
                equalTo(
                  "string: [{\"kind\":\"subgraph\",\"name\":\"subgraph1\"},{\"kind\":\"type\",\"subgraph\":\"subgraph2\",\"type\":\"TypeA\"},{\"kind\":\"cache_tag\",\"subgraphs\":[\"subgraph1\",\"subgraph2\"],\"cache_tag\":\"tag-1\"}]"
                )
              )
          )
        )).provide(
        ZLayer.succeed(config),
        ZLayer.succeedEnvironment(
          ZEnvironment[WebSocketStreamBackend[Task, ZioStreams], RecordingBackend](backend, backend)
        ),
        CacheInvalidator.live
      )
    }
  )
}
