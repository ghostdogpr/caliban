package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir._
import caliban.uploads.Uploads
import com.comcast.ip4s._
import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._
import fs2.io.net.Network
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import sttp.client3.UriContext
import sttp.tapir.Codec.JsonCodec
import zio._
import zio.interop.catz._
import zio.test.{ Live, ZIOSpecDefault }

import scala.language.postfixOps

object Http4sAdapterSpec extends ZIOSpecDefault {

  type Env         = TestService with Uploads
  type TestTask[A] = RIO[Env, A]

  private implicit val network: Network[TestTask] = Network.forAsync[TestTask]

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private def apiLayer(implicit
    requestCodec: JsonCodec[GraphQLRequest],
    mapCodec: JsonCodec[Map[String, Seq[String]]],
    responseValueCodec: JsonCodec[ResponseValue],
    wsInputCodec: JsonCodec[GraphQLWSInput],
    wsOutputCodec: JsonCodec[GraphQLWSOutput]
  ) = envLayer >>> ZLayer.scoped {
    for {
      interpreter <- TestApi.api.interpreter
      _           <- EmberServerBuilder
                       .default[TestTask]
                       .withHost(host"localhost")
                       .withPort(port"8087")
                       .withHttpWebSocketApp(wsBuilder =>
                         Router[TestTask](
                           "/api/graphql"    -> CORS.policy(
                             Http4sAdapter.makeHttpService[Env, CalibanError](
                               HttpInterpreter(interpreter).intercept(FakeAuthorizationInterceptor.bearer[Env])
                             )
                           ),
                           "/upload/graphql" -> CORS.policy(
                             Http4sAdapter.makeHttpUploadService[Env, CalibanError](HttpUploadInterpreter(interpreter))
                           ),
                           "/ws/graphql"     -> CORS.policy(
                             Http4sAdapter
                               .makeWebSocketService[Env, Env, CalibanError](wsBuilder, WebSocketInterpreter(interpreter))
                           )
                         ).orNotFound
                       )
                       .build
                       .toScopedZIO
                       .forkScoped
      _           <- Live.live(Clock.sleep(3 seconds))
      service     <- ZIO.service[TestService]
    } yield service
  }

  override def spec = suite("Http4sAdapterSpec") {
    val suites =
      List(
        Some({
          import sttp.tapir.json.circe._
          TapirAdapterSpec
            .makeSuite(
              "circe codec",
              uri"http://localhost:8087/api/graphql",
              uploadUri = Some(uri"http://localhost:8087/upload/graphql"),
              wsUri = Some(uri"ws://localhost:8087/ws/graphql")
            )
            .provideLayerShared(apiLayer)
        }),
        Some({
          import sttp.tapir.json.jsoniter._
          implicit val mapCodec: JsonValueCodec[Map[String, Seq[String]]] = JsonCodecMaker.make

          TapirAdapterSpec
            .makeSuite(
              "jsoniter codec",
              uri"http://localhost:8087/api/graphql",
              uploadUri = Some(uri"http://localhost:8087/upload/graphql"),
              wsUri = Some(uri"ws://localhost:8087/ws/graphql")
            )
            .provideLayerShared(apiLayer)
        })
      )
    ZIO.succeed(suites.flatten)
  }
}
