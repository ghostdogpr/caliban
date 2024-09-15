package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir._
import caliban.uploads.Uploads
import com.comcast.ip4s._
import fs2.io.net.Network
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import sttp.client3.UriContext
import zio._
import zio.interop.catz._
import zio.test.{ Live, ZIOSpecDefault }

import scala.language.postfixOps

object Http4sAdapterSpec extends ZIOSpecDefault {

  type Env         = TestService with Uploads
  type TestTask[A] = RIO[Env, A]

  private implicit val network: Network[TestTask] = Network.forAsync[TestTask]

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private def apiLayer = envLayer >>> ZLayer.scoped {
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
    TapirAdapterSpec
      .makeSuite(
        "jsoniter codec",
        uri"http://localhost:8087/api/graphql",
        uploadUri = Some(uri"http://localhost:8087/upload/graphql"),
        wsUri = Some(uri"ws://localhost:8087/ws/graphql")
      )
      .provideLayerShared(apiLayer)
  }
}
