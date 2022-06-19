package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{ FakeAuthorizationInterceptor, TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import org.http4s.blaze.server.BlazeServerBuilder
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

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private val apiLayer = envLayer >>> ZLayer.scoped {
    for {
      interpreter <- TestApi.api.interpreter
      _           <- BlazeServerBuilder[TestTask]
                       .bindHttp(8087, "localhost")
                       .withHttpWebSocketApp(wsBuilder =>
                         Router[TestTask](
                           "/api/graphql"    -> CORS.policy(
                             Http4sAdapter.makeHttpService[Env, CalibanError](
                               interpreter,
                               requestInterceptor = FakeAuthorizationInterceptor.bearer
                             )
                           ),
                           "/upload/graphql" -> CORS.policy(Http4sAdapter.makeHttpUploadService[Env, CalibanError](interpreter)),
                           "/ws/graphql"     -> CORS.policy(
                             Http4sAdapter.makeWebSocketService[Env, Env, CalibanError](wsBuilder, interpreter)
                           )
                         ).orNotFound
                       )
                       .resource
                       .toScopedZIO
                       .forkScoped
      _           <- Live.live(Clock.sleep(3 seconds))
      service     <- ZIO.service[TestService]
    } yield service
  }

  override def spec = {
    val suite = TapirAdapterSpec.makeSuite(
      "Http4sAdapterSpec",
      uri"http://localhost:8087/api/graphql",
      uploadUri = Some(uri"http://localhost:8087/upload/graphql"),
      wsUri = Some(uri"ws://localhost:8087/ws/graphql")
    )
    suite.provideCustomLayerShared(apiLayer)
  }
}
