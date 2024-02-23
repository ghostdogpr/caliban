package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{
  FakeAuthorizationInterceptor,
  HttpInterpreter,
  HttpUploadInterpreter,
  TapirAdapterSpec,
  TestApi,
  TestService,
  WebSocketInterpreter
}
import caliban.uploads.Uploads
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import play.api.Mode
import play.api.routing._
import play.api.routing.sird._
import play.core.server.{ PekkoHttpServer, ServerConfig }
import sttp.client3.UriContext
import zio._
import zio.test.{ Live, ZIOSpecDefault }

import scala.language.postfixOps

object PlayAdapterSpec extends ZIOSpecDefault {
  import sttp.tapir.json.play._

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private val apiLayer = envLayer >>> ZLayer.scoped {
    for {
      system      <- ZIO.succeed(ActorSystem()).withFinalizer(sys => ZIO.fromFuture(_ => sys.terminate()).ignore)
      mat          = Materializer(system)
      runtime     <- ZIO.runtime[TestService with Uploads]
      interpreter <- TestApi.api.interpreter
      router       = {
        implicit val rt0: Runtime[TestService with Uploads] = runtime
        implicit val mat0: Materializer                     = mat
        Router.from {
          case req @ (GET(p"/api/graphql") | POST(p"/api/graphql")) =>
            PlayAdapter
              .makeHttpService(
                HttpInterpreter(interpreter)
                  .intercept(FakeAuthorizationInterceptor.bearer[TestService & Uploads])
              )
              .apply(req)
          case req @ POST(p"/upload/graphql")                       =>
            PlayAdapter
              .makeHttpUploadService(HttpUploadInterpreter(interpreter))
              .apply(req)
          case req @ GET(p"/ws/graphql")                            =>
            PlayAdapter.makeWebSocketService(WebSocketInterpreter(interpreter)).apply(req)
        }
      }
      _           <- ZIO
                       .attempt(
                         PekkoHttpServer.fromRouterWithComponents(
                           ServerConfig(
                             mode = Mode.Dev,
                             port = Some(8088),
                             address = "127.0.0.1"
                           )
                         )(_ => router.routes)
                       )
                       .withFinalizer(server => ZIO.attempt(server.stop()).ignore)
      _           <- Live.live(Clock.sleep(3 seconds))
      service     <- ZIO.service[TestService]
    } yield service
  }

  override def spec = {
    val suite = TapirAdapterSpec.makeSuite(
      "PlayAdapterSpec",
      uri"http://localhost:8088/api/graphql",
      uploadUri = Some(uri"http://localhost:8088/upload/graphql"),
      wsUri = Some(uri"ws://localhost:8088/ws/graphql")
    )
    suite.provideLayerShared(apiLayer)
  }
}
