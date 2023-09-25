package caliban

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.server.Directives._
import org.apache.pekko.stream.Materializer
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
import sttp.client3.UriContext
import sttp.tapir.json.circe._
import zio._
import zio.test._

import scala.language.postfixOps

object PekkoHttpAdapterSpec extends ZIOSpecDefault {

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private val apiLayer = envLayer >>> ZLayer.scoped {
    for {
      runtime     <- ZIO.runtime[TestService with Uploads]
      system      <- ZIO.succeed(ActorSystem()).withFinalizer(sys => ZIO.fromFuture(_ => sys.terminate()).ignore)
      ec           = system.dispatcher
      mat          = Materializer(system)
      interpreter <- TestApi.api.interpreter
      adapter      = PekkoHttpAdapter.default(ec)
      route        = path("api" / "graphql") {
                       adapter
                         .makeHttpService(
                           HttpInterpreter(interpreter).intercept(FakeAuthorizationInterceptor.bearer[TestService & Uploads])
                         )(runtime, mat)
                     } ~ path("upload" / "graphql") {
                       adapter.makeHttpUploadService(HttpUploadInterpreter(interpreter))(runtime, mat, implicitly, implicitly)
                     } ~ path("ws" / "graphql") {
                       adapter.makeWebSocketService(WebSocketInterpreter(interpreter))(runtime, mat)
                     }
      _           <- ZIO.fromFuture { _ =>
                       implicit val s: ActorSystem = system
                       Http().newServerAt("localhost", 8085).bind(route)
                     }.withFinalizer(server => ZIO.fromFuture(_ => server.unbind()).ignore)
      _           <- Live.live(Clock.sleep(3 seconds))
      service     <- ZIO.service[TestService]
    } yield service
  }

  override def spec = {
    val suite = TapirAdapterSpec.makeSuite(
      "PekkoHttpAdapterSpec",
      uri"http://localhost:8085/api/graphql",
      uploadUri = Some(uri"http://localhost:8085/upload/graphql"),
      wsUri = Some(uri"ws://localhost:8085/ws/graphql")
    )
    suite.provideLayerShared(apiLayer)
  }
}
