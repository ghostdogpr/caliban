package caliban

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.stream.Materializer
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
import sttp.tapir.json.play._
import zio._
import zio.test._

import scala.language.postfixOps

object AkkaHttpAdapterSpec extends ZIOSpecDefault {

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private val apiLayer = envLayer >>> ZLayer.scoped {
    for {
      runtime     <- ZIO.runtime[TestService with Uploads]
      system      <- ZIO.succeed(ActorSystem()).withFinalizer(sys => ZIO.fromFuture(_ => sys.terminate()).ignore)
      ec           = system.dispatcher
      mat          = Materializer(system)
      interpreter <- TestApi.api.interpreter
      adapter      = AkkaHttpAdapter.default(ec)
      route        = path("api" / "graphql") {
                       adapter.makeHttpService(
                         HttpInterpreter(interpreter).configure(FakeAuthorizationInterceptor.bearer[TestService & Uploads])
                       )(runtime, mat)
                     } ~ path("upload" / "graphql") {
                       adapter.makeHttpUploadService(HttpUploadInterpreter(interpreter))(runtime, mat, implicitly, implicitly)
                     } ~ path("ws" / "graphql") {
                       adapter.makeWebSocketService(WebSocketInterpreter(interpreter))(runtime, mat)
                     }
      _           <- ZIO.fromFuture { _ =>
                       implicit val s: ActorSystem = system
                       Http().newServerAt("localhost", 8086).bind(route)
                     }.withFinalizer(server => ZIO.fromFuture(_ => server.unbind()).ignore)
      _           <- Live.live(Clock.sleep(3 seconds))
      service     <- ZIO.service[TestService]
    } yield service
  }

  override def spec = {
    val suite = TapirAdapterSpec.makeSuite(
      "AkkaHttpAdapterSpec",
      uri"http://localhost:8086/api/graphql",
      uploadUri = Some(uri"http://localhost:8086/upload/graphql"),
      wsUri = Some(uri"ws://localhost:8086/ws/graphql")
    )
    suite.provideLayerShared(apiLayer)
  }
}
