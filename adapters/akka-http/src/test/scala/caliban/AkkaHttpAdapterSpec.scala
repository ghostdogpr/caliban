package caliban

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{ FakeAuthorizationInterceptor, TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import sttp.client3.UriContext
import sttp.tapir.json.circe._
import zio._
import zio.test._

import scala.concurrent.ExecutionContextExecutor
import scala.language.postfixOps

object AkkaHttpAdapterSpec extends ZIOSpecDefault {

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  implicit val system: ActorSystem                                 = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor          = system.dispatcher
  override implicit val runtime: Runtime[TestService with Uploads] = Runtime.unsafeFromLayer(envLayer)

  val apiLayer: ZLayer[Live, Throwable, Unit] = ZLayer.scoped {
    for {
      interpreter <- TestApi.api.interpreter
      adapter      = AkkaHttpAdapter.default
      route        = path("api" / "graphql") {
                       adapter.makeHttpService(interpreter, requestInterceptor = FakeAuthorizationInterceptor.bearer)
                     } ~ path("upload" / "graphql") {
                       adapter.makeHttpUploadService(interpreter)
                     } ~ path("ws" / "graphql") {
                       adapter.makeWebSocketService(interpreter)
                     }
      _           <- ZIO
                       .fromFuture(_ => Http().newServerAt("localhost", 8086).bind(route))
                       .withFinalizer(server =>
                         ZIO.fromFuture(_ => server.unbind()).ignore *> ZIO.fromFuture(_ => system.terminate()).ignore
                       )
      _           <- Live.live(Clock.sleep(3 seconds))
    } yield ()
  }

  override val spec = {
    val suite =
      TapirAdapterSpec.makeSuite(
        "AkkaHttpAdapterSpec",
        uri"http://localhost:8086/api/graphql",
        uploadUri = Some(uri"http://localhost:8086/upload/graphql"),
        wsUri = Some(uri"ws://localhost:8086/ws/graphql")
      )
    suite.provideCustomLayerShared(apiLayer)
  }
}
