package caliban

import akka.actor.ActorSystem
import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{ FakeAuthorizationInterceptor, TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import play.api.Mode
import play.api.routing._
import play.api.routing.sird._
import play.core.server.{ AkkaHttpServer, ServerConfig }
import sttp.client3.UriContext
import sttp.tapir.json.play._
import zio._
import zio.test.{ Live, ZIOSpecDefault }

import scala.concurrent.ExecutionContextExecutor
import scala.language.postfixOps

object PlayAdapterSpec extends ZIOSpecDefault {

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  implicit val system: ActorSystem                                 = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor          = system.dispatcher
  override implicit val runtime: Runtime[TestService with Uploads] = Runtime.unsafeFromLayer(envLayer)

  val interceptor = FakeAuthorizationInterceptor.bearer

  val apiLayer: ZLayer[Live, Throwable, Unit] = ZLayer.scoped {
    for {
      interpreter <- TestApi.api.interpreter
      router       = Router.from {
                       case req @ POST(p"/api/graphql")    =>
                         PlayAdapter.makeHttpService(interpreter, requestInterceptor = interceptor).apply(req)
                       case req @ POST(p"/upload/graphql") => PlayAdapter.makeHttpUploadService(interpreter).apply(req)
                       case req @ GET(p"/ws/graphql")      => PlayAdapter.makeWebSocketService(interpreter).apply(req)
                     }
      _           <-
        ZIO
          .attempt(
            AkkaHttpServer.fromRouterWithComponents(
              ServerConfig(
                mode = Mode.Dev,
                port = Some(8088),
                address = "127.0.0.1"
              )
            )(_ => router.routes)
          )
          .withFinalizer(server => ZIO.attempt(server.stop()).ignore *> ZIO.fromFuture(_ => system.terminate()).ignore)
      _           <- Live.live(Clock.sleep(3 seconds))
    } yield ()
  }

  override def spec = {
    val suite =
      TapirAdapterSpec.makeSuite(
        "PlayAdapterSpec",
        uri"http://localhost:8088/api/graphql",
        uploadUri = Some(uri"http://localhost:8088/upload/graphql"),
        wsUri = Some(uri"ws://localhost:8088/ws/graphql")
      )
    suite.provideCustomLayerShared(apiLayer)
  }
}
