package caliban

import akka.actor.ActorSystem
import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.TestService.TestService
import caliban.interop.tapir.{ FakeAuthorizationInterceptor, TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import play.api.Mode
import play.api.routing._
import play.api.routing.sird._
import play.core.server.{ AkkaHttpServer, ServerConfig }
import sttp.client3.UriContext
import sttp.tapir.json.play._
import zio._
import zio.clock.Clock
import zio.console.Console
import zio.duration._
import zio.internal.Platform
import zio.random.Random
import zio.test.{ DefaultRunnableSpec, TestFailure, ZSpec }

import scala.concurrent.ExecutionContextExecutor
import scala.language.postfixOps

object PlayAdapterSpec extends DefaultRunnableSpec {

  implicit val system: ActorSystem                                                            = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor                                     = system.dispatcher
  implicit val runtime: Runtime[TestService with Console with Clock with Random with Uploads] =
    Runtime.unsafeFromLayer(
      TestService.make(sampleCharacters) ++ Console.live ++ Clock.live ++ Random.live ++ Uploads.empty,
      Platform.default
    )

  val interceptor = FakeAuthorizationInterceptor.bearer

  val apiLayer: ZLayer[zio.ZEnv, Throwable, Has[Unit]] =
    (for {
      interpreter <- TestApi.api.interpreter.toManaged_
      router       = Router.from {
                       case req @ POST(p"/api/graphql")    =>
                         PlayAdapter.makeHttpService(interpreter, requestInterceptor = interceptor).apply(req)
                       case req @ POST(p"/upload/graphql") => PlayAdapter.makeHttpUploadService(interpreter).apply(req)
                       case req @ GET(p"/ws/graphql")      => PlayAdapter.makeWebSocketService(interpreter).apply(req)
                     }
      _           <- ZIO
                       .effect(
                         AkkaHttpServer.fromRouterWithComponents(
                           ServerConfig(
                             mode = Mode.Dev,
                             port = Some(8088),
                             address = "127.0.0.1"
                           )
                         )(_ => router.routes)
                       )
                       .toManaged(server => ZIO.effect(server.stop()).ignore *> ZIO.fromFuture(_ => system.terminate()).ignore)
      _           <- clock.sleep(3 seconds).toManaged_
    } yield ())
      .provideCustomLayer(TestService.make(sampleCharacters) ++ Uploads.empty ++ Clock.live)
      .toLayer

  def spec: ZSpec[ZEnv, Any] = {
    val suite: ZSpec[Has[Unit], Throwable] =
      TapirAdapterSpec.makeSuite(
        "PlayAdapterSpec",
        uri"http://localhost:8088/api/graphql",
        uploadUri = Some(uri"http://localhost:8088/upload/graphql"),
        wsUri = Some(uri"ws://localhost:8088/ws/graphql")
      )
    suite.provideSomeLayerShared[ZEnv](apiLayer.mapError(TestFailure.fail))
  }
}
