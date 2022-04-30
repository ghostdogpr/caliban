package caliban

import akka.actor.ActorSystem
import akka.stream.Materializer
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

  val interceptor = FakeAuthorizationInterceptor.bearer

  val apiLayer: ZLayer[zio.ZEnv, Throwable, TestService] =
    (for {
      system      <- ZManaged.make(Task.effectTotal(ActorSystem()))(sys => ZIO.fromFuture(_ => sys.terminate()).ignore)
      ec           = system.dispatcher
      mat          = Materializer(system)
      runtime     <- ZManaged.runtime[TestService with Console with Clock with Random with Uploads]
      interpreter <- TestApi.api.interpreter.toManaged_
      router       = Router.from {
                       case req @ POST(p"/api/graphql")    =>
                         PlayAdapter
                           .makeHttpService(interpreter, requestInterceptor = interceptor)(runtime, mat)
                           .apply(req)
                       case req @ POST(p"/upload/graphql") =>
                         PlayAdapter.makeHttpUploadService(interpreter)(runtime, mat).apply(req)
                       case req @ GET(p"/ws/graphql")      =>
                         PlayAdapter.makeWebSocketService(interpreter)(ec, runtime, mat, implicitly, implicitly).apply(req)
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
                       .toManaged(server => ZIO.effect(server.stop()).ignore)
      _           <- clock.sleep(3 seconds).toManaged_
      service     <- ZManaged.service[TestService.Service]
    } yield service)
      .provideCustomLayer(TestService.make(sampleCharacters) ++ Uploads.empty ++ Clock.live)
      .toLayer

  def spec: ZSpec[ZEnv, Any] = {
    val suite: ZSpec[TestService, Throwable] =
      TapirAdapterSpec.makeSuite(
        "PlayAdapterSpec",
        uri"http://localhost:8088/api/graphql",
        uploadUri = Some(uri"http://localhost:8088/upload/graphql"),
        wsUri = Some(uri"ws://localhost:8088/ws/graphql")
      )
    suite.provideSomeLayerShared[ZEnv](apiLayer.mapError(TestFailure.fail))
  }
}
