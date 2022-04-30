package caliban

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.stream.Materializer
import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.TestService.TestService
import caliban.interop.tapir.{ FakeAuthorizationInterceptor, TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import sttp.client3.UriContext
import sttp.tapir.json.circe._
import zio._
import zio.clock.Clock
import zio.console.Console
import zio.duration._
import zio.random.Random
import zio.test.{ DefaultRunnableSpec, TestFailure, ZSpec }

import scala.language.postfixOps

object AkkaHttpAdapterSpec extends DefaultRunnableSpec {

  val apiLayer: ZLayer[zio.ZEnv, Throwable, TestService] =
    (for {
      runtime     <- ZManaged.runtime[TestService with Console with Clock with Random with Uploads]
      system      <- ZManaged.make(Task.effectTotal(ActorSystem()))(sys => ZIO.fromFuture(_ => sys.terminate()).ignore)
      ec           = system.dispatcher
      mat          = Materializer(system)
      service     <- ZManaged.service[TestService.Service]
      interpreter <- TestApi.api.interpreter.toManaged_
      route        = path("api" / "graphql") {
                       AkkaHttpAdapter.makeHttpService(interpreter, requestInterceptor = FakeAuthorizationInterceptor.bearer)(
                         runtime,
                         implicitly,
                         implicitly
                       )
                     } ~ path("upload" / "graphql") {
                       AkkaHttpAdapter.makeHttpUploadService(interpreter)(runtime, implicitly, implicitly, implicitly)
                     } ~ path("ws" / "graphql") {
                       AkkaHttpAdapter.makeWebSocketService(interpreter)(ec, runtime, mat, implicitly, implicitly)
                     }
      _           <- ZIO.fromFuture { _ =>
                       implicit val s: ActorSystem = system
                       Http()(system).newServerAt("localhost", 8088).bind(route)
                     }
                       .toManaged(server => ZIO.fromFuture(_ => server.unbind()).ignore)
      _           <- clock.sleep(3 seconds).toManaged_
    } yield service)
      .provideCustomLayer(TestService.make(sampleCharacters) ++ Uploads.empty ++ Clock.live)
      .toLayer

  def spec: ZSpec[ZEnv, Any] = {
    val suite: ZSpec[TestService, Throwable] =
      TapirAdapterSpec.makeSuite(
        "AkkaHttpAdapterSpec",
        uri"http://localhost:8088/api/graphql",
        uploadUri = Some(uri"http://localhost:8088/upload/graphql"),
        wsUri = Some(uri"ws://localhost:8088/ws/graphql")
      )
    suite.provideSomeLayerShared[ZEnv](apiLayer.mapError(TestFailure.fail))
  }
}
