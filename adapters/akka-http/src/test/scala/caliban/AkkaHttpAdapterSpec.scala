package caliban

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{ TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import sttp.client3.UriContext
import sttp.tapir.json.circe._
import zio._
import zio.test._

import scala.concurrent.ExecutionContextExecutor
import scala.language.postfixOps

object AkkaHttpAdapterSpec extends DefaultRunnableSpec {

  implicit val system: ActorSystem                                                            = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor                                     = system.dispatcher
  implicit val runtime: Runtime[TestService with Console with Clock with Random with Uploads] =
    Runtime.unsafeFromLayer(
      TestService.make(sampleCharacters) ++ Console.live ++ Clock.live ++ Random.live ++ Uploads.empty,
      RuntimeConfig.default
    )

  val apiLayer: ZLayer[zio.ZEnv, Throwable, Unit] =
    (for {
      interpreter <- TestApi.api.interpreter.toManaged
      route        = path("api" / "graphql") {
                       AkkaHttpAdapter.makeHttpService(interpreter)
                     } ~ path("upload" / "graphql") {
                       AkkaHttpAdapter.makeHttpUploadService(interpreter)
                     } ~ path("ws" / "graphql") {
                       AkkaHttpAdapter.makeWebSocketService(interpreter)
                     }
      _           <- ZIO
                       .fromFuture(_ => Http().newServerAt("localhost", 8088).bind(route))
                       .toManagedWith(server =>
                         ZIO.fromFuture(_ => server.unbind()).ignore *> ZIO.fromFuture(_ => system.terminate()).ignore
                       )
      _           <- Clock.sleep(3 seconds).toManaged
    } yield ())
      .provideCustomLayer(TestService.make(sampleCharacters) ++ Uploads.empty ++ Clock.live)
      .toLayer

  def spec: ZSpec[ZEnv, Any] = {
    val suite: ZSpec[Unit, Throwable] =
      TapirAdapterSpec.makeSuite(
        "AkkaHttpAdapterSpec",
        uri"http://localhost:8088/api/graphql",
        uploadUri = Some(uri"http://localhost:8088/upload/graphql"),
        wsUri = Some(uri"ws://localhost:8088/ws/graphql")
      )
    suite.provideSomeLayerShared[ZEnv](apiLayer.mapError(TestFailure.fail))
  }
}
