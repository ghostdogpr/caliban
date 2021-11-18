package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.TestService.TestService
import caliban.interop.tapir.{ TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import sttp.client3.UriContext
import zio._
import zio.clock.Clock
import zio.duration._
import zio.interop.catz._
import zio.test.{ DefaultRunnableSpec, TestFailure, ZSpec }

import scala.language.postfixOps

object Http4sAdapterSpec extends DefaultRunnableSpec {

  type Env         = ZEnv with TestService with Uploads
  type TestTask[A] = RIO[Env, A]

  val apiLayer: ZLayer[zio.ZEnv, Throwable, Has[Unit]] =
    (for {
      interpreter <- TestApi.api.interpreter.toManaged_
      _           <- BlazeServerBuilder[TestTask]
                       .bindHttp(8088, "localhost")
                       .withHttpWebSocketApp(wsBuilder =>
                         Router[TestTask](
                           "/api/graphql"    -> CORS.policy(Http4sAdapter.makeHttpService[Env, CalibanError](interpreter)),
                           "/upload/graphql" -> CORS.policy(Http4sAdapter.makeHttpUploadService[Env, CalibanError](interpreter)),
                           "/ws/graphql"     -> CORS.policy(
                             Http4sAdapter.makeWebSocketService[Env, Env, CalibanError](wsBuilder, interpreter)
                           )
                         ).orNotFound
                       )
                       .resource
                       .toManagedZIO
                       .fork
      _           <- clock.sleep(3 seconds).toManaged_
    } yield ())
      .provideCustomLayer(TestService.make(sampleCharacters) ++ Uploads.empty ++ Clock.live)
      .toLayer

  def spec: ZSpec[ZEnv, Any] = {
    val suite: ZSpec[Has[Unit], Throwable] =
      TapirAdapterSpec.makeSuite(
        "Http4sAdapterSpec",
        uri"http://localhost:8088/api/graphql",
        uploadUri = Some(uri"http://localhost:8088/upload/graphql"),
        wsUri = Some(uri"ws://localhost:8088/ws/graphql")
      )
    suite.provideSomeLayerShared[ZEnv](apiLayer.mapError(TestFailure.fail))
  }
}
