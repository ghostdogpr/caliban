package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{ TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import sttp.client3.UriContext
import zhttp.http._
import zhttp.service.Server
import zio._
import zio.duration._
import zio.test.{ DefaultRunnableSpec, TestFailure, ZSpec }

import scala.language.postfixOps

object ZHttpAdapterSpec extends DefaultRunnableSpec {

  val apiLayer: ZLayer[zio.ZEnv, Throwable, Has[Unit]] =
    (for {
      interpreter <- TestApi.api.interpreter.toManaged_
      _           <- Server
                       .start(
                         8088,
                         Http.route {
                           case _ -> Root / "api" / "graphql" => ZHttpAdapter.makeHttpService(interpreter)
                           case _ -> Root / "ws" / "graphql"  => ZHttpAdapter.makeWebSocketService(interpreter)
                         }
                       )
                       .forkManaged
      _           <- clock.Clock.Service.live.sleep(3 seconds).toManaged_
    } yield ())
      .provideCustomLayer(TestService.make(sampleCharacters) ++ Uploads.empty)
      .toLayer

  def spec: ZSpec[ZEnv, Any] = {
    val suite: ZSpec[Has[Unit], Throwable] =
      TapirAdapterSpec.makeSuite(
        "ZHttpAdapterSpec",
        uri"http://localhost:8088/api/graphql",
        wsUri = Some(uri"ws://localhost:8088/ws/graphql")
      )
    suite.provideSomeLayerShared[ZEnv](apiLayer.mapError(TestFailure.fail))
  }
}
