package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{ FakeAuthorizationInterceptor, TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import sttp.client3.UriContext
import zhttp.http._
import zhttp.service.Server
import zio._
import zio.test.{ Live, ZIOSpecDefault }

import scala.language.postfixOps

object ZHttpAdapterSpec extends ZIOSpecDefault {

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private val apiLayer = envLayer >>> ZLayer.scoped {
    for {
      interpreter <- TestApi.api.interpreter
      _           <- Server
                       .start(
                         8089,
                         Http.collectHttp[Request] {
                           case _ -> !! / "api" / "graphql" =>
                             ZHttpAdapter.makeHttpService(interpreter, requestInterceptor = FakeAuthorizationInterceptor.bearer)
                           case _ -> !! / "ws" / "graphql"  =>
                             ZHttpAdapter.makeWebSocketService(interpreter)
                         }
                       )
                       .forkScoped
      _           <- Live.live(Clock.sleep(3 seconds))
      service     <- ZIO.service[TestService]
    } yield service
  }

  override def spec = suite("ZIO Http") {
    val suite = TapirAdapterSpec.makeSuite(
      "ZHttpAdapterSpec",
      uri"http://localhost:8089/api/graphql",
      wsUri = Some(uri"ws://localhost:8089/ws/graphql")
    )
    suite.provideCustomLayerShared(apiLayer)
  }
}
