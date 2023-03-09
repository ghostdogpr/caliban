package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{ FakeAuthorizationInterceptor, TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import sttp.client3.UriContext
import zio.http._
import zio.http.netty.server.NettyDriver
import zio.http.netty.{ ChannelFactories, NettyRuntime }
import zio._
import zio.test.{ Live, ZIOSpecDefault }

import scala.language.postfixOps

object ZHttpAdapterSpec extends ZIOSpecDefault {
  import sttp.tapir.json.zio._

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private val apiLayer = envLayer >>> ZLayer.fromZIO {
    for {
      interpreter <- TestApi.api.interpreter
      _           <- Server
                       .serve(
                         Http
                           .collectRoute[Request] {
                             case _ -> !! / "api" / "graphql" =>
                               ZHttpAdapter.makeHttpService(interpreter, requestInterceptor = FakeAuthorizationInterceptor.bearer)
                             case _ -> !! / "ws" / "graphql"  =>
                               ZHttpAdapter.makeWebSocketService(interpreter)
                           }
                           .withDefaultErrorResponse
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
    suite.provideShared(
      apiLayer,
      Scope.default,
      Server.live,
      ServerConfig.live(ServerConfig.default.port(8089))
    )
  }
}
