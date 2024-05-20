package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{
  FakeAuthorizationInterceptor,
  HttpInterpreter,
  TapirAdapterSpec,
  TestApi,
  TestService,
  WebSocketInterpreter
}
import caliban.uploads.Uploads
import sttp.client3.UriContext
import zio._
import zio.http._
import zio.test.{ Live, TestEnvironment, ZIOSpecDefault }

import scala.annotation.nowarn
import scala.language.postfixOps

@nowarn
object ZHttpAdapterSpec extends ZIOSpecDefault {
  import sttp.tapir.json.zio._

  // Temporary, remove on next zio-http release
  override val bootstrap: ZLayer[Any, Any, TestEnvironment] =
    super.bootstrap ++ Runtime.setExecutor(Executor.makeDefault(true))

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private val apiLayer = envLayer >>> ZLayer.fromZIO {
    for {
      interpreter <- TestApi.api.interpreter
      _           <-
        Server
          .serve(
            Routes(
              Method.ANY / "api" / "graphql" ->
                ZHttpAdapter
                  .makeHttpService(
                    HttpInterpreter(interpreter).intercept(FakeAuthorizationInterceptor.bearer[TestService & Uploads])
                  ),
              Method.ANY / "ws" / "graphql"  ->
                ZHttpAdapter.makeWebSocketService(WebSocketInterpreter(interpreter))
            )
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
      Server.defaultWith(_.port(8089).responseCompression())
    )
  }
}
