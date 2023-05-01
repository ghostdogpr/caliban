package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{ FakeAuthorizationInterceptor, HttpInterpreter, TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import sttp.client3.UriContext
import zio._
import zio.http._
import zio.test.{ Live, ZIOSpecDefault }

import scala.language.postfixOps

object ZHttpAdapterSpec extends ZIOSpecDefault {
  import sttp.tapir.json.zio._

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private val apiLayer = envLayer >>> ZLayer.fromZIO {
    for {
      interpreter <- TestApi.api.interpreter
      _           <-
        Server
          .serve(
            Http
              .collectHttp[Request] {
                case _ -> !! / "api" / "graphql" =>
                  ZHttpAdapter.makeHttpService(
                    HttpInterpreter(interpreter).configure(FakeAuthorizationInterceptor.bearer[TestService & Uploads])
                  )
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
      Server.defaultWithPort(8089)
    )
  }
}
