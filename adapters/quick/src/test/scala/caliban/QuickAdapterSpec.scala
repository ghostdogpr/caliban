package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{ TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import sttp.client3.UriContext
import zio._
import zio.http._
import zio.test.{ testEnvironment, Live, TestEnvironment, ZIOSpecDefault }

import scala.language.postfixOps

object QuickAdapterSpec extends ZIOSpecDefault {
  import caliban.quick._
  import sttp.tapir.json.jsoniter._

  // Temporary, remove on next zio-http release
  override val bootstrap: ZLayer[Any, Any, TestEnvironment] =
    testEnvironment ++ Runtime.setExecutor(Executor.makeDefault(true))

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private val auth = Middleware.intercept { case (req, resp) =>
    if (req.headers.get("X-Invalid").nonEmpty)
      Response(Status.Unauthorized, body = Body.fromString("You are unauthorized!"))
    else resp
  }

  private val apiLayer = envLayer >>> ZLayer.fromZIO {
    for {
      app     <- TestApi.api
                   .routes("/api/graphql", uploadPath = Some("/upload/graphql"), webSocketPath = Some("/ws/graphql"))
                   .map(_ @@ auth)
      _       <- Server.serve(app).forkScoped
      _       <- Live.live(Clock.sleep(3 seconds))
      service <- ZIO.service[TestService]
    } yield service
  }

  override def spec = suite("ZIO Http Quick") {
    val suite = TapirAdapterSpec.makeSuite(
      "QuickAdapterSpec",
      uri"http://localhost:8090/api/graphql",
      wsUri = Some(uri"ws://localhost:8090/ws/graphql"),
      uploadUri = Some(uri"http://localhost:8090/upload/graphql")
    )
    suite.provideShared(
      apiLayer,
      Scope.default,
      Server.defaultWith(_.port(8090).responseCompression())
    )
  }
}
