package caliban

import caliban.interop.tapir.TestData.sampleCharacters
import caliban.interop.tapir.{ TapirAdapterSpec, TestApi, TestService }
import caliban.uploads.Uploads
import sttp.client3.UriContext
import zio._
import zio.http._
import zio.test.{ Live, ZIOSpecDefault }

import scala.language.postfixOps

object ZHttpAdapterSlimSpec extends ZIOSpecDefault {
  import sttp.tapir.json.jsoniter._

  private val envLayer = TestService.make(sampleCharacters) ++ Uploads.empty

  private val auth = HttpAppMiddleware.intercept { case (req, resp) =>
    if (req.headers.get("X-Invalid").nonEmpty)
      Response(Status.Unauthorized, body = Body.fromString("You are unauthorized!"))
    else resp
  }

  private val apiLayer = envLayer >>> ZLayer.fromZIO {
    for {
      interpreter <- TestApi.api.interpreter.map(new ZHttpAdapterSlim(_))
      _           <-
        Server
          .serve(Http.collectHandler { case _ -> Root / "api" / "graphql" => interpreter.handler @@ auth })
          .forkScoped
      _           <- Live.live(Clock.sleep(3 seconds))
      service     <- ZIO.service[TestService]
    } yield service
  }

  override def spec = suite("ZIO Http Slim") {
    val suite = TapirAdapterSpec.makeSuite(
      "ZHttpAdapterSlimSpec",
      uri"http://localhost:8090/api/graphql",
      wsUri = None
    )
    suite.provideShared(
      apiLayer,
      Scope.default,
      Server.defaultWith(_.port(8090).responseCompression())
    )
  }
}
