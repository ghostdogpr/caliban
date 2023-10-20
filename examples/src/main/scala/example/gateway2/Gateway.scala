package example.gateway2

import caliban.ZHttpAdapter
import caliban.gateway.{ SubGraph, SuperGraph }
import caliban.interop.tapir.HttpInterpreter
import caliban.tools.SttpClient
import sttp.client3.httpclient.zio.HttpClientZioBackend
import sttp.tapir.json.circe._
import zio._
import zio.http._

object Gateway extends ZIOAppDefault {
  val products: SubGraph[SttpClient] = SubGraph.federated("Products", "http://localhost:8088/api/graphql")
  val reviews: SubGraph[SttpClient]  = SubGraph.federated("Reviews", "http://localhost:8089/api/graphql")

  val gateway: SuperGraph[SttpClient] =
    SuperGraph.compose(List(products, reviews))

  def run: Task[Unit] =
    gateway.build
      .tap(api => ZIO.debug(api.render))
      .flatMap(_.interpreter)
      .flatMap(interpreter =>
        Server.serve(
          Http.collectHttp[Request] { case _ -> Root / "api" / "graphql" =>
            ZHttpAdapter.makeHttpService(HttpInterpreter(interpreter))
          }
        )
      )
      .provide(ZLayer.succeed(Server.Config.default.port(8090)), Server.live, HttpClientZioBackend.layer())
}
