package example.gateway2

import caliban.gateway.{ SubGraph, SuperGraph }
import caliban.quick.GraphqlServerOps
import caliban.tools.SttpClient
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio._

object Gateway extends ZIOAppDefault {
  val products: SubGraph[SttpClient] = SubGraph.federated("Products", "http://localhost:8088/api/graphql")
  val reviews: SubGraph[SttpClient]  = SubGraph.federated("Reviews", "http://localhost:8089/api/graphql")

  val gateway: SuperGraph[SttpClient] =
    SuperGraph.compose(List(products, reviews))

  def run: Task[Unit] =
    gateway.build
      .tap(api => ZIO.debug(api.render))
      .flatMap(_.runServer(8090, apiPath = "api/graphql"))
      .provide(HttpClientZioBackend.layer())
}
