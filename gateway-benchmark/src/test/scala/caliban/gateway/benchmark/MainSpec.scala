package caliban.gateway.benchmark

import caliban.gateway.Subgraph
import zio.test._

object MainSpec extends ZIOSpecDefault {

  def spec = suite("Gateway benchmark adapter")(
    test("uses the four pinned benchmark subgraph ports") {
      assertTrue(
        Main.subgraphs.map(_.name) == List("accounts", "inventory", "products", "reviews"),
        Main.subgraphs.map(_.source).collect { case remote: Subgraph.Source.Remote[_] => remote.endpoint.encode } ==
          (5221 to 5224).toList.map(port => s"http://127.0.0.1:$port/graphql")
      )
    }
  )
}
