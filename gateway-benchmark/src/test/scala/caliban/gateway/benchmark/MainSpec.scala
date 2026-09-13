package caliban.gateway.benchmark

import zio.test._

object MainSpec extends ZIOSpecDefault {

  def spec = suite("Gateway benchmark adapter")(
    test("uses the four pinned benchmark subgraph ports") {
      val sources = Main.benchmarkSubgraphs("127.0.0.1")

      assertTrue(
        sources.map(_.map(_.name)) == Right(List("accounts", "inventory", "products", "reviews")),
        Main.benchmarkSubgraphs("").isLeft,
        Main.benchmarkSubgraphs("not a host").isLeft
      )
    }
  )
}
