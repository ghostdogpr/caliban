package caliban.gateway.benchmark

import zio.test._

object MainSpec extends ZIOSpecDefault {

  def spec = suite("Gateway benchmark adapter")(
    test("uses the four pinned benchmark subgraph endpoints") {
      val sources = Main.benchmarkSubgraphs("http://127.0.0.1:4200/")

      assertTrue(
        sources.map(_.map(_.name)) == Right(List("accounts", "inventory", "products", "reviews")),
        sources.isRight,
        Main.benchmarkSubgraphs("not a uri").isLeft
      )
    }
  )
}
