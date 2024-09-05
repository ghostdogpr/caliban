package poc.caliban.logrocket

import scala.io.Source
import zio.test.Assertion._
import zio.test._

object ValidateGraphQlSpec extends ZIOSpecDefault {

  override def spec =
    suite("Validate Logrocket graphql")(
      test("Render logrocket as expected") {
        val filename = "logrocket.graphql"
        val expectedGraphQL: String = Source.fromResource(filename).getLines().mkString("\n")
        val gqlApi = GraphQLApi.api
        val renderContent: String = s"${gqlApi.render}"

        assertTrue(expectedGraphQL == renderContent)
      }
    )
}