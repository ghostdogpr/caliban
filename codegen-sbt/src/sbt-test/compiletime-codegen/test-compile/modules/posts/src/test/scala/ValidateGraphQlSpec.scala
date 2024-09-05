import poc.caliban.posts.GraphQLApi

import scala.io.Source
import java.io.File
import java.nio.file.{Files, Paths}
import zio.test.Assertion._
import zio.test._

class ValidateGraphQlSpec extends ZIOSpecDefault {

  override def spec =
    suite("Validate Postservice")(
      test("Render postservice as earlier") {
        val filename = "postservice.graphql"
        val expectedGraphQL: String = Source.fromResource(filename).getLines().mkString("\n")
        val gqlApi = GraphQLApi.api
        val renderContent: String = s"${gqlApi.render}"

        //Files.writeString(Paths.get(File(s"/tmp/$filename").toURI), renderContent)

        assertTrue(expectedGraphQL == renderContent)
      }
    )
}