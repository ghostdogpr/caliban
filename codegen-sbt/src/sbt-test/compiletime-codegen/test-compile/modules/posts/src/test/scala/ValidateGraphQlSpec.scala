import poc.caliban.posts.GraphQLApi

import scala.io.Source
import zio.test.Assertion._
import zio.test._
import caliban.tools._
import java.nio.file.Path

object ValidateGraphQlSpec extends SnapshotTest {
  override val testName: String = "ValidateGraphQlSpec"

  val graphqlFile= "src/sbt-test/compiletime-codegen/test-compile/modules/posts/src/test/resources/postservice.graphql"
  val projectDir = sys.props.get("project.dir").getOrElse("")

  override def spec =
    suite("Validate Postservice")(
      test("Render postservice as earlier") {
        val gqlApi = GraphQLApi.api
        val renderContent: String = s"${gqlApi.render}"

        writeAndCompare(Path.of(projectDir).resolve(graphqlFile), renderContent, "Render postservice")
      }
    )
}