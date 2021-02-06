package caliban.interop.circe

import caliban.GraphQLRequest
import io.circe._
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test._

object GraphQLRequestCirceSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLRequestCirceSpec")(
      test("can be parsed from JSON by circe") {
        val request = Json
          .obj("query" -> Json.fromString("{}"), "operationName" -> Json.fromString("op"), "variables" -> Json.obj())
        assert(request.as[GraphQLRequest])(
          isRight(
            equalTo(GraphQLRequest(query = Some("{}"), operationName = Some("op"), variables = Some(Map.empty)))
          )
        )
      }
    )
}
