package caliban.interop.play

import caliban.GraphQLRequest
import zio.test.environment.TestEnvironment
import zio.test.Assertion.{ equalTo, isRight }
import zio.test._
import play.api.libs.json._

object GraphQLRequestPlaySpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLRequestPlaySpec")(
      test("can be parsed from JSON by play") {
        val request = Json
          .obj("query" -> JsString("{}"), "operationName" -> JsString("op"), "variables" -> Json.obj())
        assert(request.validate[GraphQLRequest].asEither)(
          isRight(
            equalTo(GraphQLRequest(query = Some("{}"), operationName = Some("op"), variables = Some(Map.empty)))
          )
        )
      }
    )
}
