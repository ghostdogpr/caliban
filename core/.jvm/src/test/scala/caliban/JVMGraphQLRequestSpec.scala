package caliban

import spray.json.{ JsObject, JsString }
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object JVMGraphQLRequestSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("JVMGraphQLRequestSpec")(
      test("can be parsed from JSON (spray)") {
        val request = JsObject("query" -> JsString("{}"), "operationName" -> JsString("op"), "variables" -> JsObject())
        assert(request.convertTo[GraphQLRequest])(
          equalTo(GraphQLRequest(query = "{}", operationName = Some("op"), variables = Some(Map.empty)))
        )
      }
    )
}
