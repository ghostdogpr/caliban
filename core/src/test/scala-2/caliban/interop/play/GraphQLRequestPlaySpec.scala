package caliban.interop.play

import caliban.GraphQLRequest
import zio.test.environment.TestEnvironment
import zio.test.Assertion.{ equalTo, isRight }
import zio.test._
import play.api.libs.json._
import caliban.Value

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
      },
      test("can be serialized to json [play]") {
        val res = GraphQLRequest(
          query = Some("{}"),
          operationName = Some("op"),
          variables = Some(
            Map(
              "hello"     -> Value.StringValue("world"),
              "answer"    -> Value.IntValue(42),
              "isAwesome" -> Value.BooleanValue(true),
              "name"      -> Value.NullValue
            )
          )
        )

        assert(Json.toJson(res).toString())(
          equalTo(
            """{"query":"{}","operationName":"op","variables":{"hello":"world","answer":42,"isAwesome":true,"name":null}}"""
          )
        )
      }
    )
}
