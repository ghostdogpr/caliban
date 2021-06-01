package caliban.interop.circe

import caliban.GraphQLRequest
import io.circe._
import io.circe.syntax._
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test._
import caliban.Value

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
      },
      test("can encode to JSON by circe") {
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

        assert(res.asJson.noSpaces)(
          equalTo(
            """{"query":"{}","operationName":"op","variables":{"hello":"world","answer":42,"isAwesome":true,"name":null},"extensions":null}"""
          )
        )
      }
    )
}
