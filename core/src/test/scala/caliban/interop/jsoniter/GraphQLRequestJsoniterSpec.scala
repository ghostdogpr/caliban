package caliban.interop.jsoniter

import caliban.{ GraphQLRequest, TestUtils, Value }
import com.github.plokhotnyuk.jsoniter_scala.core.{ readFromString, writeToString }
import zio.test.Assertion.equalTo
import zio.test.{ assert, assertTrue, ZIOSpecDefault }

object GraphQLRequestJsoniterSpec extends ZIOSpecDefault {

  override def spec =
    suite("GraphQLRequestJsoniterSpec")(
      test("can be parsed from JSON by jsoniter") {
        val request =
          """{"query": "{}", "operationName": "op", "variables": {"hello":"world","answer":42,"isAwesome":true, "name": null}}"""
        assert(readFromString[GraphQLRequest](request))(
          equalTo(
            GraphQLRequest(
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
          )
        )
      },
      test("can encode to JSON by jsoniter") {
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

        assertTrue(
          writeToString(res) ==
            """{"query":"{}","operationName":"op","variables":{"hello":"world","answer":42,"isAwesome":true,"name":null}}"""
        )
      }
    ) @@ TestUtils.skipJdk8
}
