package caliban.interop.zio

import caliban.GraphQLRequest
import caliban.Value.{ BooleanValue, IntValue, NullValue, StringValue }
import zio.json._
import zio.test.Assertion.{ equalTo, isRight }
import zio.test._

object GraphQLRequestZIOSpec extends ZIOSpecDefault {
  override def spec = suite("GraphQLRequestZIOSpec")(
    test("can be parsed from JSON by zio-json") {
      val request =
        """{"query": "{}", "operationName": "op", "variables": {"hello":"world","answer":42,"isAwesome":true, "name": null}}"""

      val res = request.fromJson[GraphQLRequest]
      assert(res)(
        isRight(
          equalTo(
            GraphQLRequest(
              query = Some("{}"),
              operationName = Some("op"),
              variables = Some(
                Map(
                  "hello"     -> StringValue("world"),
                  "answer"    -> IntValue(42),
                  "isAwesome" -> BooleanValue(true),
                  "name"      -> NullValue
                )
              )
            )
          )
        )
      )
    },
    test("can encode to JSON by zio-json") {
      val res = GraphQLRequest(
        query = Some("{}"),
        operationName = Some("op"),
        variables = Some(
          Map(
            "hello"     -> StringValue("world"),
            "answer"    -> IntValue(42),
            "isAwesome" -> BooleanValue(true),
            "name"      -> NullValue
          )
        )
      )

      assertTrue(
        res.toJson == """{"query":"{}","operationName":"op","variables":{"hello":"world","answer":42,"isAwesome":true,"name":null}}"""
      )
    }
  )
}
