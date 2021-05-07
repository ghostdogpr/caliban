package caliban.interop.zio

import caliban.GraphQLRequest
import zio.test.environment.TestEnvironment
import zio.test._
import Assertion._
import caliban.Value.{ BooleanValue, IntValue, NullValue, StringValue }
import zio.json._

object GraphQLRequestZIOSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] = suite("GraphQLRequestZIOSpec")(
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
    }
  )
}
