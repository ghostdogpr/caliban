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
          """{"query": "{}", "operationName": "op", "variables": {"hello":"world","intValue":42,"longValue":99999999999,"bigIntValue":9223372036854775810,"decimalValue":1.1,"bigDecimalValue":4.4028235E38,"isAwesome":true, "name": null}}"""
        assert(readFromString[GraphQLRequest](request))(
          equalTo(
            GraphQLRequest(
              query = Some("{}"),
              operationName = Some("op"),
              variables = Some(
                Map(
                  "hello"           -> Value.StringValue("world"),
                  "intValue"        -> Value.IntValue.IntNumber(42),
                  "longValue"       -> Value.IntValue.LongNumber(99999999999L),
                  "bigIntValue"     -> Value.IntValue.BigIntNumber(BigInt("9223372036854775810")),
                  "decimalValue"    -> Value.FloatValue(BigDecimal("1.1")),
                  "bigDecimalValue" -> Value.FloatValue(BigDecimal("4.4028235E38")),
                  "isAwesome"       -> Value.BooleanValue(true),
                  "name"            -> Value.NullValue
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
      },
      test("isHttpGetRequest is ignored when serializing to JSON") {
        val res = writeToString(GraphQLRequest(isHttpGetRequest = true))
        assertTrue(res == """{}""")
      },
      test("isHttpGetRequest is ignored when deserializing from JSON") {
        val res = readFromString[GraphQLRequest]("""{"isHttpGetRequest":true}""").isHttpGetRequest
        assertTrue(!res)
      }
    )
}
