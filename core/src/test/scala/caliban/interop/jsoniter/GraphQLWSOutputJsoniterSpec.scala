package caliban.interop.jsoniter

import caliban.{ GraphQLWSOutput, ResponseValue, Value }
import com.github.plokhotnyuk.jsoniter_scala.core._
import zio.test.Assertion.equalTo
import zio.test.{ assert, assertTrue, ZIOSpecDefault }

object GraphQLWSOutputJsoniterSpec extends ZIOSpecDefault {
  override def spec =
    suite("GraphQLWSOutputJsoniter")(
      test("can be parsed from JSON by jsoniter") {
        val request =
          """{"id":"id","type":"some type","payload":{"field":"yo"}}"""

        val res = readFromString[GraphQLWSOutput](request)
        assert(res)(
          equalTo(
            GraphQLWSOutput(
              `type` = "some type",
              id = Some("id"),
              payload = Some(ResponseValue.ObjectValue(List("field" -> Value.StringValue("yo"))))
            )
          )
        )
      },
      test("can encode to JSON by jsoniter") {
        val res = GraphQLWSOutput(
          `type` = "some type",
          id = Some("id"),
          payload = Some(ResponseValue.ObjectValue(List("field" -> Value.StringValue("yo"))))
        )

        assertTrue(writeToString(res) == """{"type":"some type","id":"id","payload":{"field":"yo"}}""")
      }
    )
}
