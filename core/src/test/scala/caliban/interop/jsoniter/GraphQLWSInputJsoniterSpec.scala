package caliban.interop.jsoniter

import caliban.{ GraphQLWSInput, InputValue, TestUtils, Value }
import com.github.plokhotnyuk.jsoniter_scala.core._
import zio.test.Assertion.equalTo
import zio.test.{ assert, assertTrue, ZIOSpecDefault }

object GraphQLWSInputJsoniterSpec extends ZIOSpecDefault {
  override def spec =
    suite("GraphQLWSInputJsoniterSpec")(
      test("can be parsed from JSON by jsoniter") {
        val request =
          """{"id":"id","type":"some type","payload":{"field":"yo"}}"""

        val res = readFromString[GraphQLWSInput](request)
        assert(res)(
          equalTo(
            GraphQLWSInput(
              `type` = "some type",
              id = Some("id"),
              payload = Some(InputValue.ObjectValue(Map("field" -> Value.StringValue("yo"))))
            )
          )
        )
      },
      test("can encode to JSON by jsoniter") {
        val res = GraphQLWSInput(
          `type` = "some type",
          id = Some("id"),
          payload = Some(InputValue.ObjectValue(Map("field" -> Value.StringValue("yo"))))
        )

        assertTrue(writeToString(res) == """{"type":"some type","id":"id","payload":{"field":"yo"}}""")
      }
    ) @@ TestUtils.skipJdk8
}
