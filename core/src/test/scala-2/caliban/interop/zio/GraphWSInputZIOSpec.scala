package caliban.interop.zio

import caliban.{ GraphQLWSInput, InputValue, Value }
import zio.json._
import zio.test.Assertion._
import zio.test._

object GraphWSInputZIOSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphWSInputZIOSpec")(
      test("can be parsed from JSON by zio-json") {
        val request =
          """{"id":"id","type":"some type","payload":{"field":"yo"}}"""

        val res = request.fromJson[GraphQLWSInput]
        assert(res)(
          isRight(
            equalTo(
              GraphQLWSInput(
                `type` = "some type",
                id = Some("id"),
                payload = Some(InputValue.ObjectValue(Map("field" -> Value.StringValue("yo"))))
              )
            )
          )
        )
      },
      test("can encode to JSON by zio-json") {
        val res = GraphQLWSInput(
          `type` = "some type",
          id = Some("id"),
          payload = Some(InputValue.ObjectValue(Map("field" -> Value.StringValue("yo"))))
        )

        assertTrue(res.toJson == """{"type":"some type","id":"id","payload":{"field":"yo"}}""")
      }
    )
}
