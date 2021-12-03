package caliban.interop.circe

import caliban.{ GraphQLWSOutput, ResponseValue, Value }
import io.circe._
import io.circe.syntax._
import zio.test.Assertion._
import zio.test._

object GraphQLWSOutputCirceSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLWSOutputCirceSpec")(
      test("can be parsed from JSON by circe") {
        val request = Json
          .obj(
            "type"    -> Json.fromString("some type"),
            "id"      -> Json.fromString("id"),
            "payload" -> Json.obj(
              "field" -> Json.fromString("yo")
            )
          )
        assert(request.as[GraphQLWSOutput])(
          isRight(
            equalTo(
              GraphQLWSOutput(
                `type` = "some type",
                id = Some("id"),
                payload = Some(ResponseValue.ObjectValue(List("field" -> Value.StringValue("yo"))))
              )
            )
          )
        )
      },
      test("can encode to JSON by circe") {
        val res = GraphQLWSOutput(
          `type` = "some type",
          id = Some("id"),
          payload = Some(ResponseValue.ObjectValue(List("field" -> Value.StringValue("yo"))))
        )

        assertTrue(res.asJson.noSpaces == """{"id":"id","type":"some type","payload":{"field":"yo"}}""")
      }
    )
}
