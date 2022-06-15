package caliban.interop.circe

import caliban.{ GraphQLWSInput, InputValue, Value }
import io.circe._
import io.circe.syntax._
import zio.test.Assertion._
import zio.test._

object GraphQLWSInputCirceSpec extends ZIOSpecDefault {

  override def spec =
    suite("GraphQLWSInputCirceSpec")(
      test("can be parsed from JSON by circe") {
        val request = Json
          .obj(
            "type"    -> Json.fromString("some type"),
            "id"      -> Json.fromString("id"),
            "payload" -> Json.obj(
              "field" -> Json.fromString("yo")
            )
          )
        assert(request.as[GraphQLWSInput])(
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
      test("can encode to JSON by circe") {
        val res = GraphQLWSInput(
          `type` = "some type",
          id = Some("id"),
          payload = Some(InputValue.ObjectValue(Map("field" -> Value.StringValue("yo"))))
        )

        assertTrue(res.asJson.noSpaces == """{"id":"id","type":"some type","payload":{"field":"yo"}}""")
      }
    )
}
