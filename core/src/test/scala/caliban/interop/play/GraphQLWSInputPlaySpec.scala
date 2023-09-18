package caliban.interop.play

import caliban.{ GraphQLWSInput, InputValue, Value }
import play.api.libs.json._
import zio.test.Assertion.{ equalTo, isRight }
import zio.test._

object GraphQLWSInputPlaySpec extends ZIOSpecDefault {

  override def spec =
    suite("GraphQLWSInputPlaySpec")(
      test("can be parsed from JSON by play") {
        val request = Json
          .obj(
            "type"    -> JsString("some type"),
            "id"      -> JsString("id"),
            "payload" -> Json.obj(
              "field" -> JsString("yo")
            )
          )
        assert(request.validate[GraphQLWSInput].asEither)(
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
      test("can be serialized to json [play]") {
        val res = GraphQLWSInput(
          `type` = "some type",
          id = Some("id"),
          payload = Some(InputValue.ObjectValue(Map("field" -> Value.StringValue("yo"))))
        )

        assertTrue(Json.toJson(res).toString() == """{"type":"some type","id":"id","payload":{"field":"yo"}}""")
      }
    )
}
