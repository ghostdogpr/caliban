package caliban.interop.play

import caliban.{ GraphQLWSOutput, ResponseValue, Value }
import play.api.libs.json._
import zio.test.Assertion.{ equalTo, isRight }
import zio.test._

object GraphQLWSOutputPlaySpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLWSOutputPlaySpec")(
      test("can be parsed from JSON by play") {
        val request = Json
          .obj(
            "type"    -> JsString("some type"),
            "id"      -> JsString("id"),
            "payload" -> Json.obj(
              "field" -> JsString("yo")
            )
          )
        assert(request.validate[GraphQLWSOutput].asEither)(
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
      test("can be serialized to json [play]") {
        val res = GraphQLWSOutput(
          `type` = "some type",
          id = Some("id"),
          payload = Some(ResponseValue.ObjectValue(List("field" -> Value.StringValue("yo"))))
        )

        assertTrue(Json.toJson(res).toString() == """{"type":"some type","id":"id","payload":{"field":"yo"}}""")
      }
    )
}
