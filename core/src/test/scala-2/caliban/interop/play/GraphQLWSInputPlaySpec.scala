package caliban.interop.play

import caliban.{ GraphQLRequest, GraphQLWSInput, InputValue, Value }
import play.api.libs.json._
import zio.test.Assertion.{ equalTo, isRight }
import zio.test._
import zio.test.environment.TestEnvironment

object GraphQLWSInputPlaySpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
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

        assert(Json.toJson(res).toString())(
          equalTo(
            """{"type":"some type","id":"id","payload":{"field":"yo"}}"""
          )
        )
      }
    )
}
