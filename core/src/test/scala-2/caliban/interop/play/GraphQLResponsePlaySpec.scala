package caliban.interop.play

import caliban.CalibanError.ExecutionError
import caliban.GraphQLResponse
import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment
import play.api.libs.json._

object GraphQLResponsePlaySpec extends DefaultRunnableSpec {

  val writer = implicitly[Writes[GraphQLResponse[Any]]]

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLResponsePlaySpec")(
      test("can be converted to JSON [play]") {
        val response = GraphQLResponse(StringValue("data"), Nil)
        assert(writer.writes(response))(
          equalTo(Json.obj("data" -> JsString("data")))
        )
      },
      test("should include error objects for every error, including extensions [play]") {
        val errorExtensions = List(
          ("errorCode", StringValue("TEST_ERROR")),
          ("myCustomKey", StringValue("my-value"))
        )

        val response = GraphQLResponse(
          StringValue("data"),
          List(
            ExecutionError(
              "Resolution failed",
              extensions = Some(ObjectValue(errorExtensions))
            )
          )
        )

        assert(writer.writes(response))(
          equalTo(
            Json.obj(
              "data"   -> JsString("data"),
              "errors" -> Json.arr(
                Json.obj(
                  "message"    -> JsString("Resolution failed"),
                  "extensions" -> Json.obj("errorCode" -> JsString("TEST_ERROR"), "myCustomKey" -> JsString("my-value"))
                )
              )
            )
          )
        )
      },
      test("should not include errors element when there are none [play]") {
        val response = GraphQLResponse(
          StringValue("data"),
          List.empty
        )

        assert(writer.writes(response))(
          equalTo(
            Json.obj(
              "data" -> JsString("data")
            )
          )
        )
      }
    )
}
