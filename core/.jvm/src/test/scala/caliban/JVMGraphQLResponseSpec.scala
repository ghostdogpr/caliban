package caliban

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.ObjectValue
import caliban.Value._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment
import spray.json.{ JsArray, JsObject, JsString, JsonWriter }

object JVMGraphQLResponseSpec extends DefaultRunnableSpec {

  val writer = implicitly[JsonWriter[GraphQLResponse[Any]]]

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("JVMGraphQLResponseSpec")(
      test("can be converted to JSON (spray)") {
        val response = GraphQLResponse(StringValue("data"), Nil)
        assert(writer.write(response))(
          equalTo(JsObject("data" -> JsString("data")))
        )
      },
      test("should include error objects for every error, including extensions (spray)") {

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

        assert(writer.write(response))(
          equalTo(
            JsObject(
              "data" -> JsString("data"),
              "errors" -> JsArray(
                JsObject(
                  "message"    -> JsString("Resolution failed"),
                  "extensions" -> JsObject("errorCode" -> JsString("TEST_ERROR"), "myCustomKey" -> JsString("my-value"))
                )
              )
            )
          )
        )
      },
      test("should not include errors element when there are none (spray)") {
        val response = GraphQLResponse(
          StringValue("data"),
          List.empty
        )

        assert(writer.write(response))(
          equalTo(
            JsObject(
              "data" -> JsString("data")
            )
          )
        )
      }
    )
}
