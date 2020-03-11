package caliban

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.ObjectValue
import caliban.Value._
import io.circe._
import io.circe.syntax._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object GraphQLResponseSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLResponseSpec")(
      test("can be converted to JSON") {
        val response = GraphQLResponse(StringValue("data"), Nil)
        assert(response.asJson)(
          equalTo(Json.obj("data" -> Json.fromString("data")))
        )
      },
      test("should include error objects for every error, including extensions") {

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

        assert(response.asJson)(
          equalTo(
            Json.obj(
              "data" -> Json.fromString("data"),
              "errors" -> Json.arr(
                Json.obj(
                  "message"    -> Json.fromString("Resolution failed"),
                  "extensions" -> Json.obj("errorCode" -> "TEST_ERROR".asJson, "myCustomKey" -> "my-value".asJson)
                )
              )
            )
          )
        )
      },
      test("should not include errors element when there are none") {
        val response = GraphQLResponse(
          StringValue("data"),
          List.empty
        )

        assert(response.asJson)(
          equalTo(
            Json.obj(
              "data" -> Json.fromString("data")
            )
          )
        )
      }
    )
}
