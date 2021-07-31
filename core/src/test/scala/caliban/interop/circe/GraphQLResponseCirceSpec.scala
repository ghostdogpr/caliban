package caliban.interop.circe

import caliban.CalibanError
import caliban.CalibanError.ExecutionError
import caliban.GraphQLResponse
import caliban.ResponseValue.ObjectValue
import caliban.Value.IntValue
import caliban.Value.StringValue
import caliban.Value.FloatValue
import io.circe._
import io.circe.syntax._
import io.circe.parser.decode
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object GraphQLResponseCirceSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLResponseCirceSpec")(
      test("can be converted to JSON [circe]") {
        val response = GraphQLResponse(StringValue("data"), Nil)
        assert(response.asJson)(
          equalTo(Json.obj("data" -> Json.fromString("data")))
        )
      },
      test("should include error objects for every error, including extensions [circe]") {

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
              "data"   -> Json.fromString("data"),
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
      test("should not include errors element when there are none [circe]") {
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
      },
      test("can be parsed from JSON [circe]") {
        val req = """{"data":{"value": 42},"errors":[{"message":"boom"}]}"""

        assert(decode[GraphQLResponse[CalibanError]](req))(
          equalTo(
            Right(
              GraphQLResponse(
                data = ObjectValue(List("value" -> IntValue(BigInt(42)))),
                errors = List(ExecutionError("boom"))
              )
            )
          )
        )
      }
    )
}
