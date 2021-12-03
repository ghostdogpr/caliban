package caliban.interop.circe

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, StringValue }
import caliban.parsing.adt.LocationInfo
import caliban.{ CalibanError, GraphQLResponse }
import io.circe._
import io.circe.parser.decode
import io.circe.syntax._
import zio.test._

object GraphQLResponseCirceSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLResponseCirceSpec")(
      test("can be converted to JSON [circe]") {
        val response = GraphQLResponse(StringValue("data"), Nil)
        assertTrue(response.asJson == Json.obj("data" -> Json.fromString("data")))
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
              locationInfo = Some(LocationInfo(1, 2)),
              extensions = Some(ObjectValue(errorExtensions))
            )
          )
        )

        assertTrue(
          response.asJson == Json.obj(
            "data"   -> Json.fromString("data"),
            "errors" -> Json.arr(
              Json.obj(
                "message"    -> Json.fromString("Resolution failed"),
                "locations"  -> Json.arr(Json.obj("column" -> Json.fromInt(1), "line" -> Json.fromInt(2))),
                "extensions" -> Json.obj("errorCode" -> "TEST_ERROR".asJson, "myCustomKey" -> "my-value".asJson)
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

        assertTrue(
          response.asJson == Json.obj(
            "data" -> Json.fromString("data")
          )
        )
      },
      test("can be parsed from JSON [circe]") {
        val req =
          """ |{
            |   "data":{"value": 42},
            |   "errors":[
            |     {
            |       "message":"boom",
            |       "path": ["step", 0],
            |       "locations": [{"column": 1, "line": 2}],
            |       "extensions": {
            |         "argumentName": "id",
            |         "code": "BAD_USER_INPUT",
            |         "exception": {
            |           "stacktrace": [
            |              "trace"
            |           ]
            |         }
            |       }
            |     }]
            |}""".stripMargin

        assertTrue(
          decode[GraphQLResponse[CalibanError]](req) == Right(
            GraphQLResponse(
              data = ObjectValue(List("value" -> IntValue(BigInt(42)))),
              errors = List(
                ExecutionError(
                  "boom",
                  path = List(Left("step"), Right(0)),
                  locationInfo = Some(LocationInfo(1, 2)),
                  extensions = Some(
                    ObjectValue(
                      List(
                        "argumentName" -> StringValue("id"),
                        "code"         -> StringValue("BAD_USER_INPUT"),
                        "exception"    -> ObjectValue(
                          List("stacktrace" -> ListValue(List(StringValue("trace"))))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      }
    )
}
