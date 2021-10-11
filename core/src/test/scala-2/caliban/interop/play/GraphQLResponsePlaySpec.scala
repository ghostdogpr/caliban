package caliban.interop.play

import caliban.CalibanError
import caliban.CalibanError.ExecutionError
import caliban.GraphQLResponse
import caliban.ResponseValue
import caliban.ResponseValue.ListValue
import caliban.ResponseValue.ObjectValue
import caliban.Value
import caliban.Value.FloatValue
import caliban.Value.IntValue
import caliban.Value.StringValue
import caliban.parsing.adt.LocationInfo
import play.api.libs.json._
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object GraphQLResponsePlaySpec extends DefaultRunnableSpec {

  val writer: Writes[GraphQLResponse[Any]] = implicitly[Writes[GraphQLResponse[Any]]]

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
      },
      test("reads a graphql response [play]") {
        val req =
          """
            |{
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

        assert(Json.parse(req).validate[GraphQLResponse[CalibanError]].asEither)(
          isRight(
            equalTo(
              GraphQLResponse(
                data = ObjectValue(List("value" -> Value.IntValue("42"))),
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
        )
      }
    )
}
