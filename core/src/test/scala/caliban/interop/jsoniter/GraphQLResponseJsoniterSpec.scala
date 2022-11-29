package caliban.interop.jsoniter

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, StringValue }
import caliban.parsing.adt.LocationInfo
import caliban.{ CalibanError, GraphQLResponse }
import com.github.plokhotnyuk.jsoniter_scala.core._
import zio.test.Assertion.equalTo
import zio.test.{ assert, assertTrue, ZIOSpecDefault }

object GraphQLResponseJsoniterSpec extends ZIOSpecDefault {

  override def spec =
    suite("GraphQLResponseJsoniterSpec")(
      test("can be converted to JSON [jsoniter]") {
        val response = GraphQLResponse(StringValue("data"), Nil)
        assertTrue(writeToString(response) == """{"data":"data"}""")
      },
      test("should include error objects for every error, including extensions [jsoniter]") {
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
          writeToString(response) ==
            """{"data":"data","errors":[{"message":"Resolution failed","locations":[{"line":2,"column":1}],"extensions":{"errorCode":"TEST_ERROR","myCustomKey":"my-value"}}]}"""
        )
      },
      test("should not include errors element when there are none [jsoniter]") {
        val response = GraphQLResponse(
          StringValue("data"),
          List.empty
        )

        assertTrue(writeToString(response) == """{"data":"data"}""")
      },
      test("can be parsed from JSON [jsoniter]") {
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

        assert(readFromString[GraphQLResponse[CalibanError]](req))(
          equalTo(
            GraphQLResponse(
              data = ObjectValue(List("value" -> IntValue("42"))),
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
