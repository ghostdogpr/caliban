package caliban.interop.zio

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, StringValue }
import caliban.parsing.adt.LocationInfo
import caliban.{ CalibanError, GraphQLResponse, PathValue }
import zio.json._
import zio.test.Assertion.{ equalTo, isRight }
import zio.test.{ assert, assertTrue, ZIOSpecDefault }

object GraphQLResponseZIOSpec extends ZIOSpecDefault {
  implicit val encoder: JsonEncoder[GraphQLResponse[Any]] = GraphQLResponse.zioJsonCodec.encoder

  override def spec =
    suite("GraphQLResponseZIOSpec")(
      test("can be converted to JSON [zio]") {
        val response = GraphQLResponse(StringValue("data"), Nil)
        assertTrue(response.toJson == """{"data":"data"}""")
      },
      test("should include error objects for every error, including extensions [zio]") {
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
          response.toJson == """{"data":"data","errors":[{"message":"Resolution failed","locations":[{"line":2,"column":1}],"extensions":{"errorCode":"TEST_ERROR","myCustomKey":"my-value"}}]}"""
        )
      },
      test("should not include errors element when there are none [zio]") {
        val response = GraphQLResponse(
          StringValue("data"),
          List.empty
        )

        assertTrue(response.toJson == """{"data":"data"}""")
      },
      test("can be parsed from JSON [zio]") {
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

        assert(req.fromJson[GraphQLResponse[CalibanError]])(
          isRight(
            equalTo(
              GraphQLResponse(
                data = ObjectValue(List("value" -> IntValue(42))),
                errors = List(
                  ExecutionError(
                    "boom",
                    path = List(PathValue.Key("step"), PathValue.Index(0)),
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
