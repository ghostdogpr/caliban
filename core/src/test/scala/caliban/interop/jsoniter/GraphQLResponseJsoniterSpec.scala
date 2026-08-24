package caliban.interop.jsoniter

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ IntValue, NullValue, StringValue }
import caliban.parsing.adt.LocationInfo
import caliban.{ CalibanError, GraphQLResponse, PathValue, ResponseValue, TestUtils }
import com.github.plokhotnyuk.jsoniter_scala.core._
import zio.test.Assertion.equalTo
import zio.test.{ assert, assertTrue, ZIOSpecDefault }

import scala.util.Try

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

        val response: GraphQLResponse[Any] = GraphQLResponse(
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
      test("encodes directly with caller-selected envelope fields [jsoniter]") {
        val response: GraphQLResponse[Any] = GraphQLResponse(
          NullValue,
          List(ExecutionError("boom")),
          Some(ObjectValue(List("cacheControl" -> StringValue("private"), "traceId" -> StringValue("trace-1"))))
        )
        val codec                          = GraphQLResponseJsoniter.codec(
          keepDataOnErrors = false,
          excludeExtensions = Set("cacheControl")
        )

        assertTrue(
          writeToString(response)(codec) ==
            """{"errors":[{"message":"boom"}],"extensions":{"traceId":"trace-1"}}"""
        )
      },
      test("direct encoding matches the materialized response envelope [jsoniter]") {
        val responses: List[GraphQLResponse[Any]] = List(
          GraphQLResponse(ObjectValue(List("nested" -> ListValue(List(IntValue(1), NullValue)))), Nil),
          GraphQLResponse(NullValue, List(ExecutionError("boom"))),
          GraphQLResponse(StringValue("data"), Nil, Some(ObjectValue(Nil)), Some(false))
        )

        assertTrue(responses.forall(response => writeToString(response) == writeToString(response.toResponseValue)))
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
      },
      test("preserves response extensions when parsing JSON [jsoniter]") {
        val response = readFromString[GraphQLResponse[CalibanError]](
          """{"data":{"value":42},"extensions":{"traceId":"trace-1"}}"""
        )

        assertTrue(
          response.extensions.contains(ObjectValue(List("traceId" -> StringValue("trace-1"))))
        )
      },
      test("accepts errors-only responses and preserves response metadata [jsoniter]") {
        val response = readFromString[GraphQLResponse[CalibanError]](
          """{"errors":[{"message":"boom"}],"extensions":{"traceId":"trace-1"},"hasNext":false}"""
        )

        assertTrue(
          response.data == NullValue,
          response.errors.map(_.msg) == List("boom"),
          response.extensions.contains(ObjectValue(List("traceId" -> StringValue("trace-1")))),
          response.hasNext.contains(false)
        )
      },
      test("accepts explicit null data and an empty errors list [jsoniter]") {
        val explicitNull = readFromString[GraphQLResponse[CalibanError]]("""{"data":null}""")
        val emptyErrors  = Try(readFromString[GraphQLResponse[CalibanError]]("""{"errors":[]}"""))

        assertTrue(
          explicitNull == GraphQLResponse(NullValue, Nil),
          emptyErrors.toOption.contains(GraphQLResponse(NullValue, Nil))
        )
      },
      test("accepts null response metadata and rejects malformed values [jsoniter]") {
        val nullMetadata = List(
          """{"data":null,"errors":null}""",
          """{"data":null,"extensions":null}""",
          """{"data":null,"hasNext":null}"""
        )
        val invalid      = List(
          """{}""",
          """{"errors":{}}""",
          """{"errors":[null]}""",
          """{"data":null,"extensions":[]}""",
          """{"data":null,"hasNext":"false"}"""
        )

        assertTrue(
          nullMetadata.forall(value =>
            Try(readFromString[GraphQLResponse[CalibanError]](value)).toOption.contains(GraphQLResponse(NullValue, Nil))
          ),
          invalid.forall(value => Try(readFromString[GraphQLResponse[CalibanError]](value)).isFailure)
        )
      },
      test("should correctly write keys containing UTF-8") {
        val response = GraphQLResponse(ObjectValue(List("utf8〜key" -> StringValue("any"))), Nil)
        assertTrue(writeToString(response) == """{"data":{"utf8〜key":"any"}}""")
      },
      test("decodes a bare out-of-Int/Long-range integer at end of input [jsoniter]") {
        assertTrue(
          readFromString[ResponseValue]("3000000000") == IntValue.LongNumber(3000000000L),
          readFromString[ResponseValue]("9999999999999999999") ==
            IntValue.BigIntNumber(BigInt("9999999999999999999")),
          readFromString[ResponseValue]("[3000000000]") == ListValue(List(IntValue.LongNumber(3000000000L)))
        )
      }
    )
}
