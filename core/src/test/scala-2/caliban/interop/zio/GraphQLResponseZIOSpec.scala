package caliban.interop.zio

import caliban.GraphQLResponse
import zio.json.{ EncoderOps, JsonEncoder }
import zio.test._
import Assertion._
import caliban.CalibanError.ExecutionError
import caliban.ResponseValue.ObjectValue
import caliban.Value.StringValue

object GraphQLResponseZIOSpec extends DefaultRunnableSpec {
  implicit val encoder: JsonEncoder[GraphQLResponse[Any]] = GraphQLResponse.zioJsonEncoder

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    suite("GraphQLResponseZIOSpec")(
      test("can be converted to JSON [zio]") {
        val response = GraphQLResponse(StringValue("data"), Nil)
        assert(response.toJson)(equalTo("""{"data":"data"}"""))
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
              extensions = Some(ObjectValue(errorExtensions))
            )
          )
        )

        assert(response.toJson)(
          equalTo(
            """{"data":"data","errors":[{"message":"Resolution failed","extensions":{"errorCode":"TEST_ERROR","myCustomKey":"my-value"}}]}"""
          )
        )
      },
      test("should not include errors element when there are none [zio]") {
        val response = GraphQLResponse(
          StringValue("data"),
          List.empty
        )

        assert(response.toJson)(equalTo("""{"data":"data"}"""))
      }
    )
}
