package caliban

import caliban.CalibanError.ExecutionError
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
        assert(response.asJson)(equalTo(Json.obj("data" -> Json.fromString("data"))))
      },
      test("should include error only if non-empty") {
        val response = GraphQLResponse(StringValue("data"), List(ExecutionError("Resolution failed")))

        assert(response.asJson)(
          equalTo(
            Json.obj(
              "data"   -> Json.fromString("data"),
              "errors" -> Json.arr(Json.obj("message" -> Json.fromString("Resolution failed")))
            )
          )
        )
      }
    )
}
