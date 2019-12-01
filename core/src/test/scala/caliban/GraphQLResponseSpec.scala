package caliban

import caliban.Value._
import io.circe._
import io.circe.syntax._
import zio.test.Assertion._
import zio.test._

object GraphQLResponseSpec
    extends DefaultRunnableSpec(
      suite("GraphQLResponseSpec")(
        test("can be converted to JSON") {
          val response = GraphQLResponse(StringValue("data"), Nil)
          assert(
            response.asJson,
            equalTo(Json.obj("data" -> Json.fromString("data"), "errors" -> Json.arr()))
          )
        }
      )
    )
