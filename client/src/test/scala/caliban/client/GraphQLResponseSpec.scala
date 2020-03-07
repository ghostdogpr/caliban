package caliban.client

import caliban.client.GraphQLResponseError.Location
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test._

object GraphQLResponseSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLResponseSpec")(
      test("can be parsed from JSON") {
        val response =
          """{"errors":[{"message":"Parse error on \"direction\" (STRING) at [1, 107]","locations":[{"line":1,"column":107}]}]}"""
        assert(io.circe.parser.decode[GraphQLResponse](response))(
          isRight(
            equalTo(
              GraphQLResponse(
                None,
                List(
                  GraphQLResponseError(
                    "Parse error on \"direction\" (STRING) at [1, 107]",
                    Some(List(Location(1, 107))),
                    None
                  )
                )
              )
            )
          )
        )
      }
    )
}
