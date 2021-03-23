package caliban.client

import caliban.client.GraphQLResponseError.Location
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test._

import zio.ZIO

import io.circe.parser.decode
import io.circe.Json

object GraphQLResponseSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("GraphQLResponseSpec")(
      test("can be parsed from JSON") {
        val response =
          """{"errors":[{"message":"Parse error on \"direction\" (STRING) at [1, 107]","locations":[{"line":1,"column":107}]}]}"""
        assert(decode[GraphQLResponse](response))(
          isRight(
            equalTo(
              GraphQLResponse(
                None,
                List(
                  GraphQLResponseError(
                    "Parse error on \"direction\" (STRING) at [1, 107]",
                    Some(List(Location(1, 107))),
                    None,
                    None
                  )
                )
              )
            )
          )
        )
      },
      testM("can parse extensions from JSON") {
        val dataRawJson =
          """|{
             |  "shop": {
             |    "products": {
             |      "edges": [
             |        {
             |          "node": {
             |            "id": "gid://shopify/Product/5388718768290",
             |            "handle": "morning-glow-eye-gel"
             |          }
             |        }
             |      ]
             |    }
             |  }
             |}""".stripMargin

        val extensionsRawJson =
          """|{
             |  "cost": {
             |    "requestedQueryCost": 8,
             |    "actualQueryCost": 8,
             |    "throttleStatus": {
             |      "maximumAvailable": 1000,
             |      "currentlyAvailable": 992,
             |      "restoreRate": 50
             |    }
             |  }
             |}""".stripMargin

        val responseRawJson =
          s"""|{
              |  "data": $dataRawJson,
              |  "extensions": $extensionsRawJson
              |}""".stripMargin

        for {
          response   <- ZIO.fromEither(decode[GraphQLResponse](responseRawJson))
          data       <- ZIO.fromEither(decode[__Value](dataRawJson))
          extensions <- ZIO.fromEither(decode[Json](extensionsRawJson))
        } yield assert(response)(
          equalTo(
            GraphQLResponse(
              Some(data),
              Nil,
              Some(extensions)
            )
          )
        )
      }
    )
}
