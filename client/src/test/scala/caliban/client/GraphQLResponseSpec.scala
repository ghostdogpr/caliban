package caliban.client

import caliban.client.GraphQLResponseError.Location
import caliban.client.__Value.__ObjectValue
import com.github.plokhotnyuk.jsoniter_scala.core.readFromString
import zio.test.Assertion._
import zio.test._
import zio.ZIO

import scala.util.Try

object GraphQLResponseSpec extends ZIOSpecDefault {

  override def spec =
    suite("GraphQLResponseSpec")(
      test("can be parsed from JSON") {
        val response =
          """{"errors":[{"message":"Parse error on \"direction\" (STRING) at [1, 107]","locations":[{"line":1,"column":107}]}]}"""
        assert(Try(readFromString[GraphQLResponse](response)))(
          isSuccess(
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
      test("can parse extensions from JSON") {
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
          response   <- ZIO.attempt(readFromString[GraphQLResponse](responseRawJson))
          data       <- ZIO.attempt(readFromString[__Value](dataRawJson))
          extensions <- ZIO.attempt(readFromString[__ObjectValue](extensionsRawJson))
        } yield assertTrue(
          response == GraphQLResponse(
            Some(data),
            Nil,
            Some(extensions)
          )
        )
      }
    )
}
