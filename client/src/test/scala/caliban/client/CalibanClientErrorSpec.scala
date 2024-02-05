package caliban.client

import caliban.client.CalibanClientError.{ CommunicationError, DecodingError, ServerError }
import caliban.client.GraphQLResponseError.Location
import zio.test._

import caliban.client.CalibanClientErrorSpec.test
import caliban.client.__Value.__ObjectValue

object CalibanClientErrorSpec extends ZIOSpecDefault {
  override def spec = {
    val msg = "SOME_MSG"
    suite("CalibanClientErrorSpec")(
      suite("CommunicationError")(
        test("getMessage - no innerThrowable") {
          val error = CommunicationError(msg = msg)
          assertTrue(error.getMessage == "Communication Error: SOME_MSG")
          assertTrue(error.render(true) == "Communication Error: SOME_MSG")
          assertTrue(error.render(false) == "Communication Error: SOME_MSG")
        },
        test("getMessage - innerThrowable") {
          val error = CommunicationError(msg = msg, innerThrowable = Option(CommunicationError("INNER")))
          assertTrue(error.getMessage == "Communication Error: SOME_MSG Communication Error: INNER")
          assertTrue(error.render(true) == "Communication Error: SOME_MSG Communication Error: INNER")
          assertTrue(error.render(false) == "Communication Error: SOME_MSG Communication Error: INNER")
        }
      ),
      suite("DecodingError")(
        test("getMessage - no innerThrowable") {
          val error = DecodingError(msg = msg)
          assertTrue(error.getMessage == "Decoding Error: SOME_MSG")
          assertTrue(error.render(true) == "Decoding Error: SOME_MSG")
          assertTrue(error.render(false) == "Decoding Error: SOME_MSG")
        },
        test("getMessage - innerThrowable") {
          val error = DecodingError(msg = msg, innerThrowable = Option(DecodingError("INNER")))
          assertTrue(error.getMessage == "Decoding Error: SOME_MSG Decoding Error: INNER")
          assertTrue(error.render(true) == "Decoding Error: SOME_MSG Decoding Error: INNER")
          assertTrue(error.render(false) == "Decoding Error: SOME_MSG Decoding Error: INNER")
        }
      ),
      suite("ServerError")(
        test("getMessage") {
          val graphQLResponseErrors = List(
            GraphQLResponseError(
              message = "Error1",
              locations = Option(List(Location(line = 1, column = 1))),
              path = Option(List(Left("somewhere"), Right(1))),
              extensions = Option(
                __ObjectValue(List("key1" -> __Value.__StringValue("value1"), "key2" -> __Value.__NumberValue(2)))
              )
            ),
            GraphQLResponseError(
              message = "Error2",
              locations = Option(List(Location(line = 1, column = 1))),
              path = Option(List(Left("somewhere"))),
              extensions = Option.empty
            )
          )
          val error                 = ServerError(graphQLResponseErrors)
          assertTrue(
            error.getMessage == "Server Error: Error1 at line 1 and column 1 at path /somewhere[1]\nError2 at line 1 and column 1 at path /somewhere"
          )

          assertTrue(
            error.render(
              false
            ) == "Server Error: Error1 at line 1 and column 1 at path /somewhere[1]\nError2 at line 1 and column 1 at path /somewhere"
          )
        },
        test("render") {
          val graphQLResponseErrors = List(
            GraphQLResponseError(
              message = "Error1",
              locations = Option(List(Location(line = 1, column = 1))),
              path = Option(List(Left("somewhere"), Right(1))),
              extensions = Option(
                __ObjectValue(List("key1" -> __Value.__StringValue("value1"), "key2" -> __Value.__NumberValue(2)))
              )
            ),
            GraphQLResponseError(
              message = "Error2",
              locations = Option(List(Location(line = 1, column = 1))),
              path = Option(List(Left("somewhere"))),
              extensions = Option.empty
            )
          )
          val error                 = ServerError(graphQLResponseErrors)
          assertTrue(
            error.render(
              true
            ) == "Server Error: Error1 at line 1 and column 1 at path /somewhere[1] Extensions: {key1:\"value1\",key2:2}\nError2 at line 1 and column 1 at path /somewhere"
          )
        }
      )
    )
  }
}
