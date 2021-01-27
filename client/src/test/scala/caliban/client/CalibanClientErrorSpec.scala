package caliban.client

import caliban.client.CalibanClientError.{ CommunicationError, DecodingError, ServerError }
import caliban.client.GraphQLResponseError.Location
import zio.test._
import zio.test.environment.TestEnvironment
import zio.test.Assertion.equalTo

object CalibanClientErrorSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] = {
    val msg = "SOME_MSG"
    suite("CalibanClientErrorSpec")(
      suite("CommunicationError")(
        test("getMessage - no innerThrowable") {
          val error = CommunicationError(msg = msg)
          assert(error.getMessage)(equalTo("Communication Error: SOME_MSG"))
        },
        test("getMessage - innerThrowable") {
          val error = CommunicationError(msg = msg, innerThrowable = Option(CommunicationError("INNER")))
          assert(error.getMessage)(equalTo("Communication Error: SOME_MSG Communication Error: INNER"))
        }
      ),
      suite("DecodingError")(
        test("getMessage - no innerThrowable") {
          val error = DecodingError(msg = msg)
          assert(error.getMessage)(equalTo("Decoding Error: SOME_MSG"))
        },
        test("getMessage - innerThrowable") {
          val error = DecodingError(msg = msg, innerThrowable = Option(DecodingError("INNER")))
          assert(error.getMessage)(equalTo("Decoding Error: SOME_MSG Decoding Error: INNER"))
        }
      ),
      suite("ServerError")(
        test("getMessage") {
          val graphQLResponseErrors = List(
            GraphQLResponseError(
              message = "Error1",
              locations = Option(List(Location(line = 1, column = 1))),
              path = Option(List(Left("somewhere"), Right(1))),
              extensions = Option.empty
            ),
            GraphQLResponseError(
              message = "Error2",
              locations = Option(List(Location(line = 1, column = 1))),
              path = Option(List(Left("somewhere"))),
              extensions = Option.empty
            )
          )
          val error                 = ServerError(graphQLResponseErrors)
          assert(error.getMessage)(
            equalTo(
              "Server Error: Error1 at line 1 and column 1 at path /somewhere[1]\nError2 at line 1 and column 1 at path /somewhere"
            )
          )
        }
      )
    )
  }
}
