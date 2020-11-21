package caliban.client

import caliban.client.CalibanClientError.{ CommunicationError, DecodingError, ServerError }
import caliban.client.GraphQLResponseError.Location
import zio.test._
import zio.test.environment.TestEnvironment
import zio.test.Assertion.equalTo

object CalibanClientErrorSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] = {
    val expectedMsg = "COMM_ERROR_MSG"
    suite("CalibanClientErrorSpec")(
      suite("CommunicationError")(
        test("getMessage") {
          val error = CommunicationError(msg = expectedMsg)
          assert(error.getMessage)(equalTo(expectedMsg))
        }
      ),
      suite("DecodingError")(
        test("getMessage") {
          val error = DecodingError(msg = expectedMsg)
          assert(error.getMessage)(equalTo(expectedMsg))
        }
      ),
      suite("ServerError")(
        test("getMessage") {
          val graphQLResponseErrors = List(
            GraphQLResponseError(
              message = "Error1",
              locations = Option(List(Location(line = 1, column = 1))),
              path = Option(List(Left("somewhere"))),
              extensions = Option.empty
            )
          )
          val error = ServerError(graphQLResponseErrors)
          assert(error.getMessage)(equalTo(error.toString))
        }
      )
    )
  }
}
