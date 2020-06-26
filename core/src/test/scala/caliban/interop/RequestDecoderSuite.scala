package caliban.interop

import caliban.GraphQLRequest
import zio.test.Assertion.equalTo
import zio.test.environment.TestEnvironment
import zio.test.{ assert, suite, test, ZSpec }

object RequestDecoderSuite {
  def apply(label: String)(parseJson: String => GraphQLRequest): ZSpec[TestEnvironment, Any] =
    suite(label)(
      test("parses empty object") {
        val request = "{}"
        assert(parseJson(request))(
          equalTo(GraphQLRequest())
        )
      },
      test("parses just a query") {
        val request = s"""{"query":"{}"}"""
        assert(parseJson(request))(
          equalTo(GraphQLRequest(query = Some("{}")))
        )
      },
      test("parses a query with operationName") {
        val request = s"""{"query":"{}","operationName":"op"}"""
        assert(parseJson(request))(
          equalTo(GraphQLRequest(query = Some("{}"), operationName = Some("op")))
        )
      },
      test("parses an operationName with query") {
        val request = s"""{"operationName":"op","query":"{}"}"""
        assert(parseJson(request))(
          equalTo(GraphQLRequest(query = Some("{}"), operationName = Some("op")))
        )
      },
      test("parses a query with operationName and empty variables") {
        val request = s"""{"query":"{}","operationName":"op","variables":{}}"""
        assert(parseJson(request))(
          equalTo(GraphQLRequest(query = Some("{}"), operationName = Some("op"), variables = Some(Map.empty)))
        )
      }
    )
}
