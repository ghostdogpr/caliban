package caliban.interop

import caliban.CalibanError.{ ExecutionError, ValidationError }
import caliban.ResponseValue.ObjectValue
import caliban.Value.{ BooleanValue, EnumValue, FloatValue, IntValue, StringValue }
import caliban.parsing.adt.LocationInfo
import caliban.{ CalibanError, GraphQLResponse }
import org.skyscreamer.jsonassert.JSONAssert
import zio.test.environment.TestEnvironment
import zio.test.{ assert, suite, test, Assertion, ZSpec }
import zio.test.Assertion.Render._
import zio.test.Assertion.assertion

object ResponseEncoderSuite {

  def apply(label: String)(asJson: GraphQLResponse[CalibanError] => String): ZSpec[TestEnvironment, Any] = {

    def equalsJson(expected: String): Assertion[String] =
      assertion[String]("isJson")(param(expected)) { actual =>
        JSONAssert.assertEquals(expected, actual, false)
        true
      }

    suite(label)(
      test("only data") {
        val response = GraphQLResponse(StringValue("data"), Nil)
        assert(asJson(response))(
          equalsJson(s"""{"data":"data"}""")
        )
      },
      test("ExecutionError with extensions") {
        val response = GraphQLResponse(
          StringValue("data"),
          List(
            ExecutionError(
              "Resolution failed",
              extensions = Some(
                ObjectValue(
                  List(
                    ("errorCode", StringValue("TEST_ERROR")),
                    ("myCustomKey", StringValue("my-value"))
                  )
                )
              )
            )
          )
        )

        assert(asJson(response))(
          equalsJson(
            s"""{
               "data": "data",
               "errors": [{
                 "message": "Resolution failed",
                 "extensions": {
                   "errorCode": "TEST_ERROR",
                   "myCustomKey": "my-value"
                 }
               }]
            }"""
          )
        )
      },
      test("ValidationError") {
        val response = GraphQLResponse(
          StringValue("data"),
          List(
            ValidationError(
              "Validation failed",
              "this is an invalid object",
              Some(LocationInfo(1, 5)),
              Some(
                ObjectValue(
                  List(
                    "name"   -> StringValue("martin"),
                    "age"    -> IntValue(3),
                    "active" -> BooleanValue(true),
                    "planet" -> EnumValue("EARTH"),
                    "weight" -> FloatValue(5.4)
                  )
                )
              )
            )
          )
        )
        assert(asJson(response))(
          equalsJson(
            s"""{
                "data": "data",
                "errors": [{
                  "message": "Validation failed",
                  "locations": [{
                    "line": 5,
                    "column": 1
                  }],
                  "extensions": {
                    "name": "martin",
                    "age": 3,
                    "active": true,
                    "planet": "EARTH",
                    "weight": 5.4
                  }
                }]    
              }"""
          )
        )
      }
    )
  }
}
