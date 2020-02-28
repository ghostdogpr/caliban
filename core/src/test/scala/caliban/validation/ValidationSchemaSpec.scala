package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.GraphQL
import caliban.GraphQL.graphQL
import caliban.TestUtils.InvalidSchemas._
import zio.IO
import zio.test.Assertion._
import zio.test._

object ValidationSchemaSpec
    extends DefaultRunnableSpec({
      def check(gql: GraphQL[Any], expectedMessage: String): IO[ValidationError, TestResult] =
        assertM(gql.interpreter.run, fails[ValidationError](hasField("msg", _.msg, equalTo(expectedMessage))))

      suite("ValidationSchemaSpec")({
        suite("InputObjects")(
          testM("name can't start with '__'") {
            check(
              graphQL(resolverWrongMutationUnderscore),
              "InputObject can't start with '__': __name"
            )
          },
          testM("should only contain types for which IsInputType(type) is true") {
            check(
              graphQL(resolverWrongMutationUnion),
              "UnionInput is of kind UNION, must be an InputType"
            )
          }
        )
      })
    })
