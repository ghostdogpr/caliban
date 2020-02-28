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

      suite("ValidationSchemaSpec")(
        suite("InputObjects")(
          testM("name can't start with '__'") {
            check(
              graphQL(resolverWrongMutationUnderscore),
              "A input value in InputObject can't start with '__': __name"
            )
          },
          testM("should only contain types for which IsInputType(type) is true") {
            check(
              graphQL(resolverWrongMutationUnion),
              "UnionInput is of kind UNION, must be an InputType"
            )
          }
        ),
        suite("Interface")(
          testM("must define one or more fields") {
            check(
              graphQL(resolverEmptyInferface),
              "message"
            )
          },
          testM("field name can't start with '__'") {
            check(
              graphQL(resolverInferfaceWrongFieldName),
              "A field in Interface can't start with '__': __name"
            )
          },
          testM("field argument name can't start with '__'") {
            check(
              graphQL(resolverInterfaceWrongArgumentName),
              "A argument input value in Interface can't start with '__': __name"
            )
          },
          testM("field argument can't be output type") {
            check(
              graphQL(resolverInterfaceWrongArgumentInputType),
              "UnionInput is of kind UNION, must be an InputType"
            )
          }
        )
      )
    })
