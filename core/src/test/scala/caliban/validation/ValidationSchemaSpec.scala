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
        assertM(gql.interpreter.run)(fails[ValidationError](hasField("msg", _.msg, equalTo(expectedMessage))))

      suite("ValidationSchemaSpec")(
        suite("InputObjects")(
          testM("name can't start with '__'") {
            check(
              graphQL(resolverWrongMutationUnderscore),
              "InputValue '__name' of InputObject 'DoubleUnderscoreArgInput' can't start with '__'"
            )
          },
          testM("should only contain types for which IsInputType(type) is true") {
            check(
              graphQL(resolverWrongMutationUnion),
              "UnionInput of InputValue 'union' of InputObject 'UnionArgInput' is of kind UNION, must be an InputType"
            )
          }
        ),
        suite("Interface")(
          testM("must define one or more fields") {
            check(
              graphQL(resolverEmptyInferface),
              "Interface 'InterfaceEmpty' does not have fields"
            )
          },
          testM("field name can't start with '__'") {
            check(
              graphQL(resolverInferfaceWrongFieldName),
              "Field '__name' of Interface 'InterfaceWrongFieldName' can't start with '__'"
            )
          },
          testM("field argument name can't start with '__'") {
            check(
              graphQL(resolverInterfaceWrongArgumentName),
              "InputValue '__name' of Field 'x' of Interface 'InterfaceWrongArgumentName' can't start with '__'"
            )
          },
          testM("field argument can't be output type") {
            check(
              graphQL(resolverInterfaceWrongArgumentInputType),
              "UnionInput of InputValue 'union' of InputObject 'UnionArgInput' is of kind UNION, must be an InputType"
            )
          }
        )
      )
    })
