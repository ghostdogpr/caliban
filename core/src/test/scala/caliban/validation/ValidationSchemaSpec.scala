package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.{ GraphQL, RootResolver }
import caliban.GraphQL.graphQL
import caliban.TestUtils.InvalidSchemas._
import zio.IO
import zio.test.Assertion._
import zio.test.environment.TestEnvironment
import zio.test._

object ValidationSchemaSpec extends DefaultRunnableSpec {

  def check(gql: GraphQL[Any], expectedMessage: String): IO[ValidationError, TestResult] =
    assertM(gql.interpreter.run)(fails[ValidationError](hasField("msg", _.msg, equalTo(expectedMessage))))

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("ValidationSchemaSpec")(
      suite("Directives")(
        testM("name on a type can't start with '__'") {
          check(
            graphQL(resolverWrongDirectiveName),
            "Directive '__name' of Type 'TestWrongDirectiveName' can't start with '__'"
          )
        },
        testM("name on a field type can't start with '__'") {
          check(
            graphQL(resolverWrongFieldDirectiveName),
            "Directive '__name' of Field 'field' of Type 'TestWrongFieldDirectiveName' can't start with '__'"
          )
        },
        testM("name on a inputValue on a type can't start with '__'") {
          check(
            graphQL(resolverWrongInputFieldDirectiveName),
            "Directive '__name' of InputValue 'inputValue' of Type 'WronDirectiveNameInput' can't start with '__'"
          )
        },
        testM("name on a inputValue on a field type can't start with '__'") {
          check(
            graphQL(resolverWrongFieldArgDirectiveName),
            "Directive '__name' of InputValue 'inputValue' of Field 'field' of Type 'TestWrongFieldArgDirectiveName' can't start with '__'"
          )
        },
        testM("argument name can't start with '__'") {
          check(
            graphQL(resolverWrongArgumentDirectiveName),
            "Argument '__name' of Directive 'name' of Field 'field' of Type 'TestWrongArgumentDirectiveName' can't start with '__'"
          )
        }
      ),
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
        },
        testM("clashing input and object types") {
          check(
            graphQL(resolverClashingObjects),
            "Type 'ClashingObjectInput' is defined multiple times (INPUT_OBJECT in caliban.TestUtils.InvalidSchemas.ClashingObject, OBJECT in caliban.TestUtils.InvalidSchemas.ClashingObjectInput)."
          )
        },
        testM("clashing names from different packages") {
          check(
            graphQL(resolverClashingNames),
            "Type 'C' is defined multiple times (OBJECT in caliban.TestUtils.InvalidSchemas.A.C, OBJECT in caliban.TestUtils.InvalidSchemas.B.C)."
          )
        },
        testM("missing root query") {
          check(
            graphQL(RootResolver[Unit, Unit, Unit](None, None, None)),
            "The query root operation is missing."
          )
        }
      )
    )
}
