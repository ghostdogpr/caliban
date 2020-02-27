package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.GraphQL
import caliban.GraphQL.graphQL
import caliban.TestUtils.InvalidSchemas._
import caliban.introspection.adt.__TypeKind.{ INPUT_OBJECT, SCALAR }
import caliban.introspection.adt.{ __InputValue, __Type }
import caliban.schema.RootType
import zio.IO
import zio.test.Assertion._
import zio.test._

object ValidationSchemaSpec
    extends DefaultRunnableSpec({
      def check(gql: GraphQL[Any], expectedMessage: String): IO[ValidationError, TestResult] =
        assertM(gql.interpreter.run, fails[ValidationError](hasField("msg", _.msg, equalTo(expectedMessage))))

      suite("ValidationSchemaSpec")({
        def checkInputObject(
          __type: __Type,
          expectedMessage: String
        ) = {
          val validate = Validator.validateSchema(RootType(__Type(kind = SCALAR), Some(__type), None))
          assertM(validate.run, fails[ValidationError](hasField("msg", _.msg, equalTo(expectedMessage))))
        }

        def inputValue(name: String, t: __Type): __InputValue =
          __InputValue(name = name, `type` = () => t, description = None, defaultValue = None)

        def inputObject(inputs: __InputValue*): __Type =
          __Type(
            name = Some("inputObject"),
            kind = INPUT_OBJECT,
            inputFields = Some(inputs.toList)
          )

        suite("InputObjects")(
          testM("any input field must have a unique name within an Input Object") {
            checkInputObject(
              inputObject(inputValue("duplicate", __Type(SCALAR)), inputValue("duplicate", __Type(SCALAR))),
              "InputObject has repeated fields: duplicate"
            )
          },
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
