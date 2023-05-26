package caliban.validation

import caliban.CalibanError.ValidationError
import caliban._
import caliban.TestUtils._
import caliban.TestUtils.InvalidSchemas._
import caliban.introspection.adt._
import caliban.schema.Types
import caliban.schema.Schema.auto._
import caliban.schema.ArgBuilder.auto._
import caliban.{ GraphQL, RootResolver }
import zio.{ IO, ZIO }
import zio.test.Assertion._
import zio.test._

object ValidationSchemaSpec extends ZIOSpecDefault {

  def check(gql: GraphQL[Any], expectedMessage: String): IO[ValidationError, TestResult] =
    gql.interpreter.exit.map(assert(_)(fails[ValidationError](hasField("msg", _.msg, equalTo(expectedMessage)))))

  def checkTypeError(t: __Type, expectedMessage: String): IO[ValidationError, TestResult] =
    Validator.validateType(t).toZIO.exit.map(assert(_)(fails(hasField("msg", _.msg, equalTo(expectedMessage)))))

  override def spec =
    suite("ValidationSchemaSpec")(
      suite("Enum")(
        test("non-empty enum is ok") {
          Validator
            .validateEnum(
              Types.makeEnum(
                name = Some("nonEmptyEnum"),
                description = None,
                values = List(__EnumValue("A", None, true, None, None)),
                origin = None
              )
            )
            .toZIO
            .exit
            .map(assert(_)(succeeds(anything)))
        },
        test("must be non-empty") {
          checkTypeError(
            Types.makeEnum(
              name = Some("EmptyEnum"),
              description = None,
              values = Nil,
              origin = None,
              directives = None
            ),
            "Enum EmptyEnum doesn't contain any values"
          )
        }
      ),
      suite("Union")(
        test("union containing object types is ok") {
          Validator
            .validateUnion(
              Types.makeUnion(
                name = Some("GoodUnion"),
                description = None,
                subTypes = List(__Type(kind = __TypeKind.OBJECT))
              )
            )
            .toZIO
            .exit
            .map(assert(_)(succeeds(anything)))
        },
        test("must be non-empty") {
          val expectedMessage = "Union EmptyUnion doesn't contain any type."
          checkTypeError(
            Types.makeUnion(
              name = Some("EmptyUnion"),
              description = None,
              subTypes = Nil
            ),
            expectedMessage
          )
        }
      ),
      suite("Directives")(
        test("name on a type can't start with '__'") {
          check(
            graphQL(resolverWrongDirectiveName),
            "Directive '__name' of Type 'TestWrongDirectiveName' can't start with '__'"
          )
        },
        test("name on a field type can't start with '__'") {
          check(
            graphQL(resolverWrongFieldDirectiveName),
            "Directive '__name' of Field 'field' of Type 'TestWrongFieldDirectiveName' can't start with '__'"
          )
        },
        test("name on a inputValue on a type can't start with '__'") {
          check(
            graphQL(resolverWrongInputFieldDirectiveName),
            "Directive '__name' of InputValue 'inputValue' of Type 'WrongDirectiveNameInput' can't start with '__'"
          )
        },
        test("name on a inputValue on a field type can't start with '__'") {
          check(
            graphQL(resolverWrongFieldArgDirectiveName),
            "Directive '__name' of InputValue 'inputValue' of Field 'field' of Type 'TestWrongFieldArgDirectiveName' can't start with '__'"
          )
        },
        test("argument name can't start with '__'") {
          check(
            graphQL(resolverWrongArgumentDirectiveName),
            "Argument '__name' of Directive 'name' of Field 'field' of Type 'TestWrongArgumentDirectiveName' can't start with '__'"
          )
        }
      ),
      suite("InputObjects")(
        test("must define one or more fields") {
          checkTypeError(
            Types.makeInputObject(
              name = Some("EmptyInputObject"),
              description = None,
              fields = Nil
            ),
            "InputObject 'EmptyInputObject' does not have fields"
          )
        },
        test("no two input fields may share the same name") {
          checkTypeError(
            Types.makeInputObject(
              name = Some("DuplicateNamesInputObject"),
              description = None,
              fields = List.fill(2)(
                __InputValue("A", None, `type` = () => Types.string, None)
              )
            ),
            "InputObject 'DuplicateNamesInputObject' has repeated fields: A"
          )
        },
        test("name can't start with '__'") {
          check(
            graphQL(resolverWrongMutationUnderscore),
            "InputValue '__name' of InputObject 'DoubleUnderscoreArgInput' can't start with '__'"
          )
        },
        test("should only contain types for which IsInputType(type) is true") {
          check(
            graphQL(resolverWrongMutationUnion),
            "UnionInput of InputValue 'union' of InputObject 'UnionArgInput' is of kind UNION, must be an InputType"
          )
        }
      ), {
        import InvalidSchemas.Interface._
        suite("Interface")(
          test("must define one or more fields") {
            check(
              graphQL(resolverEmptyInterface),
              "Interface 'InterfaceEmpty' does not have fields"
            )
          },
          test("field names must be unique") {
            checkTypeError(
              Types.makeInterface(
                name = Some("DuplicateNamesInterface"),
                description = None,
                fields = () =>
                  List(
                    __Field("A", None, List.empty, `type` = () => __Type(__TypeKind.SCALAR)),
                    __Field("A", None, List.empty, `type` = () => __Type(__TypeKind.SCALAR))
                  ),
                subTypes = Nil
              ),
              "Interface 'DuplicateNamesInterface' has repeated fields: A"
            )
          },
          test("field name can't start with '__'") {
            check(
              graphQL(resolverInterfaceWrongFieldName),
              "Field '__name' of Interface 'InterfaceWrongFieldName' can't start with '__'"
            )
          },
          test("field can't be input type") {
            checkTypeError(
              Types.makeInterface(
                name = Some("InputTypeFieldInterface"),
                description = None,
                fields = () =>
                  List(
                    __Field(
                      "InputField",
                      None,
                      List.empty,
                      `type` = () => __Type(name = Some("InputType"), kind = __TypeKind.INPUT_OBJECT)
                    )
                  ),
                subTypes = Nil
              ),
              "InputType of Field 'InputField' of Interface 'InputTypeFieldInterface' is of kind INPUT_OBJECT, must be an OutputType"
            )
          },
          test("field argument name can't start with '__'") {
            check(
              graphQL(resolverInterfaceWrongArgumentName),
              "InputValue '__name' of Field 'x' of Interface 'InterfaceWrongArgumentName' can't start with '__'"
            )
          },
          test("field argument can't be output type") {
            check(
              graphQL(resolverInterfaceWrongArgumentInputType),
              "UnionInput of InputValue 'union' of InputObject 'UnionArgInput' is of kind UNION, must be an InputType"
            )
          },
          test("clashing input and object types") {
            check(
              graphQL(resolverClashingObjects),
              "Type 'ClashingObjectInput' is defined multiple times (INPUT_OBJECT in caliban.TestUtils.InvalidSchemas.Interface.ClashingObject, OBJECT in caliban.TestUtils.InvalidSchemas.Interface.ClashingObjectInput)."
            )
          },
          test("clashing names from different packages") {
            check(
              graphQL(resolverClashingNames),
              "Type 'C' is defined multiple times (OBJECT in caliban.TestUtils.InvalidSchemas.Interface.A.C, OBJECT in caliban.TestUtils.InvalidSchemas.Interface.B.C)."
            )
          },
          test("missing root query") {
            check(
              graphQL(RootResolver[Unit, Unit, Unit](None, None, None)),
              "The query root operation is missing."
            )
          }
        )
      }, {
        import Object._
        suite("Object")(
          test("must define one or more fields") {
            check(
              graphQL(resolverEmpty),
              "Object 'EmptyObject' does not have fields"
            )
          },
          test("field name can't start with '__'") {
            check(
              graphQL(resolverWrongFieldName),
              "Field '__name' of Object 'ObjectWrongFieldName' can't start with '__'"
            )
          },
          test("field argument name can't start with '__'") {
            check(
              graphQL(resolverWrongArgumentName),
              "InputValue '__name' of Field 'x' of Object 'ObjectWrongArgumentName' can't start with '__'"
            )
          },
          test("field argument can't be output type") {
            check(
              graphQL(resolverWrongArgumentInputType),
              "UnionInput of InputValue 'union' of InputObject 'UnionArgInput' is of kind UNION, must be an InputType"
            )
          },
          test("may declare that it implements one or more unique interfaces") {
            graphQL(resolverTwoInterfaces).interpreter.exit.map(assert(_)(succeeds(anything)))
          },
          test("must include a field of the same name for every field defined in an interface") {
            ZIO
              .collectAll(
                List(
                  checkTypeError(objectWithFields("a"), "Object 'FieldsA' is missing field(s): b"),
                  checkTypeError(objectWithFields("b"), "Object 'FieldsB' is missing field(s): a"),
                  checkTypeError(objectWithFields("c"), "Object 'FieldsC' is missing field(s): a, b"),
                  checkTypeError(objectWithFields("a", "c"), "Object 'FieldsAC' is missing field(s): b")
                )
              )
              .map(_.reduce(_ && _))
          },
          test("field type when equal to interface field type is a valid sub-type") {
            graphQL(resolverTwoInterfaces).interpreter.exit.map(assert(_)(succeeds(anything)))
          },
          test("field type in the possible types of an interface or union is a valid sub-type") {
            for {
              a <- graphQL(resolverUnionSubtype).interpreter.exit.map(assert(_)(succeeds(anything)))
              b <- graphQL(resolverFieldObject).interpreter.exit.map(assert(_)(succeeds(anything)))
            } yield a && b
          },
          test("field type with the same name but not equal to or a subtype of an interface field is invalid") {
            checkTypeError(
              objectWrongInterfaceFieldType,
              "Field 'a' in Object 'ObjectWrongInterfaceFieldType' is an invalid subtype"
            )
          },
          test("field type that is a valid list item subtype is valid") {
            for {
              a <- graphQL(resolverListUnionSubtype).interpreter.exit.map(assert(_)(succeeds(anything)))
              b <- graphQL(resolverListInterfaceSubtype).interpreter.exit.map(assert(_)(succeeds(anything)))
            } yield a && b
          },
          test("field type that is an invalid list item subtype is invalid") {
            checkTypeError(
              objectWrongListItemSubtype,
              "Field 'a' in Object 'ObjectWrongListItemSubtype' is an invalid list item subtype"
            )
          },
          test("field type that is a Non-Null variant of a valid interface field type is valid") {
            graphQL(resolverNonNullableSubtype).interpreter.exit.map(assert(_)(succeeds(anything)))
          },
          test("fields including arguments of the same name and type defined in an interface are valid") {
            graphQL(resolverFieldWithArg).interpreter.exit.map(assert(_)(succeeds(anything)))
          },
          test("fields with additional nullable args are valid") {
            Validator.validateObject(nullableExtraArgsObject).toZIO.exit.map(assert(_)(succeeds(anything)))
          },
          test("fields with additional non-nullable args are invalid") {
            checkTypeError(
              nonNullableExtraArgsObject,
              "Field 'fieldWithArg' with extra non-nullable arg(s) 'extraArg'" +
                " in Object 'NonNullableExtraArgsObject' is invalid"
            )
          },
          test("wrong type name is invalid") {
            check(
              graphQL(resolverBadTypeName),
              "Type 'Foo { str: String }\ntype Bar' is not a valid name."
            )
          },
          test("wrong field name is invalid") {
            check(
              graphQL(resolverBadFieldName),
              "Field 'bool: Boolean\n  num' of Object 'WrongFieldName' is not a valid name."
            )
          },
          test("query root type not an object") {
            check(
              graphQL(resolverQueryNoObject),
              "The query root operation is not an object type."
            )
          }
        )
      }
    )
}
