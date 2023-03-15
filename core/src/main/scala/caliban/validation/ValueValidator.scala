package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.InputValue._
import caliban.Value._
import caliban.introspection.adt._
import caliban.introspection.adt.__TypeKind._
import caliban.parsing.Parser
import caliban.{ InputValue, Value }
import zio.prelude.EReader
import zio.prelude.fx.ZPure
import zio.prelude._

object ValueValidator {
  def validateDefaultValue(field: __InputValue, errorContext: String): EReader[Any, ValidationError, Unit] =
    field.defaultValue match {
      case Some(v) =>
        for {
          value <- ZPure
                     .fromEither(Parser.parseInputValue(v))
                     .mapError(e =>
                       ValidationError(
                         s"$errorContext failed to parse default value: ${e.msg}",
                         "The default value for a field must be written using GraphQL input syntax."
                       )
                     )
          _     <- Validator.validateInputValues(field, value, Context.empty, errorContext)
        } yield ()
      case None    =>
        ZPure.unit
    }

  def validateInputTypes(
    inputValue: __InputValue,
    argValue: InputValue,
    context: Context,
    errorContext: String
  ): EReader[Any, ValidationError, Unit] = validateType(inputValue.`type`(), argValue, context, errorContext)

  def validateType(
    inputType: __Type,
    argValue: InputValue,
    context: Context,
    errorContext: String
  ): EReader[Any, ValidationError, Unit] =
    argValue match {
      case v: VariableValue =>
        val value =
          context.variables
            .getOrElse(v.name, context.variableDefinitions.get(v.name).flatMap(_.defaultValue).getOrElse(NullValue))

        validateType(inputType, value, context, errorContext)
      case _                =>
        inputType.kind match {
          case NON_NULL =>
            argValue match {
              case NullValue =>
                failValidation(s"$errorContext is null", "Input field was null but was supposed to be non-null.")
              case x         => validateType(inputType.ofType.getOrElse(inputType), x, context, errorContext)
            }
          case LIST     =>
            argValue match {
              case ListValue(values) =>
                values.forEach_(v =>
                  validateType(inputType.ofType.getOrElse(inputType), v, context, s"List item in $errorContext")
                )
              case NullValue         =>
                ZPure.unit
              case other             =>
                // handle item as the first item in the list
                validateType(inputType.ofType.getOrElse(inputType), other, context, s"List item in $errorContext")
            }

          case INPUT_OBJECT =>
            argValue match {
              case ObjectValue(fields) =>
                inputType.inputFields.getOrElse(List.empty).forEach_ { f =>
                  fields.collectFirst { case (name, fieldValue) if name == f.name => fieldValue } match {
                    case Some(value) =>
                      validateType(f.`type`(), value, context, s"Field ${f.name} in $errorContext")
                    case None        =>
                      ZPure.when(f.defaultValue.isEmpty) {
                        validateType(f.`type`(), NullValue, context, s"Field ${f.name} in $errorContext")
                      }
                  }
                }
              case NullValue           =>
                ZPure.unit
              case _                   =>
                failValidation(
                  s"$errorContext has invalid type: $argValue",
                  "Input field was supposed to be an input object."
                )
            }
          case ENUM         =>
            argValue match {
              case EnumValue(value) =>
                validateEnum(value, inputType, errorContext)
              case NullValue        =>
                ZPure.unit
              case _                =>
                failValidation(
                  s"$errorContext has invalid type: $argValue",
                  "Input field was supposed to be an enum value."
                )
            }
          case SCALAR       => validateScalar(inputType, argValue, errorContext)
          case _            =>
            failValidation(
              s"$errorContext has invalid type $inputType",
              "Input value is invalid, should be a scalar, list or input object."
            )
        }
    }

  def validateEnum(value: String, inputType: __Type, errorContext: String): EReader[Any, ValidationError, Unit] = {
    val possible = inputType
      .enumValues(__DeprecatedArgs(Some(true)))
      .getOrElse(List.empty)
      .map(_.name)
    val exists   = possible.contains(value)

    ZPure
      .unless(exists)(
        failValidation(
          s"$errorContext has invalid enum value: $value",
          s"Was supposed to be one of ${possible.mkString(", ")}"
        )
      )
      .unit
  }

  def validateScalar(
    inputType: __Type,
    argValue: InputValue,
    errorContext: String
  ): EReader[Any, ValidationError, Unit] =
    inputType.name.getOrElse("") match {
      case "String"  =>
        argValue match {
          case _: StringValue | NullValue => ZPure.unit
          case t                          => failValidation(s"$errorContext has invalid type $t", "Expected 'String'")
        }
      case "ID"      =>
        argValue match {
          case _: StringValue | NullValue => ZPure.unit
          case t                          => failValidation(s"$errorContext has invalid type $t", "Expected 'ID'")
        }
      case "Int"     =>
        argValue match {
          case _: Value.IntValue | NullValue => ZPure.unit
          case t                             => failValidation(s"$errorContext has invalid type $t", "Expected 'Int'")
        }
      case "Float"   =>
        argValue match {
          case _: Value.FloatValue | _: Value.IntValue | NullValue => ZPure.unit
          case t                                                   => failValidation(s"$errorContext has invalid type $t", "Expected 'Float'")
        }
      case "Boolean" =>
        argValue match {
          case _: BooleanValue | NullValue => ZPure.unit
          case t                           => failValidation(s"$errorContext has invalid type $t", "Expected 'Boolean'")
        }
      // We can't really validate custom scalars here (since we can't summon a correct ArgBuilder instance), so just pass them along
      case _         => ZPure.unit
    }

  def failValidation[T](msg: String, explanatoryText: String): EReader[Any, ValidationError, T] =
    ZPure.fail(ValidationError(msg, explanatoryText))

}
