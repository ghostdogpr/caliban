package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.InputValue._
import caliban.Value._
import caliban.introspection.adt._
import caliban.introspection.adt.__TypeKind._
import caliban.parsing.Parser
import caliban.{ InputValue, Value }

private object ValueValidator {
  import ValidationOps._

  def validateDefaultValue(field: __InputValue, errorContext: => String): Either[ValidationError, Unit] =
    field.defaultValue match {
      case Some(v) =>
        for {
          value <- Parser
                     .parseInputValue(v)
                     .left
                     .map(e =>
                       ValidationError(
                         s"$errorContext failed to parse default value: ${e.msg}",
                         "The default value for a field must be written using GraphQL input syntax."
                       )
                     )
          _     <- Validator.validateInputValues(field, value, Context.empty, errorContext)
        } yield ()
      case None    =>
        when(field.isDeprecated && !field._type.isNullable) {
          failValidation(
            s"$errorContext has no default value, is non-null and deprecated.",
            "If input field type is Non-Null and a default value is not defined, the `@deprecated` directive must not be applied to this input field."
          )
        }
    }

  def validateInputTypes(
    inputValue: __InputValue,
    argValue: InputValue,
    context: Context,
    errorContext: => String
  ): Either[ValidationError, Unit] = validateType(inputValue._type, argValue, context, errorContext)

  def validateType(
    inputType: __Type,
    argValue: InputValue,
    context: Context,
    errorContext: => String
  ): Either[ValidationError, Unit] =
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
                validateAllDiscard(values)(v =>
                  validateType(inputType.ofType.getOrElse(inputType), v, context, s"List item in $errorContext")
                )
              case NullValue         =>
                unitR
              case other             =>
                // handle item as the first item in the list
                validateType(inputType.ofType.getOrElse(inputType), other, context, s"List item in $errorContext")
            }

          case INPUT_OBJECT =>
            argValue match {
              case ObjectValue(fields) =>
                validateAllDiscard(inputType.allInputFields) { f =>
                  fields.collectFirst { case (name, fieldValue) if name == f.name => fieldValue } match {
                    case Some(value)                    =>
                      validateType(f._type, value, context, s"Field ${f.name} in $errorContext")
                    case None if f.defaultValue.isEmpty =>
                      validateType(f._type, NullValue, context, s"Field ${f.name} in $errorContext")
                    case _                              =>
                      unitR
                  }
                }
              case NullValue           =>
                unitR
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
                unitR
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

  def validateEnum(value: String, inputType: __Type, errorContext: => String): Either[ValidationError, Unit] = {
    val possible = inputType
      .enumValues(__DeprecatedArgs.include)
      .getOrElse(List.empty)
      .map(_.name)
    val exists   = possible.contains(value)

    when(!exists)(
      failValidation(
        s"$errorContext has invalid enum value: $value",
        s"Was supposed to be one of ${possible.mkString(", ")}"
      )
    )
  }

  def validateScalar(
    inputType: __Type,
    argValue: InputValue,
    errorContext: => String
  ): Either[ValidationError, Unit] =
    inputType.name.getOrElse("") match {
      case "String"  =>
        argValue match {
          case _: StringValue | NullValue => unitR
          case t                          => failValidation(s"$errorContext has invalid type $t", "Expected 'String'")
        }
      case "ID"      =>
        argValue match {
          case _: StringValue | NullValue => unitR
          case t                          => failValidation(s"$errorContext has invalid type $t", "Expected 'ID'")
        }
      case "Int"     =>
        argValue match {
          case _: Value.IntValue | NullValue => unitR
          case t                             => failValidation(s"$errorContext has invalid type $t", "Expected 'Int'")
        }
      case "Float"   =>
        argValue match {
          case _: Value.FloatValue | _: Value.IntValue | NullValue => unitR
          case t                                                   => failValidation(s"$errorContext has invalid type $t", "Expected 'Float'")
        }
      case "Boolean" =>
        argValue match {
          case _: BooleanValue | NullValue => unitR
          case t                           => failValidation(s"$errorContext has invalid type $t", "Expected 'Boolean'")
        }
      // We can't really validate custom scalars here (since we can't summon a correct ArgBuilder instance), so just pass them along
      case _         => unitR
    }

  def failValidation[T](msg: String, explanatoryText: String): Either[ValidationError, T] =
    Left(ValidationError(msg, explanatoryText))

}
