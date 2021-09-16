package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.InputValue
import caliban.InputValue._
import caliban.Value
import caliban.Value._
import caliban.introspection.adt._
import caliban.introspection.adt.__TypeKind._
import caliban.parsing.Parser
import zio.IO

object DefaultValueValidator {
  def validateDefaultValue(field: __InputValue, errorContext: String): IO[ValidationError, Unit] =
    IO.whenCase(field.defaultValue) { case Some(v) =>
      for {
        value <-
          IO.fromEither(Parser.parseInputValue(v))
            .mapError(e =>
              ValidationError(
                s"$errorContext failed to parse default value: ${e.msg}",
                "The default value for a field must be written using GraphQL input syntax."
              )
            )
        _     <- Validator.validateInputValues(field, value)
        _     <- validateInputTypes(field, value, errorContext)
      } yield ()
    }

  def validateInputTypes(
    inputValue: __InputValue,
    argValue: InputValue,
    errorContext: String
  ): IO[ValidationError, Unit] = validateType(inputValue.`type`(), argValue, errorContext)

  def validateType(inputType: __Type, argValue: InputValue, errorContext: String): IO[ValidationError, Unit] =
    inputType.kind match {
      case NON_NULL =>
        argValue match {
          case NullValue =>
            failValidation(s"$errorContext is null", "Input field was null but was supposed to be non-null.")
          case x         => validateType(inputType.ofType.getOrElse(inputType), x, errorContext)
        }
      case LIST     =>
        argValue match {
          case ListValue(values) =>
            IO.foreach_(values)(v =>
              validateType(inputType.ofType.getOrElse(inputType), v, s"List item in $errorContext")
            )
          case _                 =>
            failValidation(s"$errorContext has invalid type: $argValue", "Input field was supposed to be a list.")
        }

      case INPUT_OBJECT =>
        argValue match {
          case ObjectValue(fields) =>
            IO.foreach_(inputType.inputFields.getOrElse(List.empty)) { f =>
              val value =
                fields.collectFirst({ case (name, fieldValue) if name == f.name => fieldValue }).getOrElse(NullValue)
              validateType(f.`type`(), value, s"Field ${f.name} in $errorContext")
            }
          case _                   =>
            failValidation(
              s"$errorContext has invalid type: $argValue",
              "Input field was supposed to be an input object."
            )
        }
      case ENUM         =>
        argValue match {
          case EnumValue(value)   =>
            validateEnum(value, inputType, errorContext)
          case StringValue(value) =>
            validateEnum(value, inputType, errorContext)
          case _                  =>
            failValidation(
              s"$errorContext has invalid type: $argValue",
              "Input field was supposed to be an enum value."
            )
        }
      case SCALAR       => validateScalar(inputType, argValue, errorContext)
      case x            =>
        failValidation(
          s"$errorContext has invalid type $inputType",
          "Input value is invalid, should be a scalar, list or input object."
        )
    }

  def validateEnum(value: String, inputType: __Type, errorContext: String) = {
    val possible = inputType
      .enumValues(__DeprecatedArgs(Some(true)))
      .getOrElse(List.empty)
      .map(_.name)
    val exists   = possible.exists(_ == value)

    IO.unless(exists)(
      failValidation(
        s"$errorContext has invalid enum value: $value",
        s"Was supposed to be one of ${possible.mkString(", ")}"
      )
    )
  }

  def validateScalar(inputType: __Type, argValue: InputValue, errorContext: String) =
    inputType.name.getOrElse("") match {
      case "String"  =>
        argValue match {
          case StringValue(value) =>
            IO.unit
          case NullValue          => IO.unit
          case t                  => failValidation(s"$errorContext has invalid type $t", "Expected 'String'")
        }
      case "ID"      =>
        argValue match {
          case StringValue(value) =>
            IO.unit
          case NullValue          => IO.unit
          case t                  => failValidation(s"$errorContext has invalid type $t", "Expected 'ID'")
        }
      case "Int"     =>
        argValue match {
          case _: Value.IntValue => IO.unit
          case NullValue         => IO.unit
          case t                 => failValidation(s"$errorContext has invalid type $t", "Expected 'Int'")
        }
      case "Float"   =>
        argValue match {
          case _: Value.FloatValue => IO.unit
          case NullValue           => IO.unit
          case t                   => failValidation(s"$errorContext has invalid type $t", "Expected 'Float'")
        }
      case "Boolean" =>
        argValue match {
          case BooleanValue(value) => IO.unit
          case NullValue           => IO.unit
          case t                   => failValidation(s"$errorContext has invalid type $t", "Expected 'Boolean'")
        }
      // We can't really validate custom scalars here (since we can't summon a correct ArgBuilder instance), so just pass them along
      case x         => IO.unit
    }

  def failValidation[T](msg: String, explanatoryText: String): IO[ValidationError, T] =
    IO.fail(ValidationError(msg, explanatoryText))
}
