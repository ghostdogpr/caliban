package caliban.validation

import caliban.CalibanError.ValidationError
import caliban.InputValue.VariableValue
import caliban.Value.NullValue
import caliban.execution.{ ExecutionRequest, Field => F }
import caliban.introspection.Introspector
import caliban.introspection.adt._
import caliban.introspection.adt.__TypeKind._
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.Definition.{ TypeSystemDefinition, TypeSystemExtension }
import caliban.parsing.adt.OperationType._
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt._
import caliban.schema.{ RootSchema, RootSchemaBuilder, RootType, Types }
import caliban.{ InputValue, Rendering, Value }
import caliban.parsing.Parser
import zio.IO
import caliban.schema.ArgBuilder
import caliban.InputValue.ListValue
import caliban.Value.EnumValue
import caliban.Value.StringValue
import caliban.Value.FloatValue.BigDecimalNumber
import caliban.Value.IntValue.BigIntNumber
import caliban.Value.BooleanValue
import caliban.Value.IntValue.IntNumber
import caliban.Value.IntValue.LongNumber
import caliban.Value.FloatValue.DoubleNumber
import caliban.Value.FloatValue.FloatNumber
import caliban.InputValue.ObjectValue

object DefaultValue {
  def validateDefaultValue(field: __InputValue): IO[ValidationError, Unit] =
    field.defaultValue.map { v =>
      for {
        value <-
          IO.fromEither(Parser.parseInputValue(v))
            .mapError(e =>
              ValidationError(
                s"Failed to parse default value for input '${field.name}': ${e.msg}",
                "The default value for a field must be written using GraphQL input syntax."
              )
            )
        _     <- Validator.validateInputValues(field, value)
        _     <- validateInputTypes(field, value)
      } yield ()
    }.getOrElse(IO.unit)

  def validateInputTypes(
    inputValue: __InputValue,
    argValue: InputValue
  ): IO[ValidationError, Unit] = validateType(inputValue.`type`(), argValue)

  def validateType(inputType: __Type, argValue: InputValue): IO[ValidationError, Unit] =
    inputType.kind match {
      case NON_NULL =>
        argValue match {
          case NullValue => failValidation(s"Field is null but was supposed to be NonNull", "")
          case x         => validateType(inputType.ofType.getOrElse(inputType), x)
        }
      case LIST     =>
        argValue match {
          case ListValue(values) => IO.foreach_(values)(v => validateType(inputType.ofType.getOrElse(inputType), v))
          case _                 => failValidation(s"Field has invalid type: $argValue, expected List", "")
        }

      case INPUT_OBJECT =>
        argValue match {
          case ObjectValue(fields) =>
            IO.foreach_(inputType.inputFields.getOrElse(List.empty)) { f =>
              val value =
                fields.collectFirst({ case (name, fieldValue) if name == f.name => fieldValue }).getOrElse(NullValue)
              validateType(f.`type`(), value)
            }
          case _                   => failValidation(s"Field has invalid type: $argValue, expected INPUT_OBJECT", "")
        }
      case ENUM         =>
        argValue match {
          case EnumValue(value) =>
            val exists = inputType
              .enumValues(__DeprecatedArgs(Some(true)))
              .getOrElse(List.empty)
              .exists(_.name == value)

            if (exists) IO.unit
            else failValidation(s"Field has invalid enum value: $value", "")
          case _                => failValidation(s"Field has invalid type: $argValue", "")
        }
      case SCALAR       => validateScalar(inputType, argValue)
      case x            =>
        failValidation(s"Field has invalid type $inputType", "")
    }

  def validateScalar(inputType: __Type, argValue: InputValue) =
    inputType.name.getOrElse("") match {
      case "String"  =>
        argValue match {
          case StringValue(value) =>
            IO.unit
          case t                  => failValidation(s"Field has invalid type $t, expected 'String'", "")
        }
      case "ID"      =>
        argValue match {
          case StringValue(value) =>
            IO.unit
          case t                  => failValidation(s"Field has invalid type $t, expected 'ID'", "")
        }
      case "Int"     =>
        argValue match {
          case _: Value.IntValue => IO.unit
          case t                 => failValidation(s"Field has invalid type $t, expected 'Int'", "")
        }
      case "Float"   =>
        argValue match {
          case _: Value.FloatValue => IO.unit
          case t                   => failValidation(s"Field has invalid type $t, expected 'Float'", "")
        }
      case "Boolean" =>
        argValue match {
          case BooleanValue(value) => IO.unit
          case t                   => failValidation(s"Field has invalid type $t, expected 'Boolean'", "")
        }
      // We can't really validate custom scalars here (since we can't summon a correct ArgBuilder instance), so just pass them along
      case x         => IO.unit
    }

  def failValidation[T](msg: String, explanatoryText: String): IO[ValidationError, T] =
    IO.fail(ValidationError(msg, explanatoryText))
}
