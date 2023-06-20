package caliban.transformers

import caliban.InputValue
import caliban.execution.Field
import caliban.introspection.adt.{ __Field, __Type }
import caliban.schema.Step.{ ObjectStep, PureStep }
import caliban.schema.{ Step, Types }

sealed trait Transformer[-R]

object Transformer {
  case class RenameType(f: PartialFunction[String, String])  extends Transformer[Any]
  case class RenameField(f: PartialFunction[String, String]) extends Transformer[Any]
  case class FilterField(f: String => Boolean)               extends Transformer[Any]
  case class Extend[R](
    typeName: String,
    fieldName: String,
    fieldDefinition: __Field,
    extraFields: List[Field],
    resolver: Map[String, InputValue] => Step[R]
  ) extends Transformer[R]

  def transformType(t: __Type, transformer: Transformer[Nothing]): __Type =
    transformer match {
      case RenameType(f)                                  =>
        t.copy(
          name = t.name.map(n => f.lift(n).getOrElse(n)),
          fields = args =>
            t.fields(args).map(_.map(field => field.copy(`type` = () => transformType(field.`type`(), transformer)))),
          inputFields =
            t.inputFields.map(_.map(field => field.copy(`type` = () => transformType(field.`type`(), transformer)))),
          enumValues = args =>
            t.enumValues(args)
              .map(_.map(enumValue => enumValue.copy(name = f.lift(enumValue.name).getOrElse(enumValue.name)))),
          interfaces = () => t.interfaces().map(_.map(transformType(_, transformer))),
          possibleTypes = t.possibleTypes.map(_.map(transformType(_, transformer))),
          ofType = t.ofType.map(transformType(_, transformer))
        )
      case RenameField(f)                                 =>
        t.copy(
          fields = args =>
            t.fields(args)
              .map(
                _.map(field =>
                  field.copy(
                    name = f.lift(field.name).getOrElse(field.name),
                    `type` = () => transformType(field.`type`(), transformer)
                  )
                )
              ),
          inputFields = t.inputFields.map(
            _.map(field =>
              field.copy(
                name = f.lift(field.name).getOrElse(field.name),
                `type` = () => transformType(field.`type`(), transformer)
              )
            )
          ),
          interfaces = () => t.interfaces().map(_.map(transformType(_, transformer))),
          possibleTypes = t.possibleTypes.map(_.map(transformType(_, transformer))),
          ofType = t.ofType.map(transformType(_, transformer))
        )
      case FilterField(f)                                 =>
        t.copy(
          fields = args =>
            t.fields(args)
              .map(
                _.filter(field => f(field.name))
                  .map(field => field.copy(`type` = () => transformType(field.`type`(), transformer)))
              ),
          inputFields = t.inputFields.map(
            _.filter(field => f(field.name)).map(field =>
              field.copy(`type` = () => transformType(field.`type`(), transformer))
            )
          ),
          interfaces = () => t.interfaces().map(_.map(transformType(_, transformer))),
          possibleTypes = t.possibleTypes.map(_.map(transformType(_, transformer))),
          ofType = t.ofType.map(transformType(_, transformer))
        )
      case Extend(typeName, fieldName, sourceField, _, _) =>
        t.copy(
          fields = args =>
            t.fields(args)
              .map(fields =>
                if (t.name.contains(typeName)) sourceField.copy(name = fieldName, args = Nil) :: fields
                else fields.map(field => field.copy(`type` = () => transformType(field.`type`(), transformer)))
              ),
          interfaces = () => t.interfaces().map(_.map(transformType(_, transformer))),
          possibleTypes = t.possibleTypes.map(_.map(transformType(_, transformer))),
          ofType = t.ofType.map(transformType(_, transformer))
        )
    }

  def transformStep[R](step: Step[R], transformer: Transformer[R]): Step[R] =
    transformer match {
      case RenameType(f)                               =>
        step match {
          case ObjectStep(name, fields) => f.lift(name).map(ObjectStep(_, fields)).getOrElse(step)
          case other                    => other
        }
      case FilterField(f)                              =>
        step match {
          case ObjectStep(name, fields) => ObjectStep(name, fields.filter { case (name, _) => f(name) })
          case other                    => other
        }
      case RenameField(f)                              =>
        step match {
          case ObjectStep(name, fields) =>
            ObjectStep(name, fields.map { case (name, step) => f.lift(name).getOrElse(name) -> step })
          case other                    => other
        }
      case Extend(typeName, fieldName, _, _, resolver) =>
        step match {
          case ObjectStep(`typeName`, fields) =>
            val pureFields = fields.collect { case (name, PureStep(value)) => name -> value.toInputValue }
            ObjectStep[R](typeName, fields + (fieldName -> resolver(pureFields)))
          case other                          => other
        }
    }

  def patchField[R](field: Field, transformer: Transformer[R]): Field =
    transformer match {
      case Extend(typeName, fieldName, _, extraFields, _) =>
        if (Types.innerType(field.fieldType).name.contains(typeName) && field.fields.exists(_.name == fieldName))
          field.copy(fields =
            field.fields.filterNot(_.name == fieldName) ++
              extraFields.filterNot(extraField => field.fields.exists(_.name == extraField.name))
          )
        else field.copy(fields = field.fields.map(patchField(_, transformer)))
      case _                                              => field
    }
}
