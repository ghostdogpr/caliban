package caliban.transformers

import caliban.InputValue
import caliban.execution.Field
import caliban.introspection.adt.{ __Field, __Type }
import caliban.schema.Step.{ ObjectStep, PureStep }
import caliban.schema.{ Step, Types }

sealed trait Transformer[-R] {
  def transformType(t: __Type): __Type
  def transformStep[R1 <: R]: PartialFunction[Step[R1], Step[R1]]
}

object Transformer {
  case class RenameType(f: PartialFunction[String, String])  extends Transformer[Any] {
    def transformType(t: __Type): __Type =
      t.copy(
        name = t.name.map(n => f.lift(n).getOrElse(n)),
        enumValues = args =>
          t.enumValues(args)
            .map(_.map(enumValue => enumValue.copy(name = f.lift(enumValue.name).getOrElse(enumValue.name))))
      )

    def transformStep[R]: PartialFunction[Step[R], Step[R]] = { case step @ ObjectStep(name, fields) =>
      f.lift(name).map(ObjectStep(_, fields)).getOrElse(step)
    }
  }
  case class RenameField(f: PartialFunction[String, String]) extends Transformer[Any] {
    def transformType(t: __Type): __Type =
      t.copy(
        fields =
          args => t.fields(args).map(_.map(field => field.copy(name = f.lift(field.name).getOrElse(field.name)))),
        inputFields = t.inputFields.map(_.map(field => field.copy(name = f.lift(field.name).getOrElse(field.name))))
      )

    def transformStep[R]: PartialFunction[Step[R], Step[R]] = { case ObjectStep(name, fields) =>
      ObjectStep(name, fields.map { case (name, step) => f.lift(name).getOrElse(name) -> step })
    }
  }
  case class FilterField(f: String => Boolean)               extends Transformer[Any] {
    def transformType(t: __Type): __Type =
      t.copy(
        fields = args => t.fields(args).map(_.filter(field => f(field.name))),
        inputFields = t.inputFields.map(_.filter(field => f(field.name)))
      )

    def transformStep[R]: PartialFunction[Step[R], Step[R]] = { case ObjectStep(name, fields) =>
      ObjectStep(name, fields.filter { case (name, _) => f(name) })
    }
  }
  case class Extend[R](
    typeName: String,
    fieldName: String,
    fieldDefinition: __Field,
    extraFields: List[Field],
    resolver: Map[String, InputValue] => Step[R]
  ) extends Transformer[R] {
    def transformType(t: __Type): __Type =
      t.copy(fields =
        args =>
          t.fields(args)
            .map(fields =>
              if (t.name.contains(typeName)) fieldDefinition.copy(name = fieldName, args = Nil) :: fields
              else fields
            )
      )

    def transformStep[R1 <: R]: PartialFunction[Step[R1], Step[R1]] = { case ObjectStep(`typeName`, fields) =>
      val pureFields = fields.collect { case (name, PureStep(value)) => name -> value.toInputValue }
      ObjectStep[R1](typeName, fields + (fieldName -> resolver(pureFields)))
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
