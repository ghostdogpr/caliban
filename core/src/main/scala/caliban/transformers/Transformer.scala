package caliban.transformers

import caliban.InputValue
import caliban.execution.Field
import caliban.introspection.adt._
import caliban.schema.Step.{ ObjectStep, PureStep }
import caliban.schema.{ Step, Types }

sealed trait Transformer[-R] {
  val typeVisitor: TypeVisitor

  def transformStep[R1 <: R](args: Map[String, InputValue]): PartialFunction[Step[R1], Step[R1]]
}

object Transformer {
  case class RenameType(f: PartialFunction[String, String])            extends Transformer[Any] {
    private def rename(name: String): String = f.lift(name).getOrElse(name)

    val typeVisitor: TypeVisitor =
      TypeVisitor.modify(t => t.copy(name = t.name.map(rename))) |+|
        TypeVisitor.enumValues.modify(v => v.copy(name = rename(v.name)))

    def transformStep[R](args: Map[String, InputValue]): PartialFunction[Step[R], Step[R]] = {
      case step @ ObjectStep(name, fields) =>
        f.lift(name).map(ObjectStep(_, fields)).getOrElse(step)
    }
  }
  case class RenameField(f: PartialFunction[(String, String), String]) extends Transformer[Any] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith((t, field) =>
        field.copy(name = f.lift((t.name.getOrElse(""), field.name)).getOrElse(field.name))
      ) |+|
        TypeVisitor.inputFields.modifyWith((t, field) =>
          field.copy(name = f.lift((t.name.getOrElse(""), field.name)).getOrElse(field.name))
        )

    def transformStep[R](args: Map[String, InputValue]): PartialFunction[Step[R], Step[R]] = {
      case ObjectStep(typeName, fields) =>
        ObjectStep(
          typeName,
          fields.map { case (fieldName, step) => f.lift((typeName, fieldName)).getOrElse(fieldName) -> step }
        )
    }
  }
  case class FilterField(f: (String, String) => Boolean)               extends Transformer[Any] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.filterWith((t, field) => f(t.name.getOrElse(""), field.name)) |+|
        TypeVisitor.inputFields.filterWith((t, field) => f(t.name.getOrElse(""), field.name))

    def transformStep[R](args: Map[String, InputValue]): PartialFunction[Step[R], Step[R]] = {
      case ObjectStep(typeName, fields) =>
        ObjectStep(typeName, fields.filter { case (fieldName, _) => f(typeName, fieldName) })
    }
  }
  case class Extend[R](
    typeName: String,
    fieldName: String,
    fieldDefinition: __Field,
    extraFields: List[Field],
    resolver: (Map[String, InputValue], Map[String, InputValue]) => Step[R]
  ) extends Transformer[R] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.addWith(t =>
        if (t.name.contains(typeName)) List(fieldDefinition.copy(name = fieldName, args = Nil)) else Nil
      )

    def transformStep[R1 <: R](args: Map[String, InputValue]): PartialFunction[Step[R1], Step[R1]] = {
      case ObjectStep(`typeName`, fields) =>
        val pureFields = fields.collect { case (name, PureStep(value)) => name -> value.toInputValue }
        ObjectStep[R1](typeName, fields + (fieldName -> resolver(pureFields, args)))
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
