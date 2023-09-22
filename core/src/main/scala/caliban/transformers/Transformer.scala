package caliban.transformers

import caliban.InputValue
import caliban.execution.Field
import caliban.introspection.adt._
import caliban.schema.Step.{ FunctionStep, MetadataFunctionStep, ObjectStep, PureStep }
import caliban.schema.{ Step, Types }

sealed trait Transformer[-R] {
  val typeVisitor: TypeVisitor

  def transformStep[R1 <: R]: PartialFunction[Step[R1], Step[R1]]
}

object Transformer {
  case class RenameType(f: PartialFunction[String, String])                 extends Transformer[Any] {
    private def rename(name: String): String = f.lift(name).getOrElse(name)

    val typeVisitor: TypeVisitor =
      TypeVisitor.modify(t => t.copy(name = t.name.map(rename))) |+|
        TypeVisitor.enumValues.modify(v => v.copy(name = rename(v.name)))

    def transformStep[R]: PartialFunction[Step[R], Step[R]] = { case step @ ObjectStep(name, fields) =>
      f.lift(name).map(ObjectStep(_, fields)).getOrElse(step)
    }
  }
  case class RenameField(f: PartialFunction[(String, String), String])      extends Transformer[Any] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith((t, field) =>
        field.copy(name = f.lift((t.name.getOrElse(""), field.name)).getOrElse(field.name))
      ) |+|
        TypeVisitor.inputFields.modifyWith((t, field) =>
          field.copy(name = f.lift((t.name.getOrElse(""), field.name)).getOrElse(field.name))
        )

    def transformStep[R]: PartialFunction[Step[R], Step[R]] = { case ObjectStep(typeName, fields) =>
      ObjectStep(
        typeName,
        fields.map { case (fieldName, step) => f.lift((typeName, fieldName)).getOrElse(fieldName) -> step }
      )
    }
  }
  case class RenameArgument(
    f: PartialFunction[(String, String), (PartialFunction[String, String], PartialFunction[String, String])]
  ) extends Transformer[Any] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith((t, field) =>
        f.lift((t.name.getOrElse(""), field.name)) match {
          case Some((rename, _)) =>
            field.copy(args =
              field.args.map(arg => rename.lift(arg.name).fold(arg)(newName => arg.copy(name = newName)))
            )
          case None              => field
        }
      )

    def transformStep[R]: PartialFunction[Step[R], Step[R]] = { case ObjectStep(typeName, fields) =>
      ObjectStep(
        typeName,
        fields.map { case (fieldName, step) =>
          fieldName -> (f.lift((typeName, fieldName)) match {
            case Some((_, rename)) =>
              step match {
                case FunctionStep(mapToStep) =>
                  FunctionStep(args => mapToStep(args.map { case (k, v) => rename.lift(k).getOrElse(k) -> v }))
                case MetadataFunctionStep(f) =>
                  MetadataFunctionStep(f(_) match {
                    case FunctionStep(mapToStep) =>
                      FunctionStep(args => mapToStep(args.map { case (k, v) => rename.lift(k).getOrElse(k) -> v }))
                    case other                   => other
                  })
                case other                   => other
              }
            case None              => step
          })
        }
      )
    }
  }
  case class FilterInterface(f: PartialFunction[(String, String), Boolean]) extends Transformer[Any] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.modify(t =>
        t.copy(interfaces =
          () =>
            t.interfaces()
              .map(_.filter(interface => f.lift((t.name.getOrElse(""), interface.name.getOrElse(""))).getOrElse(true)))
        )
      )

    def transformStep[R]: PartialFunction[Step[R], Step[R]] = { case step => step }
  }
  case class FilterField(f: PartialFunction[(String, String), Boolean])     extends Transformer[Any] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.filterWith((t, field) => f.lift((t.name.getOrElse(""), field.name)).getOrElse(true)) |+|
        TypeVisitor.inputFields.filterWith((t, field) => f.lift((t.name.getOrElse(""), field.name)).getOrElse(true))

    def transformStep[R]: PartialFunction[Step[R], Step[R]] = { case ObjectStep(typeName, fields) =>
      ObjectStep(typeName, fields.filter { case (fieldName, _) => f.lift((typeName, fieldName)).getOrElse(true) })
    }
  }
  case class Extend[R](
    typeName: String,
    fieldName: String,
    fieldDefinition: __Field,
    extraFields: Set[String],
    resolver: Map[String, InputValue] => Step[R]
  ) extends Transformer[R] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.addWith(t =>
        if (t.name.contains(typeName)) List(fieldDefinition.copy(name = fieldName, args = Nil)) else Nil
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
              (extraFields -- field.fields.map(_.name).toSet).map(Field(_, __Type(__TypeKind.NON_NULL), None))
          )
        else field.copy(fields = field.fields.map(patchField(_, transformer)))
      case _                                              => field
    }
}
