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
  case class RenameType(from: String, to: String)                                          extends Transformer[Any] {
    private def rename(name: String): String = if (name == from) to else name

    val typeVisitor: TypeVisitor =
      TypeVisitor.modify(t => t.copy(name = t.name.map(rename))) |+|
        TypeVisitor.enumValues.modify(v => v.copy(name = rename(v.name)))

    def transformStep[R]: PartialFunction[Step[R], Step[R]] = { case ObjectStep(`from`, fields) =>
      ObjectStep(to, fields)
    }
  }
  case class RenameField(typeName: String, from: String, to: String)                       extends Transformer[Any] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith((t, field) =>
        if (t.name.contains(typeName) && field.name == from) field.copy(name = to) else field
      ) |+|
        TypeVisitor.inputFields.modifyWith((t, field) =>
          if (t.name.contains(typeName) && field.name == from) field.copy(name = to) else field
        )

    def transformStep[R]: PartialFunction[Step[R], Step[R]] = { case ObjectStep(`typeName`, fields) =>
      ObjectStep(
        typeName,
        fields.map { case (fieldName, step) => if (fieldName == from) to -> step else fieldName -> step }
      )
    }
  }
  case class RenameArgument(typeName: String, fieldName: String, from: String, to: String) extends Transformer[Any] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith((t, field) =>
        if (t.name.contains(typeName) && field.name == fieldName)
          field.copy(args = field.args.map(arg => if (arg.name == from) arg.copy(name = to) else arg))
        else field
      )

    def transformStep[R]: PartialFunction[Step[R], Step[R]] = { case ObjectStep(typeName, fields) =>
      ObjectStep(
        typeName,
        fields.map { case (fieldName, step) =>
          fieldName -> (if (typeName == this.typeName && fieldName == this.fieldName) step match {
                          case FunctionStep(mapToStep) =>
                            FunctionStep(args =>
                              mapToStep(args.map { case (k, v) => if (k == to) from -> v else k -> v })
                            )
                          case MetadataFunctionStep(f) =>
                            MetadataFunctionStep(f(_) match {
                              case FunctionStep(mapToStep) =>
                                FunctionStep(args =>
                                  mapToStep(args.map { case (k, v) =>
                                    if (k == to) from -> v else k -> v
                                  })
                                )
                              case other                   => other
                            })
                          case other                   => other
                        }
                        else step)
        }
      )
    }
  }
  case class FilterInterface(f: PartialFunction[(String, String), Boolean])                extends Transformer[Any] {
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
  case class FilterField(f: PartialFunction[(String, String), Boolean])                    extends Transformer[Any] {
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
