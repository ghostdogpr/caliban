package caliban.transformers

import caliban.Value.StringValue
import caliban.execution.Field
import caliban.introspection.adt._
import caliban.schema.Step.{ FunctionStep, MetadataFunctionStep, ObjectStep, QueryStep }
import caliban.schema.{ PureStep, Step }
import caliban.{ InputValue, ResponseValue, Value }

/**
 * A transformer is able to modify a type, modifying its schema and the way it is resolved.
 */
trait Transformer[-R] { self =>
  val typeVisitor: TypeVisitor

  def transformField[R1 <: R]: PartialFunction[(Step[R1], Field), Field] = PartialFunction.empty

  def transformStep[R1 <: R]: PartialFunction[(Step[R1], Field), Step[R1]]

  def |+|[R0 <: R](that: Transformer[R0]): Transformer[R0] = new Transformer[R0] {
    val typeVisitor: TypeVisitor = self.typeVisitor |+| that.typeVisitor

    override def transformField[R1 <: R]: PartialFunction[(Step[R1], Field), Field] = { case (step, field) =>
      val modifiedField = self.transformField.lift((step, field)).getOrElse(field)
      that.transformField.lift((step, modifiedField)).getOrElse(modifiedField)
    }

    def transformStep[R1 <: R0]: PartialFunction[(Step[R1], Field), Step[R1]] = { case (step, field) =>
      val modifiedStep = self.transformStep.lift((step, field)).getOrElse(step)
      that.transformStep.lift((modifiedStep, field)).getOrElse(modifiedStep)
    }
  }
}

object Transformer {

  /**
   * A transformer that does nothing.
   */
  val empty: Transformer[Any] = new Transformer[Any] {
    val typeVisitor: TypeVisitor                                        = TypeVisitor.empty
    def transformStep[R1]: PartialFunction[(Step[R1], Field), Step[R1]] = PartialFunction.empty
    override def |+|[R0](that: Transformer[R0]): Transformer[R0]        = new Transformer[R0] {
      val typeVisitor: TypeVisitor                                                     = that.typeVisitor
      override def transformField[R1 <: R0]: PartialFunction[(Step[R1], Field), Field] = that.transformField
      def transformStep[R1 <: R0]: PartialFunction[(Step[R1], Field), Step[R1]]        = that.transformStep
    }
  }

  /**
   * A transformer that allows renaming types.
   * @param f a partial function that takes a type name and returns a new name for that type
   */
  case class RenameType(f: PartialFunction[String, String]) extends Transformer[Any] {
    private def rename(name: String): String = f.lift(name).getOrElse(name)

    val typeVisitor: TypeVisitor =
      TypeVisitor.modify(t => t.copy(name = t.name.map(rename))) |+|
        TypeVisitor.enumValues.modify(v => v.copy(name = rename(v.name)))

    def transformStep[R]: PartialFunction[(Step[R], Field), Step[R]] = { case (step @ ObjectStep(name, fields), _) =>
      f.lift(name).map(ObjectStep(_, fields)).getOrElse(step)
    }
  }

  /**
   * A transformer that allows renaming fields.
   * @param f a partial function that takes a type name and a field name and returns a new name for that field
   */
  case class RenameField(f: PartialFunction[(String, String), String]) extends Transformer[Any] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith((t, field) =>
        field.copy(name = f.lift((t.name.getOrElse(""), field.name)).getOrElse(field.name))
      ) |+|
        TypeVisitor.inputFields.modifyWith((t, field) =>
          field.copy(name = f.lift((t.name.getOrElse(""), field.name)).getOrElse(field.name))
        )

    def transformStep[R]: PartialFunction[(Step[R], Field), Step[R]] = { case (ObjectStep(typeName, fields), _) =>
      ObjectStep(
        typeName,
        fields.map { case (fieldName, step) => f.lift((typeName, fieldName)).getOrElse(fieldName) -> step }
      )
    }
  }

  /**
   * A transformer that allows renaming arguments.
   * @param f a partial function that takes a type name and a field name and returns another
   *          partial function from an argument name to a new name for that argument
   */
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

    def transformStep[R]: PartialFunction[(Step[R], Field), Step[R]] = { case (ObjectStep(typeName, fields), _) =>
      ObjectStep(
        typeName,
        fields.map { case (fieldName, step) =>
          fieldName -> (f.lift((typeName, fieldName)) match {
            case Some((_, rename)) =>
              mapFunctionStep(step)(_.map { case (argName, input) => rename.lift(argName).getOrElse(argName) -> input })
            case None              => step
          })
        }
      )
    }
  }

  /**
   * A transformer that allows filtering fields.
   * @param f a partial function that takes a type name and a field name and
   *          returns a boolean (true means the field should be kept)
   */
  case class FilterField(f: PartialFunction[(String, String), Boolean]) extends Transformer[Any] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.filterWith((t, field) => f.lift((t.name.getOrElse(""), field.name)).getOrElse(true)) |+|
        TypeVisitor.inputFields.filterWith((t, field) => f.lift((t.name.getOrElse(""), field.name)).getOrElse(true))

    def transformStep[R]: PartialFunction[(Step[R], Field), Step[R]] = { case (ObjectStep(typeName, fields), _) =>
      ObjectStep(typeName, fields.filter { case (fieldName, _) => f.lift((typeName, fieldName)).getOrElse(true) })
    }
  }

  /**
   * A transformer that allows filtering types.
   *
   * @param f a partial function that takes a type name and an interface name and
   *          returns a boolean (true means the type should be kept)
   */
  case class FilterInterface(f: PartialFunction[(String, String), Boolean]) extends Transformer[Any] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.modify(t =>
        t.copy(interfaces =
          () =>
            t.interfaces()
              .map(_.filter(interface => f.lift((t.name.getOrElse(""), interface.name.getOrElse(""))).getOrElse(true)))
        )
      )

    def transformStep[R]: PartialFunction[(Step[R], Field), Step[R]] = { case (step, _) => step }
  }

  /**
   * A transformer that allows filtering arguments.
   * @param f a partial function that takes a type name, a field name and an argument name and
   *          returns a boolean (true means the argument should be kept)
   */
  case class FilterArgument(f: PartialFunction[(String, String, String), Boolean]) extends Transformer[Any] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith((t, field) =>
        field.copy(args =
          field.args.filter(arg => f.lift((t.name.getOrElse(""), field.name, arg.name)).getOrElse(true))
        )
      )

    def transformStep[R]: PartialFunction[(Step[R], Field), Step[R]] = { case (ObjectStep(typeName, fields), _) =>
      ObjectStep(
        typeName,
        fields.map { case (fieldName, step) =>
          fieldName -> mapFunctionStep(step)(_.filter { case (argName, _) =>
            f.lift((typeName, fieldName, argName)).getOrElse(true)
          })
        }
      )
    }
  }
  case class Extend[R](
    typeName: String,
    fieldName: String,
    sourceFieldDefinition: __Field,
    sourceStep: Step[R],
    extendField: (Map[String, InputValue], Field) => Field,
    argumentMappings: Map[String, InputValue => (String, InputValue)],
    mapBatchResultToArguments: PartialFunction[ResponseValue, Map[String, ResponseValue]]
  ) extends Transformer[R] {
    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.addWith(t =>
        if (t.name.contains(typeName)) List(sourceFieldDefinition.copy(name = fieldName, args = Nil)) else Nil
      )

    // TODO this is called only for ObjectStep and MetadataFunctionStep, weird?
    override def transformField[R1 <: R]: PartialFunction[(Step[R1], Field), Field] = {
      case (ObjectStep(`typeName`, fields), field) if field.name == fieldName =>
        val pureFields = fields.collect { case (name, PureStep(value)) => name -> value.toInputValue }
        extendField(pureFields, field)
      case (MetadataFunctionStep(_), field)                                   =>
        extendField(Map.empty, field)
    }

    def transformStep[R1 <: R]: PartialFunction[(Step[R1], Field), Step[R1]] = {
      case (ObjectStep(`typeName`, fields), _)                                                   =>
        ObjectStep[R1](typeName, fields + (fieldName -> sourceStep))
      case (QueryStep(inner), field) if field.isRoot || field.name == sourceFieldDefinition.name =>
        QueryStep(inner.map {
          case PureStep(value) =>
            responseValueToStep(
              value.asListValue.fold(value)(
                _.filter(
                  mapBatchResultToArguments
                    .lift(_)
                    .fold(true)(_.flatMap { case (k, v) =>
                      argumentMappings.get(k).map(_(v.toInputValue))
                    } == field.arguments)
                )
              )
            )
          case other           => other
        })
    }
  }

  private def responseValueToStep(responseValue: ResponseValue): Step[Any] =
    responseValue match {
      case ResponseValue.ListValue(values)   => Step.ListStep(values.map(responseValueToStep))
      case ResponseValue.ObjectValue(fields) =>
        val typeName = fields.toMap.get("__typename").collect { case StringValue(value) => value }.getOrElse("")
        Step.ObjectStep(typeName, fields.map { case (k, v) => k -> responseValueToStep(v) }.toMap)
      case ResponseValue.StreamValue(stream) => Step.StreamStep(stream.map(responseValueToStep))
      case value: Value                      => PureStep(value)
    }

  private def mapFunctionStep[R](step: Step[R])(f: Map[String, InputValue] => Map[String, InputValue]): Step[R] =
    step match {
      case FunctionStep(mapToStep) => FunctionStep(args => mapToStep(f(args)))
      case MetadataFunctionStep(m) =>
        MetadataFunctionStep(m(_) match {
          case FunctionStep(mapToStep) => FunctionStep(args => mapToStep(f(args)))
          case other                   => other
        })
      case other                   => other
    }
}
