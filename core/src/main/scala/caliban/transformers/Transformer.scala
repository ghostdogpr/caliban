package caliban.transformers

import caliban.InputValue
import caliban.execution.Field
import caliban.introspection.adt._
import caliban.schema.Step
import caliban.schema.Step.{ FunctionStep, MetadataFunctionStep, NullStep, ObjectStep }

import scala.collection.mutable

/**
 * A transformer is able to modify a type, modifying its schema and the way it is resolved.
 */
abstract class Transformer[-R] { self =>
  val typeVisitor: TypeVisitor

  def transformStep[R1 <: R]: PartialFunction[(Step[R1], Field), Step[R1]]

  def |+|[R0 <: R](that: Transformer[R0]): Transformer[R0] = new Transformer[R0] {
    val typeVisitor: TypeVisitor = self.typeVisitor |+| that.typeVisitor

    def transformStep[R1 <: R0]: PartialFunction[(Step[R1], Field), Step[R1]] = { case (step, field) =>
      val modifiedStep = self.transformStep.lift((step, field)).getOrElse(step)
      that.transformStep.lift((modifiedStep, field)).getOrElse(modifiedStep)
    }
  }

  final def isEmpty: Boolean = self eq Transformer.Empty
}

object Transformer {

  /**
   * A transformer that does nothing.
   */
  def empty[R]: Transformer[R] = Empty

  /**
   * A transformer that does nothing.
   */
  case object Empty extends Transformer[Any] {
    val typeVisitor: TypeVisitor = TypeVisitor.empty

    def transformStep[R1]: PartialFunction[(Step[R1], Field), Step[R1]] = PartialFunction.empty

    override def |+|[R0](that: Transformer[R0]): Transformer[R0] = new Transformer[R0] {
      val typeVisitor: TypeVisitor                                              = that.typeVisitor
      def transformStep[R1 <: R0]: PartialFunction[(Step[R1], Field), Step[R1]] = that.transformStep
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
  case class RenameField(f: ((String, String), String)*) extends Transformer[Any] {
    private val map        = mutable.HashMap.from(f)
    private val inverseMap = map.map { case ((tName, fName0), fName1) => (tName, fName1) -> fName0 }

    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith((t, field) =>
        field.copy(name = map.getOrElse((t.name.getOrElse(""), field.name), field.name))
      ) |+|
        TypeVisitor.inputFields.modifyWith((t, field) =>
          field.copy(name = map.getOrElse((t.name.getOrElse(""), field.name), field.name))
        )

    def transformStep[R]: PartialFunction[(Step[R], Field), Step[R]] = { case (ObjectStep(typeName, fields), _) =>
      ObjectStep(
        typeName,
        fieldName => fields(inverseMap.getOrElse((typeName, fieldName), fieldName))
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
              field
                .args(_)
                .map(arg =>
                  rename
                    .lift(arg.name)
                    .fold(arg)(newName => arg.copy(name = newName))
                )
            )
          case None              => field
        }
      )

    def transformStep[R]: PartialFunction[(Step[R], Field), Step[R]] = { case (ObjectStep(typeName, fields), _) =>
      ObjectStep(
        typeName,
        fieldName => {
          val step = fields(fieldName)
          f.lift((typeName, fieldName)) match {
            case Some((_, rename)) =>
              mapFunctionStep(step)(_.map { case (argName, input) => rename.lift(argName).getOrElse(argName) -> input })
            case _                 => step
          }
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

    private val fnTrue = (_: (String, String)) => true

    def transformStep[R]: PartialFunction[(Step[R], Field), Step[R]] = { case (ObjectStep(typeName, fields), _) =>
      ObjectStep(
        typeName,
        fieldName =>
          if (f.applyOrElse((typeName, fieldName), fnTrue)) fields(fieldName)
          else NullStep
      )
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
          field.args(_).filter(arg => f.lift((t.name.getOrElse(""), field.name, arg.name)).getOrElse(true))
        )
      )

    def transformStep[R]: PartialFunction[(Step[R], Field), Step[R]] = { case (ObjectStep(typeName, fields), _) =>
      ObjectStep(
        typeName,
        fieldName =>
          mapFunctionStep(fields(fieldName))(_.filter { case (argName, _) =>
            f.lift((typeName, fieldName, argName)).getOrElse(true)
          })
      )
    }
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
