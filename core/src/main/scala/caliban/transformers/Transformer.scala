package caliban.transformers

import caliban.CalibanError.ValidationError
import caliban.InputValue
import caliban.execution.Field
import caliban.introspection.adt._
import caliban.schema.Step
import caliban.schema.Step.{ FunctionStep, MetadataFunctionStep, NullStep, ObjectStep }

import scala.collection.compat._
import scala.collection.mutable

/**
 * A transformer is able to modify a type, modifying its schema and the way it is resolved.
 */
abstract class Transformer[-R] { self =>
  val typeVisitor: TypeVisitor

  /**
   * Set of type names that this transformer applies to.
   * Needed for applying optimizations when combining transformers.
   */
  protected val typeNames: collection.Set[String]

  protected def transformStep[R1 <: R](step: ObjectStep[R1], field: Field): ObjectStep[R1]

  def apply[R1 <: R](step: ObjectStep[R1], field: Field): ObjectStep[R1] =
    transformStep(step, field)

  def |+|[R0 <: R](that: Transformer[R0]): Transformer[R0] =
    (self, that) match {
      case (l, Transformer.Empty) => l
      case (Transformer.Empty, r) => r
      case _                      => new Transformer.Combined[R0](self, that)
    }
}

object Transformer {

  /**
   * A transformer that does nothing.
   */
  def empty[R]: Transformer[R] = Empty

  private case object Empty extends Transformer[Any] {
    val typeVisitor: TypeVisitor = TypeVisitor.empty

    protected val typeNames: Set[String] = Set.empty

    protected def transformStep[R1](step: ObjectStep[R1], field: Field): ObjectStep[R1] = step
  }

  object RenameType {

    /**
     * A transformer that allows renaming types.
     * {{{
     *   RenameType(
     *     "Foo" -> "Bar",
     *     "Baz" -> "Qux"
     *   )
     * }}}
     * @param f tuples in the format of `(OldName -> NewName)`
     */
    def apply(f: (String, String)*): Transformer[Any] =
      if (f.isEmpty) Empty else new RenameType(f.toMap)
  }

  final private class RenameType(map: Map[String, String]) extends Transformer[Any] {

    val typeVisitor: TypeVisitor = {
      val renameType = { (t: __Type) =>
        t.name.flatMap(map.get).fold(t)(newName => t.copy(name = Some(newName)))
      }
      val renameEnum = { (t: __EnumValue) =>
        map.get(t.name).fold(t)(newName => t.copy(name = newName))
      }

      TypeVisitor.modify(renameType) |+| TypeVisitor.enumValues.modify(renameEnum)
    }

    protected val typeNames: Set[String] = map.keySet

    protected def transformStep[R](step: ObjectStep[R], field: Field): ObjectStep[R] =
      map.getOrElse(step.name, null) match {
        case null    => step
        case newName => step.copy(name = newName)
      }
  }

  object RenameField {

    /**
     * A transformer that allows renaming fields on types
     *
     * {{{
     *   RenameField(
     *     "TypeA" -> "foo" -> "bar",
     *     "TypeB" -> "baz" -> "qux",
     *   )
     * }}}
     *
     * @param f tuples in the format of `(TypeName -> oldName -> newName)`
     */

    def apply(f: ((String, String), String)*): Transformer[Any] =
      if (f.isEmpty) Empty else new RenameField(tuplesToMap2(f: _*))
  }

  final private class RenameField(visitorMap: Map[String, Map[String, String]]) extends Transformer[Any] {
    private val transformMap = swapMap2(visitorMap)

    val typeVisitor: TypeVisitor = {
      def getName(t: __Type, name: String) = getFromMap2(visitorMap, null)(t.name.getOrElse(""), name)

      val renameField = { (t: __Type, field: __Field) =>
        val newName = getName(t, field.name)
        if (newName eq null) field else field.copy(name = newName)
      }

      val renameInputField = { (t: __Type, input: __InputValue) =>
        val newName = getName(t, input.name)
        if (newName eq null) input else input.copy(name = newName)
      }
      TypeVisitor.fields.modifyWith(renameField) |+| TypeVisitor.inputFields.modifyWith(renameInputField)
    }

    protected val typeNames: Set[String] = transformMap.keySet

    protected def transformStep[R](step: ObjectStep[R], field: Field): ObjectStep[R] =
      transformMap.getOrElse(step.name, null) match {
        case null => step
        case map  => step.copy(fields = name => step.fields(map.getOrElse(name, name)))
      }
  }

  object RenameArgument {

    /**
     * A transformer that allows renaming arguments on fields
     *
     * {{{
     *   RenameArgument(
     *     "TypeA" -> "fieldA" -> "foo" -> "bar",
     *     "TypeA" -> "fieldB" -> "baz" -> "qux",
     *   )
     * }}}
     *
     * @param f tuples in the format of `(TypeName -> fieldName -> oldArgumentName -> newArgumentName)`
     */
    def apply(f: (((String, String), String), String)*): Transformer[Any] =
      if (f.isEmpty) Empty else new RenameArgument(tuplesToMap3(f: _*))
  }

  final private class RenameArgument(visitorMap: Map[String, Map[String, Map[String, String]]])
      extends Transformer[Any] {

    private val transformMap: Map[String, Map[String, Map[String, String]]] = swapMap3(visitorMap)

    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith((t, field) =>
        visitorMap.get(t.name.getOrElse("")).flatMap(_.get(field.name)) match {
          case Some(renames) =>
            field.copy(args = field.args(_).map { arg =>
              renames.get(arg.name).fold(arg)(newName => arg.copy(name = newName))
            })
          case None          => field
        }
      )

    protected val typeNames: Set[String] = transformMap.keySet

    protected def transformStep[R](step: ObjectStep[R], field: Field): ObjectStep[R] =
      transformMap.getOrElse(step.name, null) match {
        case null => step
        case map0 =>
          val fields = step.fields
          step.copy(fields =
            fieldName =>
              map0.getOrElse(fieldName, null) match {
                case null => fields(fieldName)
                case map1 =>
                  mapFunctionStep(fields(fieldName))(_.map { case (argName, input) =>
                    map1.getOrElse(argName, argName) -> input
                  })
              }
          )
      }
  }

  object ExcludeField {

    /**
     * A transformer that allows excluding fields from types.
     *
     * {{{
     *   ExcludeField(
     *     "TypeA" -> "foo",
     *     "TypeB" -> "bar",
     *   )
     * }}}
     *
     * @param f tuples in the format of `(TypeName -> fieldToBeExcluded)`
     */
    def apply(f: (String, String)*): Transformer[Any] =
      if (f.isEmpty) Empty else new ExcludeField(f.groupMap(_._1)(_._2).transform((_, l) => l.toSet))
  }

  final private class ExcludeField(map: Map[String, Set[String]]) extends Transformer[Any] {

    private def shouldKeep(typeName: String, fieldName: String): Boolean =
      !map.getOrElse(typeName, Set.empty).contains(fieldName)

    val typeVisitor: TypeVisitor = {
      TypeVisitor.fields.filterWith((t, field) => shouldKeep(t.name.getOrElse(""), field.name)) |+|
        TypeVisitor.inputFields.filterWith((t, field) => shouldKeep(t.name.getOrElse(""), field.name))
    }

    protected val typeNames: Set[String] = map.keySet

    protected def transformStep[R](step: ObjectStep[R], field: Field): ObjectStep[R] =
      map.getOrElse(step.name, null) match {
        case null => step
        case excl => step.copy(fields = name => if (!excl(name)) step.fields(name) else NullStep)
      }
  }

  object ExcludeInputField {

    /**
     * A transformer that allows excluding fields from input types.
     *
     * {{{
     *   ExcludeField(
     *     "TypeAInput" -> "foo",
     *     "TypeBInput" -> "bar",
     *   )
     * }}}
     *
     * @note the '''field must be optional''', otherwise the filter will be silently ignored
     * @param f tuples in the format of `(TypeName -> inputFieldToExclude)`
     */
    def apply(f: (String, String)*): Transformer[Any] =
      if (f.isEmpty) Empty else new ExcludeInputField(f.groupMap(_._1)(_._2).transform((_, l) => l.toSet))
  }

  final private class ExcludeInputField(map: Map[String, Set[String]]) extends Transformer[Any] {

    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modify { field =>
        def loop(parentType: Option[String])(arg: __InputValue): Option[__InputValue] =
          parentType.flatMap(map.get) match {
            case Some(s) if arg._type.isNullable && s.contains(arg.name) =>
              None
            case _                                                       =>
              lazy val newType = arg._type.mapInnerType { t =>
                t.copy(inputFields = t.inputFields(_).map(_.flatMap(loop(t.name))))
              }
              Some(arg.copy(`type` = () => newType))
          }

        field.copy(args = field.args(_).flatMap(loop(None)))
      }

    protected val typeNames: Set[String]                                             = Set.empty
    protected def transformStep[R](step: ObjectStep[R], field: Field): ObjectStep[R] = step

  }

  object ExcludeArgument {

    /**
     * A transformer that allows excluding arguments from fields
     *
     * {{{
     *   ExcludeArgument(
     *     "TypeA" -> "fieldA" -> "arg",
     *     "TypeA" -> "fieldB" -> "arg2",
     *   )
     * }}}
     *
     * @param f tuples in the format of `(TypeName -> fieldName -> argumentToBeExcluded)`
     */
    def apply(f: ((String, String), String)*): Transformer[Any] =
      if (f.isEmpty) Empty
      else
        new ExcludeArgument(
          f
            .groupMap(_._1._1)(v => v._1._2 -> v._2)
            .transform((_, v) => v.groupMap(_._1)(_._2).transform((_, v) => v.toSet))
        )
  }

  final private class ExcludeArgument(map: Map[String, Map[String, Set[String]]]) extends Transformer[Any] {

    private def shouldKeep(typeName: String, fieldName: String, argName: String): Boolean =
      !getFromMap2(map, Set.empty[String])(typeName, fieldName).contains(argName)

    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith((t, field) =>
        field.copy(args =
          field
            .args(_)
            .filter(arg => shouldKeep(t.name.getOrElse(""), field.name, arg.name))
        )
      )

    protected val typeNames: Set[String] = map.keySet

    protected def transformStep[R](step: ObjectStep[R], field: Field): ObjectStep[R] =
      map.getOrElse(step.name, null) match {
        case null  => step
        case inner =>
          val fields = step.fields
          step.copy(fields =
            fieldName =>
              inner.getOrElse(fieldName, null) match {
                case null => fields(fieldName)
                case excl => mapFunctionStep(fields(fieldName))(_.filterNot { case (argName, _) => excl(argName) })
              }
          )
      }
  }

  final private class Combined[-R](left: Transformer[R], right: Transformer[R]) extends Transformer[R] {
    val typeVisitor: TypeVisitor = left.typeVisitor |+| right.typeVisitor

    protected val typeNames: mutable.HashSet[String] = {
      val set = mutable.HashSet.from(left.typeNames)
      set ++= right.typeNames
      set
    }

    protected def transformStep[R1 <: R](step: ObjectStep[R1], field: Field): ObjectStep[R1] =
      right.transformStep(left.transformStep(step, field), field)

    override def apply[R1 <: R](step: ObjectStep[R1], field: Field): ObjectStep[R1] =
      if (typeNames(step.name)) transformStep(step, field) else step
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

  private def tuplesToMap2(f: ((String, String), String)*): Map[String, Map[String, String]] =
    f.groupMap(_._1._1)(v => v._1._2 -> v._2).transform((_, l) => l.toMap)

  private def tuplesToMap3(f: (((String, String), String), String)*): Map[String, Map[String, Map[String, String]]] =
    f.groupMap(_._1._1._1)(v => v._1._1._2 -> v._1._2 -> v._2).transform((_, l) => tuplesToMap2(l: _*))

  private def swapMap2[V](m: Map[String, Map[String, V]]): Map[String, Map[V, String]] =
    m.transform((_, m) => m.map(_.swap))

  private def swapMap3[V](m: Map[String, Map[String, Map[String, V]]]): Map[String, Map[String, Map[V, String]]] =
    m.transform((_, m) => swapMap2(m))

  private def getFromMap2[V](
    m: Map[String, Map[String, V]],
    default: => V
  )(k1: String, k2: String): V =
    m.get(k1).flatMap(_.get(k2)).getOrElse(default)
}
