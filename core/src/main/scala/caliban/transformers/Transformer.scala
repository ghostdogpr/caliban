package caliban.transformers

import caliban.InputValue
import caliban.execution.Field
import caliban.introspection.adt._
import caliban.schema.Step
import caliban.schema.Step.{ FunctionStep, MetadataFunctionStep, NullStep, ObjectStep }
import scala.collection.compat._

/**
 * A transformer is able to modify a type, modifying its schema and the way it is resolved.
 */
abstract class Transformer[-R] { self =>
  val typeVisitor: TypeVisitor

  def transformStep[R1 <: R](step: Step[R1], field: Field): Step[R1]

  def |+|[R0 <: R](that: Transformer[R0]): Transformer[R0] = new Transformer[R0] {
    val typeVisitor: TypeVisitor = self.typeVisitor |+| that.typeVisitor

    def transformStep[R1 <: R0](step: Step[R1], field: Field): Step[R1] =
      that.transformStep(self.transformStep(step, field), field)
  }

  final def isEmpty: Boolean = self eq Transformer.Empty
}

object Transformer {

  /**
   * A transformer that does nothing.
   */
  def empty[R]: Transformer[R] = Empty

  private case object Empty extends Transformer[Any] {
    val typeVisitor: TypeVisitor = TypeVisitor.empty

    def transformStep[R1](step: Step[R1], field: Field): Step[R1] = step
    override def |+|[R0](that: Transformer[R0]): Transformer[R0]  = that
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
      new RenameType(f.toMap)
  }

  final private class RenameType(map: Map[String, String]) extends Transformer[Any] {

    private def renameType(t: __Type) =
      t.name.flatMap(map.get).fold(t)(newName => t.copy(name = Some(newName)))

    private def renameEnum(t: __EnumValue) =
      map.get(t.name).fold(t)(newName => t.copy(name = newName))

    val typeVisitor: TypeVisitor =
      TypeVisitor.modify(renameType(_)) |+| TypeVisitor.enumValues.modify(renameEnum)

    def transformStep[R](step: Step[R], field: Field): Step[R] = step match {
      case step @ ObjectStep(typeName, _) =>
        map.getOrElse(typeName, null) match {
          case null    => step
          case newName => step.copy(name = newName)
        }
      case _                              => step
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
      new RenameField(tuplesToMap2(f: _*))
  }

  final private class RenameField(visitorMap: Map[String, Map[String, String]]) extends Transformer[Any] {
    private val transformMap = swapMap2(visitorMap)

    private def renameField(t: __Type, field: __Field) = {
      val newName = getFromMap2(visitorMap, null)(t.name.getOrElse(""), field.name)
      if (newName eq null) field else field.copy(name = newName)
    }

    private def renameInputField(t: __Type, input: __InputValue) = {
      val newName = getFromMap2(visitorMap, null)(t.name.getOrElse(""), input.name)
      if (newName eq null) input else input.copy(name = newName)
    }

    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith(renameField) |+| TypeVisitor.inputFields.modifyWith(renameInputField)

    def transformStep[R](step: Step[R], field: Field): Step[R] = step match {
      case step @ ObjectStep(typeName, fields) =>
        transformMap.getOrElse(typeName, null) match {
          case null => step
          case map  => step.copy(fields = name => fields(map.getOrElse(name, name)))
        }
      case _                                   => step
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
     * }}}
     *
     * @param f tuples in the format of `(TypeName -> fieldName -> oldArgumentName -> newArgumentName)`
     */
    def apply(f: (((String, String), String), String)*): Transformer[Any] =
      new RenameArgument(tuplesToMap3(f: _*))
  }

  final private class RenameArgument(visitorMap: Map[String, Map[String, Map[String, String]]])
      extends Transformer[Any] {

    private val transformMap: Map[String, Map[String, Map[String, String]]] =
      swapMap3(visitorMap)

    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith((t, field) =>
        visitorMap.get(t.name.getOrElse("")).flatMap(_.get(field.name)) match {
          case Some(rename) =>
            field.copy(args =
              field
                .args(_)
                .map(arg =>
                  rename
                    .get(arg.name)
                    .fold(arg)(newName => arg.copy(name = newName))
                )
            )
          case None         => field
        }
      )

    def transformStep[R](step: Step[R], field: Field): Step[R] = step match {
      case step @ ObjectStep(typeName, fields) =>
        transformMap.getOrElse(typeName, null) match {
          case null => step
          case map0 =>
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
      case _                                   => step
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
      new ExcludeField(f.groupMap(_._1)(_._2).transform((_, l) => l.toSet))
  }

  final private class ExcludeField(map: Map[String, Set[String]]) extends Transformer[Any] {

    private def shouldKeep(typeName: String, fieldName: String): Boolean =
      !map.getOrElse(typeName, Set.empty).contains(fieldName)

    val typeVisitor: TypeVisitor = {
      TypeVisitor.fields.filterWith((t, field) => shouldKeep(t.name.getOrElse(""), field.name)) |+|
        TypeVisitor.inputFields.filterWith((t, field) => shouldKeep(t.name.getOrElse(""), field.name))
    }

    def transformStep[R](step: Step[R], field: Field): Step[R] = step match {
      case step @ ObjectStep(typeName, fields) =>
        map.getOrElse(typeName, null) match {
          case null    => step
          case exclude => step.copy(fields = name => if (!exclude(name)) fields(name) else NullStep)
        }
      case _                                   => step
    }
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

    def transformStep[R](step: Step[R], field: Field): Step[R] = step match {
      case step @ ObjectStep(typeName, fields) =>
        map.getOrElse(typeName, null) match {
          case null => step
          case map1 =>
            step.copy(fields = fieldName => {
              val s = map1.getOrElse(fieldName, null)
              if (s eq null) fields(fieldName)
              else mapFunctionStep(fields(fieldName))(_.filterNot { case (argName, _) => s.contains(argName) })
            })
        }
      case _                                   => step
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
