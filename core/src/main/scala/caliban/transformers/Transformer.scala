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

  /**
   * A transformer that does nothing.
   */
  case object Empty extends Transformer[Any] {
    val typeVisitor: TypeVisitor = TypeVisitor.empty

    def transformStep[R1](step: Step[R1], field: Field): Step[R1] = step

    override def |+|[R0](that: Transformer[R0]): Transformer[R0] = new Transformer[R0] {
      val typeVisitor: TypeVisitor                                        = that.typeVisitor
      def transformStep[R1 <: R0](step: Step[R1], field: Field): Step[R1] = that.transformStep(step, field)
    }
  }

  /**
   * A transformer that allows renaming types.
   * @param f a partial function that takes a type name and returns a new name for that type
   */
  case class RenameType(private val f: (String, String)*) extends Transformer[Any] {
    private val map = f.toMap

    private def rename(name: String): String = map.getOrElse(name, name)

    val typeVisitor: TypeVisitor =
      TypeVisitor.modify(t => t.copy(name = t.name.map(rename))) |+|
        TypeVisitor.enumValues.modify(v => v.copy(name = rename(v.name)))

    def transformStep[R](step: Step[R], field: Field): Step[R] = step match {
      case ObjectStep(typeName, fields) =>
        val res = map.get(typeName)
        if (res.isEmpty) step else ObjectStep(res.get, fields)
      case _                            => step
    }
  }

  /**
   * A transformer that allows renaming fields.
   * @param f a partial function that takes a type name and a field name and returns a new name for that field
   */
  case class RenameField(private val f: (String, (String, String))*) extends Transformer[Any] {
    private val visitorMap   = toMap2(f)
    private val transformMap = swapMap2(visitorMap)

    val typeVisitor: TypeVisitor = {
      def _get(t: __Type, name: String) = getFromMap2(visitorMap, name)(t.name.getOrElse(""), name)

      TypeVisitor.fields.modifyWith((t, field) => field.copy(name = _get(t, field.name))) |+|
        TypeVisitor.inputFields.modifyWith((t, field) => field.copy(name = _get(t, field.name)))
    }

    def transformStep[R](step: Step[R], field: Field): Step[R] = step match {
      case ObjectStep(typeName, fields) =>
        ObjectStep(
          typeName,
          fieldName => fields(getFromMap2(transformMap, fieldName)(typeName, fieldName))
        )
      case _                            => step
    }
  }

  /**
   * A transformer that allows renaming arguments.
   * @param f a partial function that takes a type name and a field name and returns another
   *          partial function from an argument name to a new name for that argument
   */
  case class RenameArgument(private val f: (String, (String, (String, String)))*) extends Transformer[Any] {
    private val visitorMap   = toMap3(f)
    private val transformMap = swapMap3(visitorMap)

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
      case ObjectStep(typeName, fields) =>
        ObjectStep(
          typeName,
          fieldName => {
            val step   = fields(fieldName)
            val rename = transformMap.get(typeName).flatMap(_.get(fieldName))
            if (rename.isEmpty) step
            else
              mapFunctionStep(step)(_.map { case (argName, input) =>
                rename.get.getOrElse(argName, argName) -> input
              })
          }
        )
      case _                            => step
    }
  }

  /**
   * A transformer that allows filtering fields.
   * @param f a partial function that takes a type name and a field name and
   *          returns a boolean (true means the field should be kept)
   */
  case class FilterField(private val f: (String, (String, Boolean))*) extends Transformer[Any] {
    private val map = toMap2[Boolean](f)

    val typeVisitor: TypeVisitor = {
      val _get = getFromMap2(map) _
      TypeVisitor.fields.filterWith((t, field) => _get(t.name.getOrElse(""), field.name)) |+|
        TypeVisitor.inputFields.filterWith((t, field) => _get(t.name.getOrElse(""), field.name))
    }

    def transformStep[R](step: Step[R], field: Field): Step[R] = step match {
      case ObjectStep(typeName, fields) =>
        ObjectStep(
          typeName,
          fieldName => if (getFromMap2(map, default = true)(typeName, fieldName)) fields(fieldName) else NullStep
        )
      case _                            => step
    }
  }

  /**
   * A transformer that allows filtering types.
   *
   * @param f a partial function that takes a type name and an interface name and
   *          returns a boolean (true means the type should be kept)
   */
  case class FilterInterface(private val f: (String, (String, Boolean))*) extends Transformer[Any] {
    private val map = toMap2(f)

    val typeVisitor: TypeVisitor =
      TypeVisitor.modify(t =>
        t.copy(interfaces =
          () =>
            t.interfaces()
              .map(
                _.filter(interface => getFromMap2(map)(t.name.getOrElse(""), interface.name.getOrElse("")))
              )
        )
      )

    def transformStep[R](step: Step[R], field: Field): Step[R] = step
  }

  /**
   * A transformer that allows filtering arguments.
   * @param f a partial function that takes a type name, a field name and an argument name and
   *          returns a boolean (true means the argument should be kept)
   */
  case class FilterArgument(private val f: (String, (String, (String, Boolean)))*) extends Transformer[Any] {
    private val map = toMap3(f)

    val typeVisitor: TypeVisitor =
      TypeVisitor.fields.modifyWith((t, field) =>
        field.copy(args =
          field
            .args(_)
            .filter(arg => getFromMap3(map)(t.name.getOrElse(""), field.name, arg.name))
        )
      )

    def transformStep[R](step: Step[R], field: Field): Step[R] = step match {
      case ObjectStep(typeName, fields) =>
        ObjectStep(
          typeName,
          fieldName =>
            mapFunctionStep(fields(fieldName))(_.filter { case (argName, _) =>
              getFromMap3(map)(typeName, fieldName, argName)
            })
        )
      case _                            => step
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

  private def toMap2[V](t: Seq[(String, (String, V))]): Map[String, Map[String, V]] =
    t.groupMap(_._1)(_._2).transform { case (_, l) => l.toMap }

  private def toMap3[V](
    t: Seq[(String, (String, (String, V)))]
  ): Map[String, Map[String, Map[String, V]]] =
    t.groupMap(_._1)(_._2).transform { case (_, l) => l.groupMap(_._1)(_._2).transform { case (_, l) => l.toMap } }

  private def swapMap2[V](m: Map[String, Map[String, V]]): Map[String, Map[V, String]] =
    m.transform { case (_, m) => m.map(_.swap) }

  private def swapMap3[V](m: Map[String, Map[String, Map[String, V]]]): Map[String, Map[String, Map[V, String]]] =
    m.transform { case (_, m) => m.transform { case (_, m) => m.map(_.swap) } }

  private def getFromMap2(
    m: Map[String, Map[String, String]],
    default: => String
  )(k1: String, k2: String): String =
    m.get(k1).flatMap(_.get(k2)).getOrElse(default)

  // Overloading to avoid boxing of Boolean
  private def getFromMap2(
    m: Map[String, Map[String, Boolean]],
    default: Boolean = true
  )(k1: String, k2: String): Boolean = {
    val res = m.get(k1).flatMap(_.get(k2))
    if (res.isEmpty) default else res.get
  }

  private def getFromMap3(
    m: Map[String, Map[String, Map[String, Boolean]]],
    default: Boolean = true
  )(k1: String, k2: String, k3: String): Boolean = {
    val res = m.get(k1).flatMap(_.get(k2)).flatMap(_.get(k3))
    if (res.isEmpty) default else res.get
  }
}
