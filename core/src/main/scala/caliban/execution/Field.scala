package caliban.execution

import caliban.Value.{ BooleanValue, IntValue, StringValue }
import caliban.introspection.adt.{ __InputValue, __Type, __TypeKind }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.FragmentDefinition
import caliban.parsing.adt.Selection.{ Field => F, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt.{ Directive, LocationInfo, Selection, VariableDefinition }
import caliban.schema.{ RootType, Types }
import caliban.{ InputValue, Value }

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

/**
 * Represents a field used during the execution of a query
 *
 * @param name The name
 * @param fieldType The GraphQL type
 * @param parentType The parent type of the field
 * @param alias A potential alias specified in the query, i.e `alias: field`
 * @param fields The selected subfields, if any, i.e `field { a b }`
 * @param targets The type conditions used to select this field, i.e `...on Type { field }`
 * @param arguments The specified arguments for the field's resolver
 * @param directives The directives specified on the field
 * @param _condition Internal, the possible types that contains this field
 * @param _locationInfo Internal, the source location in the query
 * @param fragment The fragment that is directly wrapping this field
 */
case class Field(
  name: String,
  fieldType: __Type,
  parentType: Option[__Type],
  alias: Option[String] = None,
  fields: List[Field] = Nil,
  targets: Option[Set[String]] = None,
  arguments: Map[String, InputValue] = Map(),
  directives: List[Directive] = List.empty,
  _condition: Option[Set[String]] = None,
  _locationInfo: () => LocationInfo = () => LocationInfo.origin,
  fragment: Option[Fragment] = None
) { self =>
  lazy val locationInfo: LocationInfo = _locationInfo()

  private[caliban] val aliasedName: String =
    if (alias.isEmpty) name else alias.get

  private[caliban] lazy val allFieldsUniqueNameAndCondition: Boolean = {
    def inner(fields: List[Field]): Boolean = {
      val headCondition = fields.head._condition

      val seen = new mutable.HashSet[String]
      seen.add(fields.head.aliasedName)

      var rem = fields.tail
      while (rem ne Nil) {
        val f        = rem.head
        val continue = seen.add(f.aliasedName) && f._condition == headCondition
        if (!continue) return false
        rem = rem.tail
      }
      true
    }

    val fields0 = fields
    fields0.isEmpty || fields0.tail.isEmpty || inner(fields0)
  }

  private[caliban] def collectFields(typeName: String): List[Field] = {
    def matchesType(field: Field): Boolean =
      field._condition.isEmpty || field._condition.get.contains(typeName)

    if (allFieldsUniqueNameAndCondition) {
      if (fields.isEmpty || !matchesType(fields.head)) Nil else fields
    } else {
      val capacity  = calculateMapCapacity(fields.size)
      val collected = new java.util.LinkedHashMap[String, Field](capacity)
      val nil       = Nil
      var remaining = fields

      while (remaining ne nil) {
        val field = remaining.head
        if (matchesType(field))
          collected.compute(
            field.aliasedName,
            (_, existing) =>
              if (existing eq null) field
              else existing.copy(fields = existing.mergeFields(field), fragment = Field.earliest(existing, field))
          )
        remaining = remaining.tail
      }

      collected.values().asScala.toList
    }
  }

  /**
   * The behaviour of mutable Maps (both Java and Scala) is to resize once the number of entries exceeds
   * the capacity * loadFactor (default of 0.75d) threshold in order to prevent hash collisions.
   *
   * This method is a helper method to estimate the initial map size depending on the number of elements the Map is
   * expected to hold
   *
   * NOTE: This method is the same as java.util.HashMap.calculateHashMapCapacity on JDK19+
   */
  private def calculateMapCapacity(nMappings: Int): Int =
    Math.ceil(nMappings / 0.75d).toInt

  /**
   * The sub-fields of this field and of another selection of it, keeping fields shared by both once.
   */
  private def mergeFields(other: Field): List[Field] =
    if (self.fields.lengthCompare(Field.MaxScannedFields) <= 0)
      self.fields ::: other.fields.filterNot(field => self.fields.exists(_ eq field))
    else {
      val shared = java.util.Collections.newSetFromMap(new java.util.IdentityHashMap[Field, java.lang.Boolean]())
      self.fields.foreach(shared.add)
      self.fields ::: other.fields.filterNot(shared.contains)
    }

  def combine(other: Field): Field =
    self.copy(
      fields = mergeFields(other),
      targets = (self.targets, other.targets) match {
        case (Some(t1), Some(t2)) => if (t1 == t2) self.targets else Some(t1 ++ t2)
        case (Some(_), None)      => self.targets
        case (None, Some(_))      => other.targets
        case (None, None)         => None
      },
      _condition = (self._condition, other._condition) match {
        case (Some(v1), Some(v2)) => if (v1 == v2) self._condition else Some(v1 ++ v2)
        case (Some(_), None)      => self._condition
        case (None, Some(_))      => other._condition
        case (None, None)         => None
      },
      fragment = Field.earliest(self, other)
    )

  lazy val toSelection: Selection = selecting(arguments, selectionSet(_.toSelection))

  /**
   * This field with `arguments` selecting `selectionSet`.
   */
  private[caliban] def selecting(arguments: Map[String, InputValue], selectionSet: List[Selection]): Selection = {
    val schemaDirectives =
      parentType.flatMap(pt => Option(pt.innerType.getFieldOrNull(name))).flatMap(_.directives).getOrElse(Nil)
    val directives       =
      if (schemaDirectives.isEmpty) self.directives else self.directives.filterNot(schemaDirectives.contains)

    Selection.Field(alias, name, arguments, directives, selectionSet, 0)
  }

  /**
   * The selections of this field's sub-fields, rendering each one with `child` and grouping those with type conditions
   * into inline fragments.
   */
  private[caliban] def selectionSet(child: Field => Selection): List[Selection] = {
    // Not pretty, but it avoids computing the hashmap if it isn't needed
    var map: mutable.Map[String, List[Selection]] =
      null.asInstanceOf[mutable.Map[String, List[Selection]]]

    val children = fields.flatMap { field =>
      val childSelection = child(field)
      field.targets match {
        case Some(targets) =>
          targets.foreach { target =>
            if (map eq null) map = mutable.LinkedHashMap.empty
            map.update(target, childSelection :: map.getOrElse(target, Nil))
          }
          None
        case None          =>
          Some(childSelection)
      }
    }

    val inlineFragments =
      if (map eq null) Nil
      else
        map.map { case (name, selections) =>
          Selection.InlineFragment(
            Some(NamedType(name, nonNull = false)),
            Nil,
            selections
          )
        }

    children ++ inlineFragments
  }
}

object Field {
  def apply(
    selectionSet: List[Selection],
    fragments: Map[String, FragmentDefinition],
    variableValues: Map[String, InputValue],
    variableDefinitions: List[VariableDefinition],
    fieldType: __Type,
    sourceMapper: SourceMapper,
    directives: List[Directive],
    rootType: RootType
  ): Field = {
    val memoizedFragments                         = new mutable.HashMap[(String, List[Directive]), List[(Field, Option[String])]]()
    val variableDefinitionsMap                    =
      if (variableDefinitions eq Nil) Map.empty[String, VariableDefinition]
      else variableDefinitions.map(v => v.name -> v).toMap
    val resolveDirectives: Directive => Directive =
      resolveDirectiveVariables(variableValues, variableDefinitionsMap, rootType)

    def loop(
      selectionSet: List[Selection],
      fieldType: __Type,
      fragment: Option[Fragment],
      targets: Option[Set[String]],
      condition: Option[Set[String]]
    ): Either[List[Field], List[(Field, Option[String])]] = {
      val builder   = FieldBuilder.forSelectionSet(selectionSet)
      val innerType = fieldType.innerType

      selectionSet.foreach {
        case F(alias, name, arguments, directives, selectionSet, index) =>
          val selected = innerType.getFieldOrNull(name)

          val schemaDirectives =
            if ((selected eq null) || selected.directives.isEmpty) Nil
            else selected.directives.get

          val resolvedDirectives =
            (directives ::: schemaDirectives).map(resolveDirectives)

          if (checkDirectives(resolvedDirectives)) {
            // default only case where it's not found is __typename
            val t           = if (selected eq null) Types.string else selected._type
            val definitions = if (selected eq null) Nil else selected.allArgs

            val fields =
              if (selectionSet.nonEmpty)
                loop(selectionSet, t, None, None, None).fold(identityFnList, _.map(_._1))
              else Nil

            builder.addField(
              new Field(
                name,
                t,
                Some(innerType),
                alias,
                fields,
                targets = targets,
                arguments = resolveVariables(arguments, variableDefinitionsMap, variableValues, definitions),
                directives = resolvedDirectives,
                _condition = condition,
                _locationInfo = () => sourceMapper.getLocation(index),
                fragment = fragment
              ),
              None
            )
          }
        case FragmentSpread(name, directives)                           =>
          val fields = memoizedFragments.getOrElseUpdate(
            (name, directives), {
              val resolvedDirectives = directives.map(resolveDirectives)
              val f                  = fragments.getOrElse(name, null)
              if ((f ne null) && checkDirectives(resolvedDirectives)) {
                val typeCondName      = f.typeCondition.name
                val t                 = rootType.types.getOrElse(typeCondName, fieldType)
                val subtypeNames0     = subtypeNames(typeCondName, rootType)
                val isSubsetCondition = subtypeNames0.getOrElse(Set.empty)
                loop(
                  f.selectionSet,
                  t,
                  fragment = Some(Fragment(Some(name), resolvedDirectives)),
                  targets = Some(Set(typeCondName)),
                  condition = subtypeNames0
                ) match {
                  case Left(l)  => l.map((_, Some(typeCondName)))
                  case Right(l) =>
                    l.map {
                      case t @ (_, Some(c)) if isSubsetCondition(c) => t
                      case (f1, _)                                  => (f1, Some(typeCondName))
                    }
                }
              } else Nil
            }
          )
          fields.foreach((builder.addField _).tupled)
        case InlineFragment(typeCondition, directives, selectionSet)    =>
          val resolvedDirectives = directives.map(resolveDirectives)
          if (checkDirectives(resolvedDirectives)) {
            val typeName          = typeCondition.map(_.name)
            val t                 = innerType.possibleTypes
              .flatMap(_.find(_.name.exists(typeName.contains)))
              .orElse(typeName.flatMap(rootType.types.get))
              .getOrElse(fieldType)
            val subtypeNames0     = typeName.flatMap(subtypeNames(_, rootType))
            val isSubsetCondition = subtypeNames0.getOrElse(Set.empty)
            loop(
              selectionSet,
              t,
              fragment = Some(Fragment(None, resolvedDirectives)),
              targets = typeName.map(Set(_)),
              condition = subtypeNames0
            ) match {
              case Left(l)  => l.foreach(builder.addField(_, typeName))
              case Right(l) =>
                l.foreach {
                  case (f, c) if c.exists(isSubsetCondition) => builder.addField(f, c)
                  case (f, _)                                => builder.addField(f, typeName)
                }
            }
          }
      }
      builder.result()
    }

    val fields = loop(selectionSet, fieldType, None, None, None)
    Field("", fieldType, None, fields = fields.fold(identityFnList, _.map(_._1)), directives = directives)
  }

  private val identityFnList: List[Field] => List[Field] = l => l

  private val MaxScannedFields = 16

  /**
   * Folds `f` over `fields` and every field below them, passing each one's depth, starting at one, and the number of
   * selection paths reaching it. A field reached by several paths is visited once per depth, in the order it is first
   * reached.
   */
  private[caliban] def foldShared[A](fields: List[Field], zero: A)(f: (A, Field, Int, Long) => A): A = {
    final class Reached(val field: Field, var paths: Long)
    final class Level {
      val entries = ListBuffer.empty[Reached]
      val reached = new java.util.IdentityHashMap[Field, Reached]()

      def add(field: Field, paths: Long): Unit = {
        val existing = reached.get(field)
        if (existing eq null) {
          val created = new Reached(field, paths)
          reached.put(field, created)
          entries += created
        } else existing.paths = addPaths(existing.paths, paths)
      }
    }

    var level = new Level
    var depth = 1
    var acc   = zero
    fields.foreach(level.add(_, 1L))
    while (level.entries.nonEmpty) {
      val next = new Level
      level.entries.foreach { entry =>
        acc = f(acc, entry.field, depth, entry.paths)
        entry.field.fields.foreach(next.add(_, entry.paths))
      }
      level = next
      depth += 1
    }
    acc
  }

  /**
   * The number of fields below `field` once every fragment spread is expanded, capped at `Long.MaxValue`.
   */
  private[caliban] def expandedCount(field: Field): Long = {
    def loop(children: List[Field], count: Long): Long = children match {
      case child :: rest if count < MaxDirectVisits => loop(rest, loop(child.fields, count + 1))
      case _                                        => count
    }

    val count = loop(field.fields, 0L)
    if (count < MaxDirectVisits) count
    else foldShared(field.fields, 0L)((count, _, _, paths) => addPaths(count, paths))
  }

  /**
   * Fields the direct walks visit, following every path, before switching to `foldShared`.
   */
  private[caliban] val MaxDirectVisits = 1024

  private def addPaths(a: Long, b: Long): Long = {
    val sum = a + b
    if (sum < 0) Long.MaxValue else sum
  }

  /**
   * The fragment of a field selected twice.
   * A field belongs to the earliest payload selecting it: outside deferred fragments, or in the outer one.
   */
  private def earliest(first: Field, second: Field): Option[Fragment] =
    (first.fragment, second.fragment) match {
      case (Some(mine @ Fragment.IsDeferred(_)), Some(theirs @ Fragment.IsDeferred(_))) =>
        if (mine.directives.startsWith(theirs.directives)) second.fragment else first.fragment
      case (Some(Fragment.IsDeferred(_)), _)                                            => second.fragment
      case _                                                                            => first.fragment
    }

  private def resolveDirectiveVariables(
    variableValues: Map[String, InputValue],
    variableDefinitions: Map[String, VariableDefinition],
    rootType: RootType
  )(directive: Directive): Directive =
    if (directive.arguments.isEmpty) directive
    else {
      val args = rootType.additionalDirectives.filter(_.name == directive.name).flatMap(_.allArgs)
      directive.copy(arguments = resolveVariables(directive.arguments, variableDefinitions, variableValues, args))
    }

  private def resolveVariables(
    arguments: Map[String, InputValue],
    variableDefinitions: Map[String, VariableDefinition],
    variableValues: Map[String, InputValue],
    definitions: List[__InputValue]
  ): Map[String, InputValue] = {
    def resolveVariable(value: InputValue): Option[InputValue] =
      value match {
        case InputValue.VariableValue(name) =>
          for {
            definition <- variableDefinitions.get(name)
            value      <- variableValues.get(name).orElse(definition.defaultValue)
          } yield value
        case InputValue.ListValue(values)   =>
          Some(InputValue.ListValue(values.map(v => resolveVariable(v).getOrElse(Value.NullValue))))
        case InputValue.ObjectValue(fields) =>
          Some(InputValue.ObjectValue(fields.flatMap { case (k, v) => resolveVariable(v).map(k -> _) }))
        case value: Value                   =>
          Some(value)
      }
    if (arguments.isEmpty) Map.empty[String, InputValue]
    else
      arguments.flatMap { case (name, value) =>
        resolveVariable(value).map(resolved => name -> coerceNamedArgument(resolved, name, definitions))
      }
  }

  @tailrec
  private def coerceNamedArgument(value: InputValue, name: String, definitions: List[__InputValue]): InputValue =
    definitions match {
      case definition :: others =>
        if (definition.name == name) coerceArgument(value, definition._type)
        else coerceNamedArgument(value, name, others)
      case Nil                  => value
    }

  // Returns the same instance when nothing needs coercion, so that unchanged arguments are not copied.
  private def coerceArgument(value: InputValue, expected: __Type): InputValue =
    expected.kind match {
      case __TypeKind.NON_NULL                               =>
        expected.ofType.fold(value)(coerceArgument(value, _))
      case __TypeKind.LIST                                   =>
        expected.ofType.fold(value) { element =>
          value match {
            case InputValue.ListValue(values) =>
              if (values.exists(v => coerceArgument(v, element) ne v))
                InputValue.ListValue(values.map(coerceArgument(_, element)))
              else value
            case single                       => coerceArgument(single, element)
          }
        }
      case __TypeKind.INPUT_OBJECT                           =>
        value match {
          case InputValue.ObjectValue(fields) =>
            def coerceField(name: String, nested: InputValue): InputValue = {
              val field = expected.getInputFieldOrNull(name)
              if (field eq null) nested else coerceArgument(nested, field._type)
            }
            if (fields.exists { case (name, nested) => coerceField(name, nested) ne nested })
              InputValue.ObjectValue(fields.map { case (name, nested) => name -> coerceField(name, nested) })
            else value
          case other                          => other
        }
      case __TypeKind.SCALAR if expected.name.contains("ID") =>
        value match {
          case int: IntValue => StringValue(int.toBigInt.toString)
          case other         => other
        }
      case _                                                 => value
    }

  private def subtypeNames(typeName: String, rootType: RootType): Option[Set[String]] = {
    def loop(sb: mutable.Builder[String, Set[String]], `type`: Option[__Type]): mutable.Builder[String, Set[String]] =
      `type`.flatMap(_.possibleTypes) match {
        case Some(tpes) =>
          tpes.foreach(_.name.foreach(name => loop(sb += name, rootType.types.get(name))))
          sb
        case _          => sb
      }

    val tpe = rootType.types.get(typeName)
    tpe.map(_ => loop(Set.newBuilder[String] += typeName, tpe).result())
  }

  private[caliban] def checkDirectives(directives: List[Directive]): Boolean =
    directives.isEmpty ||
      (!checkDirective("skip", default = false, directives) &&
        checkDirective("include", default = true, directives))

  private def checkDirective(name: String, default: Boolean, directives: List[Directive]): Boolean =
    directives
      .find(_.name == name)
      .flatMap(_.arguments.get("if")) match {
      case Some(BooleanValue(value)) => value
      case _                         => default
    }

  private abstract class FieldBuilder {
    def addField(f: Field, condition: Option[String]): Unit
    def result(): Either[List[Field], List[(Field, Option[String])]]
  }

  private object FieldBuilder {
    def forSelectionSet(selectionSet: List[Selection]): FieldBuilder =
      if (selectionSet.forall(_.isInstanceOf[F])) new FieldsOnly else new Full

    private final class FieldsOnly extends FieldBuilder {
      private[this] val builder = new ListBuffer[Field]

      def addField(f: Field, condition: Option[String]): Unit          = builder += f
      def result(): Either[List[Field], List[(Field, Option[String])]] = Left(builder.result())
    }

    private final class Full extends FieldBuilder {
      private[this] val map = new java.util.LinkedHashMap[(String, Option[String]), Field]()

      def addField(f: Field, condition: Option[String]): Unit =
        map.compute((f.aliasedName, condition), (_, existing) => if (existing eq null) f else existing.combine(f))

      def result(): Either[List[Field], List[(Field, Option[String])]] = {
        val builder = new ListBuffer[(Field, Option[String])]
        map.forEach { case ((_, cond), field) => builder += ((field, cond)) }
        Right(builder.result())
      }
    }
  }
}
