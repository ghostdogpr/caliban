package caliban.execution

import caliban.Value.BooleanValue
import caliban.introspection.adt.__Type
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.FragmentDefinition
import caliban.parsing.adt.Selection.{ Field => F, FragmentSpread, InlineFragment }
import caliban.parsing.adt.Type.NamedType
import caliban.parsing.adt.{ Directive, LocationInfo, Selection, VariableDefinition }
import caliban.schema.{ RootType, Types }
import caliban.{ InputValue, Value }

import scala.collection.mutable
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

  private[caliban] lazy val fieldNames: Set[String] =
    fields.foldLeft(Set.newBuilder[String]) { case (sb, f) => sb += f.name }.result()

  private[caliban] val aliasedName: String = alias.getOrElse(name)

  def combine(other: Field): Field =
    self.copy(
      fields = self.fields ::: other.fields,
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
      }
    )

  lazy val toSelection: Selection = {
    def loop(f: Field): Selection = {
      // Not pretty, but it avoids computing the hashmap if it isn't needed
      var map: mutable.Map[String, List[Selection]] =
        null.asInstanceOf[mutable.Map[String, List[Selection]]]

      val children = f.fields.flatMap { child =>
        val childSelection = loop(child)
        child.targets match {
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

      Selection.Field(
        f.alias,
        f.name,
        f.arguments,
        f.directives,
        children ++ inlineFragments,
        0
      )
    }

    loop(this)
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
    val memoizedFragments      = new mutable.HashMap[String, (List[Field], Option[String])]()
    val variableDefinitionsMap = variableDefinitions.map(v => v.name -> v).toMap

    def loop(selectionSet: List[Selection], fieldType: __Type, fragment: Option[Fragment]): List[Field] = {
      val map = new java.util.LinkedHashMap[(String, Option[String]), Field]()

      def addField(f: Field, condition: Option[String]): Unit =
        map.compute((f.aliasedName, condition), (_, existing) => if (existing == null) f else existing.combine(f))

      val innerType = fieldType.innerType

      selectionSet.foreach {
        case F(alias, name, arguments, directives, selectionSet, index) =>
          val selected = innerType.allFieldsMap.get(name)

          val schemaDirectives   = selected.flatMap(_.directives).getOrElse(Nil)
          val resolvedDirectives =
            (directives ::: schemaDirectives).map(resolveDirectiveVariables(variableValues, variableDefinitionsMap))

          if (checkDirectives(resolvedDirectives)) {
            val t = selected.fold(Types.string)(_._type) // default only case where it's not found is __typename

            val fields =
              if (selectionSet.nonEmpty) loop(selectionSet, t, None)
              else Nil // Fragments apply on to the direct children of the fragment spread

            addField(
              Field(
                name,
                t,
                Some(innerType),
                alias,
                fields,
                None,
                resolveVariables(arguments, variableDefinitionsMap, variableValues),
                resolvedDirectives,
                None,
                () => sourceMapper.getLocation(index),
                fragment
              ),
              None
            )
          }
        case FragmentSpread(name, directives)                           =>
          val (fields, condition) = memoizedFragments.getOrElseUpdate(
            name, {
              val resolvedDirectives = directives.map(resolveDirectiveVariables(variableValues, variableDefinitionsMap))

              val _fields = if (checkDirectives(resolvedDirectives)) {
                fragments.get(name).map { f =>
                  val t          = rootType.types.getOrElse(f.typeCondition.name, fieldType)
                  val _targets   = Some(Set(f.typeCondition.name))
                  val _condition = subtypeNames(f.typeCondition.name, rootType)

                  loop(f.selectionSet, t, Some(Fragment(Some(name), resolvedDirectives))).map { field =>
                    if (field._condition.isDefined) field
                    else field.copy(targets = _targets, _condition = _condition)
                  } -> Some(f.typeCondition.name)
                }
              } else None
              _fields.getOrElse(Nil -> None)
            }
          )
          fields.foreach(addField(_, condition))
        case InlineFragment(typeCondition, directives, selectionSet)    =>
          val resolvedDirectives = directives.map(resolveDirectiveVariables(variableValues, variableDefinitionsMap))

          if (checkDirectives(resolvedDirectives)) {
            val t      = innerType.possibleTypes
              .flatMap(_.find(_.name.exists(typeCondition.map(_.name).contains)))
              .orElse(typeCondition.flatMap(typeName => rootType.types.get(typeName.name)))
              .getOrElse(fieldType)
            val fields = loop(selectionSet, t, Some(Fragment(None, resolvedDirectives)))
            typeCondition match {
              case None           => fields.map(addField(_, None))
              case Some(typeName) =>
                val _targets   = Some(Set(typeName.name))
                val _condition = subtypeNames(typeName.name, rootType)
                fields.foreach { field =>
                  val _field =
                    if (field._condition.isDefined) field
                    else field.copy(targets = _targets, _condition = _condition)
                  addField(_field, Some(typeName.name))
                }
            }
          }
      }
      map.values().asScala.toList
    }

    val fields = loop(selectionSet, fieldType, None)
    Field("", fieldType, None, fields = fields, directives = directives)
  }

  private def resolveDirectiveVariables(
    variableValues: Map[String, InputValue],
    variableDefinitions: Map[String, VariableDefinition]
  )(directive: Directive): Directive =
    if (directive.arguments.isEmpty) directive
    else directive.copy(arguments = resolveVariables(directive.arguments, variableDefinitions, variableValues))

  private def resolveVariables(
    arguments: Map[String, InputValue],
    variableDefinitions: Map[String, VariableDefinition],
    variableValues: Map[String, InputValue]
  ): Map[String, InputValue] = {
    def resolveVariable(value: InputValue): Option[InputValue] =
      value match {
        case InputValue.VariableValue(name) =>
          for {
            definition <- variableDefinitions.get(name)
            value      <- variableValues.get(name).orElse(definition.defaultValue)
          } yield value
        case InputValue.ListValue(values)   =>
          Some(InputValue.ListValue(values.flatMap(resolveVariable)))
        case InputValue.ObjectValue(fields) =>
          Some(InputValue.ObjectValue(fields.flatMap { case (k, v) => resolveVariable(v).map(k -> _) }))
        case value: Value                   =>
          Some(value)
      }
    if (arguments.isEmpty) Map.empty[String, InputValue]
    else arguments.flatMap { case (k, v) => resolveVariable(v).map(k -> _) }
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

  private def checkDirectives(directives: List[Directive]): Boolean =
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
}
