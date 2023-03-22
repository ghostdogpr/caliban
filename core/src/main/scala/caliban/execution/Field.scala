package caliban.execution

import caliban.Value.BooleanValue
import caliban.introspection.adt.{ __DeprecatedArgs, __Type }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.Definition.ExecutableDefinition.FragmentDefinition
import caliban.parsing.adt.Selection.{ Field => F, FragmentSpread, InlineFragment }
import caliban.parsing.adt.{ Directive, LocationInfo, Selection, VariableDefinition }
import caliban.schema.{ RootType, Types }
import caliban.{ InputValue, Value }

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
  _locationInfo: () => LocationInfo = () => LocationInfo.origin
) { self =>
  lazy val locationInfo: LocationInfo = _locationInfo()

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
    val memoizedFragments = collection.mutable.Map.empty[FragmentSpread, (List[Field], Option[String])]

    def loop(selectionSet: List[Selection], fieldType: __Type): List[Field] = {
      val map = new java.util.LinkedHashMap[(String, String), Field](selectionSet.length)

      def addField(f: Field, condition: Option[String]): Unit = {
        val name = f.alias.getOrElse(f.name)
        val key  = (name, condition.getOrElse(""))
        map.compute(key, (_, existing) => if (existing == null) f else existing.combine(f))
      }

      val innerType            = Types.innerType(fieldType)
      lazy val innerTypeFields = innerType.fields(__DeprecatedArgs(Some(true)))

      selectionSet.foreach {
        case F(alias, name, arguments, directives, selectionSet, index) =>
          val selected = innerTypeFields.flatMap(_.find(_.name == name))

          val schemaDirectives   = selected.flatMap(_.directives).getOrElse(Nil)
          val resolvedDirectives =
            (directives ::: schemaDirectives).map(resolveDirectiveVariables(variableValues, variableDefinitions))

          if (checkDirectives(resolvedDirectives)) {
            val t = selected.fold(Types.string)(_.`type`()) // default only case where it's not found is __typename

            val fields = if (selectionSet.nonEmpty) loop(selectionSet, t) else Nil

            addField(
              Field(
                name,
                t,
                Some(innerType),
                alias,
                fields,
                None,
                resolveVariables(arguments, variableDefinitions, variableValues),
                resolvedDirectives,
                None,
                () => sourceMapper.getLocation(index)
              ),
              None
            )
          }
        case fr @ FragmentSpread(name, directives)                      =>
          val (fields, condition) = memoizedFragments.getOrElseUpdate(
            fr, {
              val resolvedDirectives = directives.map(resolveDirectiveVariables(variableValues, variableDefinitions))

              val _fields = if (checkDirectives(resolvedDirectives)) {
                fragments.get(name).map { f =>
                  val t = rootType.types.getOrElse(f.typeCondition.name, fieldType)
                  loop(f.selectionSet, t).map { field =>
                    if (field._condition.isDefined) field
                    else
                      field.copy(
                        targets = Some(Set(f.typeCondition.name)),
                        _condition = subtypeNames(f.typeCondition.name, rootType)
                      )
                  } -> Some(f.typeCondition.name)
                }
              } else None
              _fields.getOrElse(Nil -> None)
            }
          )
          fields.foreach(addField(_, condition))
        case InlineFragment(typeCondition, directives, selectionSet)    =>
          val resolvedDirectives = directives.map(resolveDirectiveVariables(variableValues, variableDefinitions))

          if (checkDirectives(resolvedDirectives)) {
            val t      = innerType.possibleTypes
              .flatMap(_.find(_.name.exists(typeCondition.map(_.name).contains)))
              .orElse(typeCondition.flatMap(typeName => rootType.types.get(typeName.name)))
              .getOrElse(fieldType)
            val fields = loop(selectionSet, t)
            typeCondition match {
              case None           => fields.map(addField(_, None))
              case Some(typeName) =>
                fields.foreach { field =>
                  val _field =
                    if (field._condition.isDefined) field
                    else
                      field.copy(
                        targets = typeCondition.map(t => Set(t.name)),
                        _condition = subtypeNames(typeName.name, rootType)
                      )
                  addField(_field, Some(typeName.name))
                }
            }
          }
      }
      map.values().asScala.toList
    }

    val fields = loop(selectionSet, fieldType)
    Field("", fieldType, None, fields = fields, directives = directives)
  }

  private def resolveDirectiveVariables(
    variableValues: Map[String, InputValue],
    variableDefinitions: List[VariableDefinition]
  )(directive: Directive): Directive =
    if (directive.arguments.isEmpty) directive
    else directive.copy(arguments = resolveVariables(directive.arguments, variableDefinitions, variableValues))

  private def resolveVariables(
    arguments: Map[String, InputValue],
    variableDefinitions: List[VariableDefinition],
    variableValues: Map[String, InputValue]
  ): Map[String, InputValue] = {
    def resolveVariable(value: InputValue): Option[InputValue] =
      value match {
        case InputValue.ListValue(values)   =>
          Some(InputValue.ListValue(values.flatMap(resolveVariable)))
        case InputValue.ObjectValue(fields) =>
          Some(InputValue.ObjectValue(fields.flatMap { case (k, v) => resolveVariable(v).map(k -> _) }))
        case InputValue.VariableValue(name) =>
          for {
            definition <- variableDefinitions.find(_.name == name)
            value      <- variableValues.get(name).orElse(definition.defaultValue)
          } yield value
        case value: Value                   =>
          Some(value)
      }
    arguments.flatMap { case (k, v) => resolveVariable(v).map(k -> _) }
  }

  private def subtypeNames(typeName: String, rootType: RootType): Option[Set[String]] =
    rootType.types
      .get(typeName)
      .map(t =>
        t.possibleTypes
          .fold(Set.empty[String])(
            _.map(_.name.map(subtypeNames(_, rootType).getOrElse(Set.empty))).toSet.flatten.flatten
          ) + typeName
      )

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
