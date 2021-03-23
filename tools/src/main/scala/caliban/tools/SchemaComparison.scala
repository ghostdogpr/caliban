package caliban.tools

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.{ DirectiveDefinition, SchemaDefinition, TypeDefinition }
import caliban.parsing.adt.{ Directive, Document }
import caliban.tools.SchemaComparisonChange._
import zio.Task

object SchemaComparison {

  def compare(left: SchemaLoader, right: SchemaLoader): Task[List[SchemaComparisonChange]] =
    for {
      docLeft  <- left.load
      docRight <- right.load
    } yield compareDocuments(docLeft, docRight)

  private[caliban] def compareDocuments(left: Document, right: Document): List[SchemaComparisonChange] = {
    val schemaChanges    = compareSchemas(left.schemaDefinition, right.schemaDefinition)
    val typeChanges      = compareAllTypes(
      left.typeDefinitions.map(t => t.name -> t).toMap,
      right.typeDefinitions.map(t => t.name -> t).toMap
    )
    val directiveChanges = compareAllDirectiveDefinitions(
      left.directiveDefinitions.map(t => t.name -> t).toMap,
      right.directiveDefinitions.map(t => t.name -> t).toMap
    )
    schemaChanges ++ typeChanges ++ directiveChanges
  }

  private def compareEnumValues(
    typeName: String,
    left: EnumValueDefinition,
    right: EnumValueDefinition
  ): List[SchemaComparisonChange] = {
    val enumTarget         = Target.EnumValue(left.enumValue, typeName)
    val descriptionChanges =
      compareDescriptions(left.description, right.description, enumTarget)
    val directiveChanges   = compareAllDirectives(
      left.directives.map(d => d.name -> d).toMap,
      right.directives.map(d => d.name -> d).toMap,
      enumTarget
    )

    descriptionChanges ++ directiveChanges
  }

  private def compareAllEnumValues(
    typeName: String,
    left: Map[String, EnumValueDefinition],
    right: Map[String, EnumValueDefinition]
  ): List[SchemaComparisonChange] = {
    val leftKeys    = left.keySet
    val rightKeys   = right.keySet
    val added       = (rightKeys -- leftKeys).map(EnumValueAdded(typeName, _)).toList
    val deleted     = (leftKeys -- rightKeys).map(EnumValueDeleted(typeName, _)).toList
    val commonTypes = leftKeys intersect rightKeys
    val changes     = commonTypes.toList.flatMap(key => compareEnumValues(typeName, left(key), right(key)))

    added ++ deleted ++ changes
  }

  private def compareEnums(left: EnumTypeDefinition, right: EnumTypeDefinition): List[SchemaComparisonChange] =
    compareAllEnumValues(
      left.name,
      left.enumValuesDefinition.map(d => d.enumValue -> d).toMap,
      right.enumValuesDefinition.map(d => d.enumValue -> d).toMap
    )

  private def compareUnions(left: UnionTypeDefinition, right: UnionTypeDefinition): List[SchemaComparisonChange] = {
    val leftKeys  = left.memberTypes.toSet
    val rightKeys = right.memberTypes.toSet
    val added     = (rightKeys -- leftKeys).map(UnionMemberAdded(left.name, _)).toList
    val deleted   = (leftKeys -- rightKeys).map(UnionMemberDeleted(left.name, _)).toList

    added ++ deleted
  }

  private def compareDirectives(
    left: Directive,
    right: Directive,
    target: Target
  ): List[SchemaComparisonChange] = {
    val leftKeys     = left.arguments.keySet
    val rightKeys    = right.arguments.keySet
    val added        = (rightKeys -- leftKeys).map(DirectiveArgumentAdded(left.name, _, target)).toList
    val deleted      = (leftKeys -- rightKeys).map(DirectiveArgumentDeleted(left.name, _, target)).toList
    val commonFields = leftKeys intersect rightKeys
    val changes      = commonFields.toList.flatMap(key =>
      if (left == right) Nil
      else List(DirectiveArgumentChanged(left.name, key, left.arguments(key), right.arguments(key), target))
    )

    added ++ deleted ++ changes
  }

  private def compareAllDirectives(
    left: Map[String, Directive],
    right: Map[String, Directive],
    target: Target
  ): List[SchemaComparisonChange] = {
    val leftKeys     = left.keySet
    val rightKeys    = right.keySet
    val added        = (rightKeys -- leftKeys).map(DirectiveAdded(_, target)).toList
    val deleted      = (leftKeys -- rightKeys).map(DirectiveDeleted(_, target)).toList
    val commonFields = leftKeys intersect rightKeys
    val changes      = commonFields.toList.flatMap(key => compareDirectives(left(key), right(key), target))

    added ++ deleted ++ changes
  }

  private def compareArgumentDefinition(
    left: InputValueDefinition,
    right: InputValueDefinition,
    target: Target
  ): List[SchemaComparisonChange] = {
    val argTarget          = target match {
      case Target.Type(name) => Target.Field(left.name, name) // input object => InputValueDefinition is a field
      case _                 => Target.Argument(left.name, target)
    }
    val descriptionChanges = compareDescriptions(left.description, right.description, argTarget)
    val directiveChanges   = compareAllDirectives(
      left.directives.map(d => d.name -> d).toMap,
      right.directives.map(d => d.name -> d).toMap,
      argTarget
    )
    val ofTypeChanges      =
      if (left.ofType != right.ofType) List(TypeChanged(left.ofType, right.ofType, argTarget)) else Nil

    // default values are not supported so far
    descriptionChanges ++ directiveChanges ++ ofTypeChanges
  }

  private def compareArguments(
    left: Map[String, InputValueDefinition],
    right: Map[String, InputValueDefinition],
    target: Target
  ): List[SchemaComparisonChange] = {
    val leftKeys     = left.keySet
    val rightKeys    = right.keySet
    val added        = (rightKeys -- leftKeys).map(ArgumentAdded(_, target)).toList
    val deleted      = (leftKeys -- rightKeys).map(ArgumentDeleted(_, target)).toList
    val commonFields = leftKeys intersect rightKeys
    val changes      = commonFields.toList.flatMap(key => compareArgumentDefinition(left(key), right(key), target))

    added ++ deleted ++ changes
  }

  private def compareDescriptions(
    left: Option[String],
    right: Option[String],
    target: Target
  ): List[SchemaComparisonChange] =
    (left, right) match {
      case (Some(_), None)              => List(DescriptionDeleted(target))
      case (None, Some(_))              => List(DescriptionAdded(target))
      case (Some(l), Some(r)) if l != r => List(DescriptionChanged(target))
      case _                            => Nil
    }

  private def compareFields(
    typeName: String,
    left: FieldDefinition,
    right: FieldDefinition
  ): List[SchemaComparisonChange] = {
    val fieldTarget        = Target.Field(left.name, typeName)
    val descriptionChanges = compareDescriptions(left.description, right.description, fieldTarget)
    val directiveChanges   = compareAllDirectives(
      left.directives.map(d => d.name -> d).toMap,
      right.directives.map(d => d.name -> d).toMap,
      fieldTarget
    )
    val ofTypeChanges      =
      if (left.ofType != right.ofType) List(TypeChanged(left.ofType, right.ofType, fieldTarget)) else Nil

    val argumentChanges =
      compareArguments(
        left.args.map(a => a.name -> a).toMap,
        right.args.map(a => a.name -> a).toMap,
        fieldTarget
      )

    descriptionChanges ++ directiveChanges ++ ofTypeChanges ++ argumentChanges
  }

  private def compareAllFields(
    typeName: String,
    left: Map[String, FieldDefinition],
    right: Map[String, FieldDefinition]
  ): List[SchemaComparisonChange] = {
    val leftKeys    = left.keySet
    val rightKeys   = right.keySet
    val added       = (rightKeys -- leftKeys).map(FieldAdded(typeName, _)).toList
    val deleted     = (leftKeys -- rightKeys).map(FieldDeleted(typeName, _)).toList
    val commonTypes = leftKeys intersect rightKeys
    val changes     = commonTypes.toList.flatMap(key => compareFields(typeName, left(key), right(key)))

    added ++ deleted ++ changes
  }

  private def compareObjects(left: ObjectTypeDefinition, right: ObjectTypeDefinition): List[SchemaComparisonChange] = {
    val leftImplements    = left.implements.toSet
    val rightImplements   = left.implements.toSet
    val implementsAdded   =
      (rightImplements -- leftImplements).map(name => ObjectImplementsAdded(left.name, name.name)).toList
    val implementsDeleted =
      (leftImplements -- rightImplements).map(name => ObjectImplementsDeleted(left.name, name.name)).toList

    val fieldChanges =
      compareAllFields(left.name, left.fields.map(f => f.name -> f).toMap, right.fields.map(f => f.name -> f).toMap)

    implementsAdded ++ implementsDeleted ++ fieldChanges
  }

  private def compareInputObjects(
    left: InputObjectTypeDefinition,
    right: InputObjectTypeDefinition
  ): List[SchemaComparisonChange] =
    compareArguments(
      left.fields.map(f => f.name -> f).toMap,
      right.fields.map(f => f.name -> f).toMap,
      Target.Type(left.name)
    )

  private def compareInterfaces(
    left: InterfaceTypeDefinition,
    right: InterfaceTypeDefinition
  ): List[SchemaComparisonChange] =
    compareAllFields(left.name, left.fields.map(f => f.name -> f).toMap, right.fields.map(f => f.name -> f).toMap)

  private def compareTypes(left: TypeDefinition, right: TypeDefinition): List[SchemaComparisonChange] = {
    val typeTarget         = Target.Type(left.name)
    val descriptionChanges = compareDescriptions(left.description, right.description, typeTarget)
    val directiveChanges   = compareAllDirectives(
      left.directives.map(d => d.name -> d).toMap,
      right.directives.map(d => d.name -> d).toMap,
      typeTarget
    )

    val typeChanges =
      (left, right) match {
        case (_: ScalarTypeDefinition, _: ScalarTypeDefinition)           => Nil
        case (l: EnumTypeDefinition, r: EnumTypeDefinition)               => compareEnums(l, r)
        case (l: UnionTypeDefinition, r: UnionTypeDefinition)             => compareUnions(l, r)
        case (l: ObjectTypeDefinition, r: ObjectTypeDefinition)           => compareObjects(l, r)
        case (l: InterfaceTypeDefinition, r: InterfaceTypeDefinition)     => compareInterfaces(l, r)
        case (l: InputObjectTypeDefinition, r: InputObjectTypeDefinition) => compareInputObjects(l, r)
        case (l, r)                                                       => List(TypeKindChanged(left.name, l.toString, r.toString))
      }

    descriptionChanges ++ directiveChanges ++ typeChanges
  }

  private def isBuiltinScalar(name: String): Boolean =
    name == "Int" || name == "Float" || name == "String" || name == "Boolean" || name == "ID"

  private def compareAllTypes(
    left: Map[String, TypeDefinition],
    right: Map[String, TypeDefinition]
  ): List[SchemaComparisonChange] = {
    val leftKeys    = left.keySet.filterNot(isBuiltinScalar)
    val rightKeys   = right.keySet.filterNot(isBuiltinScalar)
    val added       = (rightKeys -- leftKeys).map(TypeAdded).toList
    val deleted     = (leftKeys -- rightKeys).map(TypeDeleted).toList
    val commonTypes = leftKeys intersect rightKeys
    val changes     = commonTypes.toList.flatMap(key => compareTypes(left(key), right(key)))

    added ++ deleted ++ changes
  }

  private def compareDirectiveDefinitions(
    left: DirectiveDefinition,
    right: DirectiveDefinition
  ): List[SchemaComparisonChange] = {
    val target             = Target.Directive(left.name)
    val descriptionChanges = compareDescriptions(left.description, right.description, target)
    val argChanges         =
      compareArguments(left.args.map(a => a.name -> a).toMap, right.args.map(a => a.name -> a).toMap, target)

    val locationAdded      =
      (right.locations -- left.locations).map(l => DirectiveLocationAdded(l, left.name)).toList
    val locationDeleted    =
      (left.locations -- right.locations).map(l => DirectiveLocationDeleted(l, left.name)).toList

    descriptionChanges ++ argChanges ++ locationAdded ++ locationDeleted
  }

  private def compareAllDirectiveDefinitions(
    left: Map[String, DirectiveDefinition],
    right: Map[String, DirectiveDefinition]
  ): List[SchemaComparisonChange] = {
    val leftKeys         = left.keySet
    val rightKeys        = right.keySet
    val added            = (rightKeys -- leftKeys).map(DirectiveDefinitionAdded).toList
    val deleted          = (leftKeys -- rightKeys).map(DirectiveDefinitionDeleted).toList
    val commonDirectives = leftKeys intersect rightKeys
    val changes          = commonDirectives.toList.flatMap(key => compareDirectiveDefinitions(left(key), right(key)))

    added ++ deleted ++ changes
  }

  private def compareSchemas(
    left: Option[SchemaDefinition],
    right: Option[SchemaDefinition]
  ): List[SchemaComparisonChange] =
    (((left.flatMap(_.query), right.flatMap(_.query)) match {
      case (None, Some("Query")) | (Some("Query"), None) => None
      case (l, r) if l != r                              => Some(SchemaQueryTypeChanged(l, r))
      case _                                             => None
    }) :: ((left.flatMap(_.mutation), right.flatMap(_.mutation)) match {
      case (None, Some("Mutation")) | (Some("Mutation"), None) => None
      case (l, r) if l != r                                    => Some(SchemaMutationTypeChanged(l, r))
      case _                                                   => None
    }) :: ((left.flatMap(_.subscription), right.flatMap(_.subscription)) match {
      case (None, Some("Subscription")) | (Some("Subscription"), None) => None
      case (l, r) if l != r                                            => Some(SchemaSubscriptionTypeChanged(l, r))
      case _                                                           => None
    }) :: Nil).flatten
}
