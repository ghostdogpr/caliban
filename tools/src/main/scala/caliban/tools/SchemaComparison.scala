package caliban.tools

import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.{ DirectiveDefinition, SchemaDefinition, TypeDefinition }
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension.{
  EnumTypeExtension,
  InputObjectTypeExtension,
  InterfaceTypeExtension,
  ObjectTypeExtension,
  ScalarTypeExtension,
  UnionTypeExtension
}
import caliban.parsing.adt.{ Directive, Document }
import caliban.tools.SchemaComparisonChange._
import zio.Task

import scala.reflect.ClassTag

object SchemaComparison {

  def compare(left: SchemaLoader, right: SchemaLoader): Task[List[SchemaComparisonChange]] =
    for {
      docLeft  <- left.load
      docRight <- right.load
    } yield compareDocuments(docLeft, docRight)

  private[caliban] def compareDocuments(left: Document, right: Document): List[SchemaComparisonChange] = {
    val schemaChanges        = compareSchemas(left.schemaDefinition, right.schemaDefinition)
    val typeChanges          = compareAllTypes(
      left.typeDefinitions.map(t => t.name -> t).toMap,
      right.typeDefinitions.map(t => t.name -> t).toMap
    )
    val directiveChanges     = compareAllDirectiveDefinitions(
      left.directiveDefinitions.map(t => t.name -> t).toMap,
      right.directiveDefinitions.map(t => t.name -> t).toMap
    )
    val leftTypeExtensions   = left.definitions.collect { case te: TypeExtension => te }
    val rightTypeExtensions  = right.definitions.collect { case te: TypeExtension => te }
    val typeExtensionChanges = compareAllTypeExtensions(
      leftTypeExtensions,
      rightTypeExtensions
    )
    schemaChanges ++ typeChanges ++ directiveChanges ++ typeExtensionChanges
  }

  private def compareEnumValues(
    typeName: String,
    left: EnumValueDefinition,
    right: EnumValueDefinition
  ): List[SchemaComparisonChange] = {
    val enumTarget         = Target.EnumValue(left.enumValue, typeName)
    val descriptionChanges =
      compareDescriptions(left.description, right.description, enumTarget)
    val directiveChanges   = compareAllDirectives(left.directives, right.directives, enumTarget)

    descriptionChanges ++ directiveChanges
  }

  private def compareAllEnumValues(
    typeName: String,
    leftEnums: List[EnumValueDefinition],
    rightEnums: List[EnumValueDefinition]
  ): List[SchemaComparisonChange] = {
    val left        = leftEnums.map(d => d.enumValue -> d).toMap
    val right       = rightEnums.map(d => d.enumValue -> d).toMap
    val leftKeys    = left.keySet
    val rightKeys   = right.keySet
    val added       = (rightKeys -- leftKeys).map(EnumValueAdded(typeName, _)).toList
    val deleted     = (leftKeys -- rightKeys).map(EnumValueDeleted(typeName, _)).toList
    val commonTypes = leftKeys intersect rightKeys
    val changes     = commonTypes.toList.flatMap(key => compareEnumValues(typeName, left(key), right(key)))

    added ++ deleted ++ changes
  }

  private def compareEnums(left: EnumTypeDefinition, right: EnumTypeDefinition): List[SchemaComparisonChange] =
    compareAllEnumValues(left.name, left.enumValuesDefinition, right.enumValuesDefinition)

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
      if (left.name == right.name && left.arguments == right.arguments) Nil
      else List(DirectiveArgumentChanged(left.name, key, left.arguments(key), right.arguments(key), target))
    )

    added ++ deleted ++ changes
  }

  private def compareAllDirectives(
    leftDirectives: List[Directive],
    rightDirectives: List[Directive],
    target: Target
  ): List[SchemaComparisonChange] = {
    val left         = leftDirectives.map(d => d.name -> d).toMap
    val right        = rightDirectives.map(d => d.name -> d).toMap
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
    val directiveChanges   = compareAllDirectives(left.directives, right.directives, argTarget)
    val ofTypeChanges      =
      if (left.ofType != right.ofType) List(TypeChanged(left.ofType, right.ofType, argTarget)) else Nil

    // default values are not supported so far
    descriptionChanges ++ directiveChanges ++ ofTypeChanges
  }

  private def compareArguments(
    leftArguments: List[InputValueDefinition],
    rightArguments: List[InputValueDefinition],
    target: Target
  ): List[SchemaComparisonChange] = {
    val left         = leftArguments.map(a => a.name -> a).toMap
    val right        = rightArguments.map(a => a.name -> a).toMap
    val leftKeys     = left.keySet
    val rightKeys    = right.keySet
    val added        = (right -- leftKeys).map { case (name, arg) =>
      ArgumentAdded(name, target, arg.ofType.nullable || arg.defaultValue.isDefined)
    }.toList
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
    val directiveChanges   = compareAllDirectives(left.directives, right.directives, fieldTarget)
    val ofTypeChanges      =
      if (left.ofType != right.ofType) List(TypeChanged(left.ofType, right.ofType, fieldTarget)) else Nil
    val argumentChanges    = compareArguments(left.args, right.args, fieldTarget)

    descriptionChanges ++ directiveChanges ++ ofTypeChanges ++ argumentChanges
  }

  private def compareAllFields(
    typeName: String,
    leftFields: List[FieldDefinition],
    rightFields: List[FieldDefinition]
  ): List[SchemaComparisonChange] = {
    val left  = leftFields.map(d => d.name -> d).toMap
    val right = rightFields.map(d => d.name -> d).toMap

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
    val rightImplements   = right.implements.toSet
    val implementsAdded   =
      (rightImplements -- leftImplements).map(name => ObjectImplementsAdded(left.name, name.name)).toList
    val implementsDeleted =
      (leftImplements -- rightImplements).map(name => ObjectImplementsDeleted(left.name, name.name)).toList

    val fieldChanges =
      compareAllFields(left.name, left.fields, right.fields)

    implementsAdded ++ implementsDeleted ++ fieldChanges
  }

  private def compareInputObjects(
    left: InputObjectTypeDefinition,
    right: InputObjectTypeDefinition
  ): List[SchemaComparisonChange] =
    compareArguments(left.fields, right.fields, Target.Type(left.name))

  private def compareInterfaces(
    left: InterfaceTypeDefinition,
    right: InterfaceTypeDefinition
  ): List[SchemaComparisonChange] =
    compareAllFields(left.name, left.fields, right.fields)

  private def compareTypes(left: TypeDefinition, right: TypeDefinition): List[SchemaComparisonChange] = {
    val typeTarget         = Target.Type(left.name)
    val descriptionChanges = compareDescriptions(left.description, right.description, typeTarget)
    val directiveChanges   = compareAllDirectives(left.directives, right.directives, typeTarget)

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
    val added       = (rightKeys -- leftKeys).map(TypeAdded.apply).toList
    val deleted     = (leftKeys -- rightKeys).map(TypeDeleted.apply).toList
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
      compareArguments(left.args, right.args, target)

    val repeatableChanges = if (left.isRepeatable == right.isRepeatable) {
      Nil
    } else {
      List(DirectiveDefinitionRepeatableChanged(left.name, left.isRepeatable, right.isRepeatable))
    }

    val locationAdded   =
      (right.locations -- left.locations).map(l => DirectiveLocationAdded(l, left.name)).toList
    val locationDeleted =
      (left.locations -- right.locations).map(l => DirectiveLocationDeleted(l, left.name)).toList

    descriptionChanges ++ argChanges ++ repeatableChanges ++ locationAdded ++ locationDeleted
  }

  private def compareAllDirectiveDefinitions(
    left: Map[String, DirectiveDefinition],
    right: Map[String, DirectiveDefinition]
  ): List[SchemaComparisonChange] = {
    val leftKeys         = left.keySet
    val rightKeys        = right.keySet
    val added            = (rightKeys -- leftKeys).map(DirectiveDefinitionAdded.apply).toList
    val deleted          = (leftKeys -- rightKeys).map(DirectiveDefinitionDeleted.apply).toList
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

  private def compareAllTypeExtensions(
    left: List[TypeExtension],
    right: List[TypeExtension]
  ): List[SchemaComparisonChange] = {
    val scalarTypeExtensionChanges    =
      compareTypeExtensionsOfType[ScalarTypeExtension](left, right, _.name, compareScalarTypeExtensions)
    val objectTypeExtensionChanges    =
      compareTypeExtensionsOfType[ObjectTypeExtension](left, right, _.name, compareObjectTypeExtensions)
    val interfaceTypeExtensionChanges =
      compareTypeExtensionsOfType[InterfaceTypeExtension](left, right, _.name, compareInterfaceTypeExtensions)
    val unionTypeExtensionChanges     =
      compareTypeExtensionsOfType[UnionTypeExtension](left, right, _.name, compareUnionTypeExtensions)
    val enumTypeExtensionChanges      =
      compareTypeExtensionsOfType[EnumTypeExtension](left, right, _.name, compareEnumTypeExtensions)
    val inputObjectTypeExtensions     =
      compareTypeExtensionsOfType[InputObjectTypeExtension](left, right, _.name, compareInputObjectTypeExtensions)
    scalarTypeExtensionChanges ++ objectTypeExtensionChanges ++ interfaceTypeExtensionChanges ++ unionTypeExtensionChanges ++ enumTypeExtensionChanges ++ inputObjectTypeExtensions
  }

  private def compareTypeExtensionsOfType[T: ClassTag](
    left: List[TypeExtension],
    right: List[TypeExtension],
    name: T => String,
    comp: (T, T) => List[SchemaComparisonChange]
  ): List[SchemaComparisonChange] =
    compareTypeExtensions(
      left.collect { case ote: T => ote }.map(t => name(t) -> t).toMap,
      right.collect { case ote: T => ote }.map(t => name(t) -> t).toMap,
      comp
    )

  private def compareTypeExtensions[T](
    left: Map[String, T],
    right: Map[String, T],
    comp: (T, T) => List[SchemaComparisonChange]
  ): List[SchemaComparisonChange] = {
    val added            = (right.keySet -- left.keySet).map(TypeExtensionAdded.apply).toList
    val deleted          = (left.keySet -- right.keySet).map(TypeExtensionDeleted.apply).toList
    val commonExtensions = left.keySet intersect right.keySet
    val changes          = commonExtensions.toList.flatMap(key => comp(left(key), right(key)))
    added ++ deleted ++ changes
  }

  private def compareScalarTypeExtensions(
    left: ScalarTypeExtension,
    right: ScalarTypeExtension
  ): List[SchemaComparisonChange] =
    compareAllDirectives(left.directives, right.directives, Target.Type(left.name))

  private def compareObjectTypeExtensions(
    left: ObjectTypeExtension,
    right: ObjectTypeExtension
  ): List[SchemaComparisonChange] = {
    val directiveChanges = compareAllDirectives(left.directives, right.directives, Target.Type(left.name))

    val leftImplements    = left.implements.toSet
    val rightImplements   = right.implements.toSet
    val implementsAdded   =
      (rightImplements -- leftImplements).map(name => ObjectImplementsAdded(left.name, name.name)).toList
    val implementsDeleted =
      (leftImplements -- rightImplements).map(name => ObjectImplementsDeleted(left.name, name.name)).toList

    val fieldChanges = compareAllFields(left.name, left.fields, right.fields)

    directiveChanges ++ implementsAdded ++ implementsDeleted ++ fieldChanges
  }

  private def compareInterfaceTypeExtensions(
    left: InterfaceTypeExtension,
    right: InterfaceTypeExtension
  ): List[SchemaComparisonChange] = {
    val directiveChanges = compareAllDirectives(left.directives, right.directives, Target.Type(left.name))
    val fieldChanges     = compareAllFields(left.name, left.fields, right.fields)
    directiveChanges ++ fieldChanges
  }

  private def compareUnionTypeExtensions(
    left: UnionTypeExtension,
    right: UnionTypeExtension
  ): List[SchemaComparisonChange] = {
    val directiveChanges = compareAllDirectives(left.directives, right.directives, Target.Type(left.name))
    val leftKeys         = left.memberTypes.toSet
    val rightKeys        = right.memberTypes.toSet
    val added            = (rightKeys -- leftKeys).map(UnionMemberAdded(left.name, _)).toList
    val deleted          = (leftKeys -- rightKeys).map(UnionMemberDeleted(left.name, _)).toList
    added ++ deleted ++ directiveChanges
  }

  private def compareEnumTypeExtensions(
    left: EnumTypeExtension,
    right: EnumTypeExtension
  ): List[SchemaComparisonChange] = {
    val directiveChanges = compareAllDirectives(left.directives, right.directives, Target.Type(left.name))
    val enumChanges      = compareAllEnumValues(left.name, left.enumValuesDefinition, right.enumValuesDefinition)
    directiveChanges ++ enumChanges
  }

  private def compareInputObjectTypeExtensions(
    left: InputObjectTypeExtension,
    right: InputObjectTypeExtension
  ): List[SchemaComparisonChange] = {
    val targetType       = Target.Type(left.name)
    val directiveChanges = compareAllDirectives(left.directives, right.directives, targetType)
    val argumentChanges  = compareArguments(left.fields, right.fields, targetType)
    directiveChanges ++ argumentChanges
  }
}
