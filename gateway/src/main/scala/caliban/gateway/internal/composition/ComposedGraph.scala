package caliban.gateway.internal.composition

import caliban.gateway.{ fieldDefinition, innerParentTypeName, isCompositeType, responseNames }
import caliban.InputValue
import caliban.execution.Field
import caliban.gateway.internal.PrivateAliases
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.DirectiveComposition.FieldCoordinate
import caliban.introspection.adt._
import caliban.parsing.adt.{ OperationType, Selection }
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType

import scala.collection.compat._

/**
 * Immutable schema and ownership metadata produced by composition.
 */
private[gateway] final case class ComposedGraph private[internal] (
  val rootType: RootType,
  val fieldRoutes: Map[FieldCoordinate, List[Source]],
  val progressiveRoutes: Map[FieldCoordinate, ProgressiveRoute],
  val sources: List[Source],
  val possibleTypesByName: Map[String, Set[String]],
  val costMetadata: CostMetadata,
  val securityApplications: List[SecurityDirectiveApplication]
) {
  def resolveOverrides(activeOverrides: Set[OverrideLabel]): ComposedGraph =
    copy(
      fieldRoutes = fieldRoutes ++ progressiveRoutes.collect {
        case (coordinate, route) if activeOverrides(route.label) => coordinate -> route.sources
      },
      progressiveRoutes = Map.empty
    )

  def hasProgressiveOverrides: Boolean = progressiveRoutes.nonEmpty

  def progressiveOverrides(fieldNames: Set[String]): Set[OverrideLabel] =
    progressiveRoutes.collect { case (FieldCoordinate(_, field), route) if fieldNames(field) => route.label }.toSet

  def operationRoot(operation: OperationType): Option[__Type] = rootType.types.get(rootName(operation))

  def rootFieldSources(operation: OperationType, field: String): List[Source] = {
    val sources   = fieldRoutes.getOrElse(FieldCoordinate(rootName(operation), field), Nil)
    val composite = operationRoot(operation)
      .flatMap(fieldDefinition(_, field))
      .exists(definition => isCompositeType(definition._type.innerType))
    if (composite) sources else sources.take(1)
  }

  def fieldSources(typeName: String, field: String, preferred: Source): List[Source] =
    preferredFirst(fieldRoutes.getOrElse(FieldCoordinate(typeName, field), Nil))(_ == preferred)

  def interfaceObjectFieldSources(typeName: String, field: String, preferred: Source): List[SourceType] =
    if (fieldRoutes.contains(FieldCoordinate(typeName, field))) Nil
    else
      preferredFirst(interfaceObjects(typeName).filter(key => ownsField(key.source, key.typeName, field)))(
        _.source == preferred
      )

  // Entity lookups allow switching sources. Otherwise, keep the current sources when any can resolve the field.
  def candidateSources(current: Set[Source], parentType: String, field: String): Set[Source] = {
    val routed = fieldRoutes.getOrElse(FieldCoordinate(parentType, field), Nil).toSet
    if (routed.isEmpty) current
    else if (sources.exists(_.entityLookups(parentType).nonEmpty)) routed
    else {
      val constrained = current intersect routed
      if (constrained.isEmpty) routed else constrained
    }
  }

  def typenameSource(typeName: String, preferred: Source): Option[Source] =
    preferredFirst(
      sources.filter(source => source.entityLookups(typeName).nonEmpty && !source.isInterfaceObject(typeName))
    )(
      _ == preferred
    ).headOption

  def sourcesDefiningKey(typeName: String, fields: List[KeyField]): List[Source] =
    sources.filter(source => fields.forall(source.definesKeyField(typeName, _)))

  def ownsField(source: Source, typeName: String, field: String): Boolean =
    fieldRoutes.getOrElse(FieldCoordinate(typeName, field), Nil).contains(source)

  val hasContextArguments: Boolean = sources.exists(_.contexts.contextBindings.nonEmpty)

  def contextDeclarations(typeName: String): List[(Source, ContextDeclaration)] = {
    val selectedTypes = possibleTypesByName.getOrElse(typeName, Set(typeName))
    sources.flatMap(source => source.contexts.declarations.map(source -> _)).filter { case (_, declared) =>
      possibleTypesByName.getOrElse(declared.typeName, Set(declared.typeName)).exists(selectedTypes)
    }
  }

  def possibleReturnTypes(
    possibleSources: Set[Source],
    source: Source,
    parentType: String,
    field: String,
    outputType: String
  ): Set[String] = {
    val candidates =
      if (possibleSources.nonEmpty) possibleSources.iterator
      else Iterator.single(source)
    val available  = candidates.flatMap { candidate =>
      candidate
        .fieldTypeName(parentType, field)
        .map(candidate.possibleTypes)
        .filter(_.nonEmpty)
    }
    // A selection planned across candidate sources can rely only on concrete return types shared by them.
    available
      .reduceOption(_ intersect _)
      .getOrElse(source.possibleTypes(outputType))
  }

  def interfaceObjectTypes(source: Source, typeName: String): List[__Type] =
    interfaceObjects(typeName).collect { case SourceType(`source`, name) => name }.flatMap(rootType.types.get)

  def isObjectType(typeName: String): Boolean =
    rootType.types.get(typeName).exists(_.kind == __TypeKind.OBJECT)

  def acceptsRuntimeType(typeName: String, runtimeType: String): Boolean =
    runtimeType == typeName || possibleTypesByName.getOrElse(typeName, Set.empty).contains(runtimeType)

  private def interfaceObjects(typeName: String): List[SourceType] = {
    val interfaces = rootType.types.get(typeName).filter(_.kind == __TypeKind.OBJECT).toList
    val names      = interfaces.flatMap(_.interfaces().getOrElse(Nil).flatMap(_.name)).sorted
    sources.flatMap(source => names.filter(source.isInterfaceObject).map(SourceType(source, _)))
  }

  private def preferredFirst[A](values: List[A])(preferred: A => Boolean): List[A] = {
    val (first, rest) = values.partition(preferred)
    first ::: rest
  }
}

private[gateway] object ComposedGraph {
  def rootName(operation: OperationType): String =
    operation match {
      case OperationType.Query        => "Query"
      case OperationType.Mutation     => "Mutation"
      case OperationType.Subscription => "Subscription"
    }

  /**
   * One composed subgraph: its schema mapping and the per-type metadata planning and execution read from it.
   */
  final class Source private[composition] (
    val name: String,
    val mapping: SchemaMapping,
    types: Map[String, __Type],
    entityLookupsByType: Map[String, List[EntityLookup]],
    interfaceObjects: Set[String],
    private[composition] val requiredFieldSets: Map[FieldCoordinate, List[Selection]],
    providedFieldSets: Map[FieldCoordinate, List[Selection]],
    private[composition] val contexts: ContextCompilation.FederationContexts
  ) {
    override def toString: String = name

    def entityLookups(typeName: String): List[EntityLookup] = entityLookupsByType.getOrElse(typeName, Nil)

    def isInterfaceObject(typeName: String): Boolean = interfaceObjects.contains(typeName)

    def sourceField(typeName: String, field: String): Option[__Field] =
      types.get(typeName).flatMap(fieldDefinition(_, field))

    def fieldTypeName(typeName: String, field: String): Option[String] =
      sourceField(typeName, field).flatMap(_._type.innerType.name)

    def definesKeyField(typeName: String, key: KeyField): Boolean =
      sourceField(typeName, key.name).exists { definition =>
        key.children.isEmpty || definition._type.innerType.name.exists(name =>
          key.children.forall(definesKeyField(name, _))
        )
      }

    def requiredFieldSet(typeName: String, field: String): List[Selection] =
      requiredFieldSets.getOrElse(FieldCoordinate(typeName, field), Nil)

    def providedFieldSet(typeName: String, field: String): List[Selection] =
      providedFieldSets.getOrElse(FieldCoordinate(typeName, field), Nil)

    def contextArguments(typeName: String, field: String): List[ContextArgument] =
      contexts.contextBindings.getOrElse(FieldCoordinate(typeName, field), Nil)

    def possibleTypes(typeName: String): Set[String] =
      types.get(typeName).fold(Set.empty[String])(_.possibleTypeNames)

    def fieldApplies(parentType: String, field: Field): Boolean =
      field._condition.forall(possibleTypes(parentType).exists(_))

    def prepareField(field: Field): Field =
      prepareField(None, field)

    def prepareEntityFields(entityType: String, fields: List[Field]): List[Field] =
      aliasConflicts(fields.map(prepareField(Some(entityType), _)))

    private def prepareField(parentType: Option[String], field: Field): Field = {
      val parent      = parentType.getOrElse(innerParentTypeName(field))
      // An @interfaceObject source sees one object, so fragments targeting concrete implementations must be removed.
      val targets     =
        if (isInterfaceObject(parent)) None
        else
          field.targets.map(original =>
            field._condition.fold(original)(
              _.filter(possibleTypes(parent)).filter(name => possibleTypes(name).contains(name))
            )
          )
      val childParent = fieldTypeName(parent, field.name).orElse(field.fieldType.innerType.name)
      val children    = aliasConflicts(field.fields.map(prepareField(childParent, _)))
      field.copy(targets = targets, fields = children)
    }

    private def aliasConflicts(fields: List[Field]): List[Field] = {
      def typeSignatures(field: Field): Set[String] = {
        val sourceDefinitions = field.targets.iterator
          .flatMap(_.iterator)
          .flatMap(target => sourceField(target, field.name))
          .map(value => DocumentRenderer.renderTypeName(value._type))
          .toSet
        if (sourceDefinitions.nonEmpty) sourceDefinitions else Set(DocumentRenderer.renderTypeName(field.fieldType))
      }

      val conflicts = fields
        .groupBy(_.aliasedName)
        .collect {
          case (name, values) if values.iterator.flatMap(typeSignatures).toSet.size > 1 => name
        }
        .toSet
      if (conflicts.isEmpty) fields
      else {
        val aliases = new PrivateAliases(responseNames(fields))
        fields.map { field =>
          if (!conflicts.contains(field.aliasedName)) field
          else field.copy(alias = Some(aliases.next(s"_caliban_gateway_${field.aliasedName}")))
        }
      }
    }
  }

  object Source {
    implicit val ordering: Ordering[Source] = Ordering.by(_.name)
  }

  final case class SourceType(source: Source, typeName: String)
  final case class SourceField(source: String, typeName: String, fieldName: String)

  final case class CostMetadata(
    types: Map[String, BigInt] = Map.empty,
    weights: Map[DirectiveComposition.Coordinate, BigInt] = Map.empty,
    listSizes: Map[SourceField, ListSize] = Map.empty
  )

  final case class ListSize(
    assumedSize: Option[BigInt],
    slicingArguments: List[SlicingArgument],
    sizedFields: List[Vector[String]],
    requireOneSlicingArgument: Boolean
  )

  final case class SlicingArgument(path: ::[String], defaultValue: Option[InputValue], listValued: Boolean)

  final case class KeyField(name: String, children: List[KeyField])

  final case class ContextName(value: String) extends AnyVal

  final case class ContextDeclaration(typeName: String, name: ContextName)

  final case class ContextArgument(argument: String, context: ContextName, selections: List[Selection])

  sealed trait OverrideLabel { def value: String }
  object OverrideLabel       {
    final case class Percent(value: String, percentage: BigDecimal) extends OverrideLabel
    final case class Custom(value: String)                          extends OverrideLabel
  }

  // Sources of a field while its progressive override label is active.
  final case class ProgressiveRoute(label: OverrideLabel, sources: List[Source])

  private[internal] final case class SecurityDirectiveApplication(
    source: String,
    typeName: String,
    fieldName: Option[String],
    member: FederationCompilation.FederationDirective.Security,
    groups: List[List[String]]
  ) {
    val coordinate: String                 = fieldName.fold(typeName)(name => s"$typeName.$name")
    val directiveName: String              = s"@${member.name}"
    val scopes: Option[List[List[String]]] =
      if (member == FederationCompilation.FederationDirective.Policy) None else Some(groups)
  }

  private[internal] object SecurityDirectiveApplication {

    def conjunction(expressions: List[List[List[String]]]): List[Set[String]] =
      expressions.foldLeft(List(Set.empty[String])) { (acc, expression) =>
        val normalized = if (expression.isEmpty) List(Nil) else expression
        val combined   = for {
          left  <- acc
          right <- normalized
        } yield left ++ right
        combined.distinct.filterNot(candidate =>
          combined.exists(other => other != candidate && other.subsetOf(candidate))
        )
      }
  }

  final case class EntityLookup(key: List[KeyField], operation: LookupOperation)

  sealed trait LookupOperation

  object LookupOperation {
    case object FederationEntities extends LookupOperation

    final case class GraphQLQuery(
      field: String,
      arguments: Map[String, LookupArgument],
      result: LookupResult
    ) extends LookupOperation
  }

  sealed trait LookupArgument

  object LookupArgument {
    final case class Key(field: String, expectedType: __Type)              extends LookupArgument
    final case class ObjectMapping(fields: List[(String, LookupArgument)]) extends LookupArgument
    final case class Batch(value: LookupArgument)                          extends LookupArgument
  }

  sealed trait LookupResult

  object LookupResult {
    case object Single extends LookupResult

    case object ByKey extends LookupResult
  }

}
