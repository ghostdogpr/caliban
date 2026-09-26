package caliban.gateway.internal.composition

import caliban.gateway.{ innerParentTypeName, responseNames }
import caliban.InputValue
import caliban.execution.Field
import caliban.gateway.PhaseHooks.SecurityDirective
import caliban.gateway.internal.PrivateAliases
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.introspection.adt._
import caliban.parsing.adt.{ Directive, OperationType, Selection }
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.compat._

/**
 * Immutable schema and ownership metadata produced by composition.
 */
private[gateway] final case class ComposedGraph private[internal] (
  val rootType: RootType,
  val possibleTypesByName: Map[String, Set[String]],
  val rootRoutes: Map[RootField, RootRoute],
  val fieldRoutes: Map[TypeField, List[FieldRoute]],
  private val sourceFields: Map[SourceField, __Field],
  private val entityLookupsByType: Map[SourceType, List[EntityLookup]],
  private val requiredFieldSets: Map[SourceField, List[Selection]],
  private val providedFieldSets: Map[SourceField, List[Selection]],
  private val declaredContexts: Map[SourceType, Set[ContextName]],
  private val contextBindings: Map[SourceField, List[ContextArgument]],
  private val interfaceObjects: Set[SourceType],
  private val sourcePossibleTypes: Map[SourceType, Set[String]],
  private val schemaMappings: Map[String, SchemaMapping],
  val costMetadata: CostMetadata,
  val securityApplications: List[SecurityDirectiveApplication],
  val schemaDirectives: List[Directive]
) {
  def resolveOverrides(activeOverrides: Set[OverrideLabel]): ComposedGraph = {
    lazy val created = graphForOverrides(activeOverrides)

    @tailrec def cacheOrGet(): ComposedGraph = {
      val current = cachedOverrideGraphs.get()
      current.find(_._1 == activeOverrides) match {
        case Some((_, graph)) => graph
        case None             =>
          val updated = (activeOverrides -> created) :: current.take(MaxCachedOverrideGraphs - 1)
          if (cachedOverrideGraphs.compareAndSet(current, updated)) created else cacheOrGet()
      }
    }

    cacheOrGet()
  }

  def hasProgressiveOverrides: Boolean = overridePercentages.nonEmpty

  def progressiveOverrides(fieldNames: Set[String]): Map[OverrideLabel, Option[BigDecimal]] =
    fieldNames.iterator
      .flatMap(overrideLabelsByField.get)
      .flatten
      .flatMap(label => overridePercentages.get(label).map(label -> _))
      .toMap

  def rootFieldSources(operation: OperationType, field: String): List[String] =
    rootRoutes
      .get(RootField(operation, field))
      .toList
      .flatMap { route =>
        val selected = if (route.selectFirst) route.candidates.take(1) else route.candidates
        selected.map(_.source)
      }

  def fieldSources(typeName: String, field: String, preferred: String): List[String] =
    preferredFirst(fieldRoutes.getOrElse(TypeField(typeName, field), Nil).map(_.source))(_ == preferred)

  def interfaceObjectFieldSources(typeName: String, field: String, preferred: String): List[(String, String)] =
    preferredFirst(inheritedFieldSources.getOrElse(TypeField(typeName, field), Nil))(_._1 == preferred)

  // Entity lookups allow switching sources. Otherwise, keep the current sources when any can resolve the field.
  def candidateSources(current: Set[String], parentType: String, field: String): Set[String] = {
    val routed = fieldRoutes.getOrElse(TypeField(parentType, field), Nil).map(_.source).toSet
    if (routed.isEmpty) current
    else if (lookupTypes.contains(parentType)) routed
    else {
      val constrained = current intersect routed
      if (constrained.isEmpty) routed else constrained
    }
  }

  def typenameSource(typeName: String, preferred: String): Option[String] =
    preferredFirst(lookupSourcesByType.getOrElse(typeName, Nil))(_ == preferred).headOption

  def entityLookups(source: String, typeName: String): List[EntityLookup] =
    entityLookupsByType.getOrElse(SourceType(source, typeName), Nil)

  def sourcesDefiningKey(typeName: String, fields: List[KeyField]): List[String] =
    fields match {
      case Nil       => Nil
      case head :: _ =>
        declaringSources
          .getOrElse(TypeField(typeName, head.name), Nil)
          .filter(source => fields.forall(definesKeyField(source, typeName, _)))
    }

  def definesKeyField(source: String, typeName: String, field: KeyField): Boolean =
    sourceFields.get(SourceField(source, typeName, field.name)).exists { definition =>
      field.children.isEmpty || definition._type.innerType.name.exists(name =>
        field.children.forall(definesKeyField(source, name, _))
      )
    }

  def ownsField(source: String, typeName: String, field: String): Boolean =
    fieldRoutes
      .getOrElse(TypeField(typeName, field), Nil)
      .exists(_.source == source)

  def sourceField(source: String, typeName: String, field: String): Option[__Field] =
    sourceFields.get(SourceField(source, typeName, field))

  def sourceFieldTypeName(source: String, typeName: String, field: String): Option[String] =
    sourceField(source, typeName, field).flatMap(_._type.innerType.name)

  def requiredFieldSet(source: String, typeName: String, field: String): List[Selection] =
    requiredFieldSets.getOrElse(SourceField(source, typeName, field), Nil)

  def providedFieldSet(source: String, typeName: String, field: String): List[Selection] =
    providedFieldSets.getOrElse(SourceField(source, typeName, field), Nil)

  def schemaMapping(source: String): SchemaMapping =
    schemaMappings(source)

  def subgraphCount: Int = schemaMappings.size

  def hasContextArguments: Boolean = contextBindings.nonEmpty

  def contextDeclarations(typeName: String): List[ContextDeclaration] = {
    val selectedTypes = possibleTypesByName.getOrElse(typeName, Set(typeName))
    declaredContexts.iterator.flatMap { case (SourceType(source, declaredType), names) =>
      val declaredTypes = possibleTypesByName.getOrElse(declaredType, Set(declaredType))
      if (selectedTypes.exists(declaredTypes))
        names.iterator.map(ContextDeclaration(source, declaredType, _))
      else Iterator.empty
    }.toList
  }

  def contextArguments(source: String, typeName: String, field: String): List[ContextArgument] =
    contextBindings.getOrElse(SourceField(source, typeName, field), Nil)

  def possibleTypes(source: String, typeName: String): List[String] =
    sourcePossibleTypes.getOrElse(SourceType(source, typeName), Set.empty).toList.sorted

  def possibleReturnTypes(
    sources: Set[String],
    source: String,
    parentType: String,
    field: String,
    outputType: String
  ): Set[String] = {
    val candidates =
      if (sources.nonEmpty) sources.iterator
      else Iterator.single(source)
    val available  = candidates.flatMap { candidate =>
      sourceFieldTypeName(candidate, parentType, field)
        .map(name => sourcePossibleTypes.getOrElse(SourceType(candidate, name), Set.empty))
        .filter(_.nonEmpty)
    }
    // A selection planned across candidate sources can rely only on concrete return types shared by them.
    available
      .reduceOption(_ intersect _)
      .getOrElse(sourcePossibleTypes.getOrElse(SourceType(source, outputType), Set.empty))
  }

  def interfaceObjectTypes(source: String, typeName: String): List[__Type] =
    interfaceObjectsByType
      .getOrElse(typeName, Nil)
      .collect { case (`source`, interfaceName) => rootType.types.get(interfaceName) }
      .flatten

  def isInterfaceObject(source: String, typeName: String): Boolean =
    interfaceObjects.contains(SourceType(source, typeName))

  def isObjectType(typeName: String): Boolean =
    rootType.types.get(typeName).exists(_.kind == __TypeKind.OBJECT)

  def acceptsRuntimeType(typeName: String, runtimeType: String): Boolean =
    runtimeType == typeName || possibleTypesByName.getOrElse(typeName, Set.empty).contains(runtimeType)

  def fieldApplies(source: String, parentType: String, field: Field): Boolean =
    field._condition.forall(sourcePossibleTypes.getOrElse(SourceType(source, parentType), Set.empty).exists(_))

  def prepareField(source: String, field: Field): Field =
    prepareField(source, None, field)

  def prepareEntityFields(source: String, entityType: String, fields: List[Field]): List[Field] =
    aliasConflicts(source, fields.map(prepareField(source, Some(entityType), _)))

  private val declaringSources       = sourceFields.keysIterator.map { case SourceField(source, owner, name) =>
    TypeField(owner, name) -> source
  }.toList
    .groupMap(_._1)(_._2)
    .map { case (coordinate, values) => coordinate -> values.sorted }
  private val lookupSourcesByType    = entityLookupsByType.keysIterator.collect {
    case SourceType(source, typeName) if !interfaceObjects.contains(SourceType(source, typeName)) => typeName -> source
  }.toList
    .groupMap(_._1)(_._2)
    .map { case (typeName, values) => typeName -> values.sorted }
  private val lookupTypes            = entityLookupsByType.keysIterator.map(_.typeName).toSet
  private val interfaceObjectsByType = rootType.types.iterator.flatMap { case (typeName, tpe) =>
    val interfaces = if (tpe.kind == __TypeKind.OBJECT) tpe.interfaces().getOrElse(Nil).flatMap(_.name) else Nil
    val inherited  = interfaceObjects.filter(key => interfaces.contains(key.typeName))
    if (inherited.isEmpty) Iterator.empty
    else Iterator.single(typeName -> inherited.toList.map(key => key.source -> key.typeName).sorted)
  }.toMap
  private val inheritedFieldSources  = {
    val fieldsByType = fieldRoutes.keys.toList.groupMap(_.typeName)(_.fieldName)
    interfaceObjectsByType.iterator.flatMap { case (typeName, inherited) =>
      inherited.iterator.flatMap { case (source, interfaceName) =>
        fieldsByType.getOrElse(interfaceName, Nil).iterator.collect {
          case field if !fieldRoutes.contains(TypeField(typeName, field)) && ownsField(source, interfaceName, field) =>
            TypeField(typeName, field) -> (source -> interfaceName)
        }
      }
    }.toList.groupMap(_._1)(_._2)
  }

  private val (overridePercentages, overrideLabelsByField) = {
    val conditions = (rootRoutes.iterator.flatMap { case (RootField(_, field), route) =>
      route.candidates.iterator.flatMap(_.condition.map(field -> _))
    } ++ fieldRoutes.iterator.flatMap { case (TypeField(_, field), routes) =>
      routes.iterator.flatMap(_.condition.map(field -> _))
    }).toList
    conditions.map { case (_, condition) => condition.label -> condition.percentage }.toMap ->
      conditions.groupMap(_._1)(_._2.label).map { case (field, labels) => field -> labels.toSet }
  }
  private val cachedOverrideGraphs                         = new AtomicReference[List[(Set[OverrideLabel], ComposedGraph)]](Nil)

  private def graphForOverrides(activeOverrides: Set[OverrideLabel]): ComposedGraph =
    copy(
      rootRoutes = rootRoutes.map { case (coordinate, route) =>
        coordinate -> route.copy(
          candidates = route.candidates.filter(_.isEnabled(activeOverrides)).map(_.copy(condition = None))
        )
      },
      fieldRoutes = fieldRoutes.map { case (coordinate, routes) =>
        coordinate -> routes.filter(_.isEnabled(activeOverrides)).map(_.copy(condition = None))
      }
    )

  private def isSourceObjectType(source: String, typeName: String): Boolean =
    sourcePossibleTypes.get(SourceType(source, typeName)).exists(_.contains(typeName))

  private def prepareField(source: String, parentType: Option[String], field: Field): Field = {
    val parent      = parentType.getOrElse(innerParentTypeName(field))
    // An @interfaceObject source sees one object, so fragments targeting concrete implementations must be removed.
    val targets     =
      if (isInterfaceObject(source, parent)) None
      else
        field.targets.map(original =>
          field._condition.fold(original)(
            _.filter(sourcePossibleTypes.getOrElse(SourceType(source, parent), Set.empty))
              .filter(isSourceObjectType(source, _))
          )
        )
    val childParent = sourceFieldTypeName(source, parent, field.name).orElse(field.fieldType.innerType.name)
    val children    = aliasConflicts(source, field.fields.map(prepareField(source, childParent, _)))
    field.copy(targets = targets, fields = children)
  }

  private def aliasConflicts(source: String, fields: List[Field]): List[Field] = {
    def typeSignatures(field: Field): Set[String] = {
      val sourceDefinitions = field.targets.iterator
        .flatMap(_.iterator)
        .flatMap(target => sourceField(source, target, field.name))
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

  private def preferredFirst[A](values: List[A])(preferred: A => Boolean): List[A] = {
    val (first, rest) = values.partition(preferred)
    first ::: rest
  }
}

private[gateway] object ComposedGraph {
  final case class RootField(operation: OperationType, fieldName: String)
  final case class TypeField(typeName: String, fieldName: String)
  final case class SourceType(source: String, typeName: String)
  final case class SourceField(source: String, typeName: String, fieldName: String)
  final case class FieldArgument(typeName: String, fieldName: String, argumentName: String)

  final case class CostMetadata(
    types: Map[String, BigInt],
    fields: Map[TypeField, BigInt],
    arguments: Map[FieldArgument, BigInt],
    inputFields: Map[TypeField, BigInt],
    listSizes: Map[SourceField, ListSize]
  )

  final case class ListSize(
    assumedSize: Option[BigInt],
    slicingArguments: List[SlicingArgument],
    sizedFields: List[Vector[String]],
    requireOneSlicingArgument: Boolean
  )

  final case class SlicingArgument(path: Vector[String], defaultValue: Option[InputValue], listValued: Boolean)

  final case class KeyField(name: String, children: List[KeyField])

  final case class ContextName(value: String) extends AnyVal

  final case class ContextDeclaration(source: String, typeName: String, name: ContextName)

  final case class ContextArgument(argument: String, context: ContextName, selections: List[Selection])

  final case class OverrideLabel(value: String) extends AnyVal

  final case class ProgressiveOverride(label: OverrideLabel, percentage: Option[BigDecimal])

  final case class OverrideCondition(label: OverrideLabel, percentage: Option[BigDecimal], active: Boolean) {
    def isEnabled(activeOverrides: Set[OverrideLabel]): Boolean = activeOverrides.contains(label) == active
  }

  final case class FieldRoute(source: String, condition: Option[OverrideCondition] = None) {
    def isEnabled(activeOverrides: Set[OverrideLabel]): Boolean = condition.forall(_.isEnabled(activeOverrides))
  }

  final case class RootRoute(candidates: List[FieldRoute], selectFirst: Boolean)

  private[internal] final case class SecurityDirectiveApplication(
    source: String,
    typeName: String,
    fieldName: Option[String],
    directive: SecurityDirective,
    policies: List[List[String]]
  ) {
    val coordinate: String    = fieldName.fold(typeName)(name => s"$typeName.$name")
    val directiveName: String = directive match {
      case SecurityDirective.UnsupportedPolicy => "@policy"
      case SecurityDirective.Authenticated     => "@authenticated"
      case _: SecurityDirective.RequiresScopes => "@requiresScopes"
    }
  }

  final case class EntityLookup(
    key: List[KeyField],
    operation: LookupOperation,
    representationType: Option[String] = None
  )

  sealed trait LookupOperation {
    def requiresTypename: Boolean
  }

  object LookupOperation {
    case object FederationEntities extends LookupOperation {
      val requiresTypename: Boolean = true
    }

    final case class GraphQLQuery(
      field: String,
      arguments: Map[String, LookupArgument],
      result: LookupResult
    ) extends LookupOperation {
      val requiresTypename: Boolean = false
    }
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

    final case class ByKey(fields: Map[String, String]) extends LookupResult
  }

  private val MaxCachedOverrideGraphs = 16
}
