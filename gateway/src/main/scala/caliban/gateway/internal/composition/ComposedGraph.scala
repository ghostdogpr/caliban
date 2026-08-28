package caliban.gateway.internal.composition

import caliban.execution.{ isMetaField, Field }
import caliban.gateway.OperationPolicy.{ SecurityDirective, SecurityRequirement }
import caliban.gateway.internal.composition.ComposedGraph.{ LookupOperation, LookupResult }
import caliban.introspection.adt._
import caliban.gateway.internal.planning.OperationPlan
import caliban.parsing.adt.{ Directive, OperationType, Selection }
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType

/**
 * Immutable schema and ownership metadata produced by composition.
 */
private[gateway] final class ComposedGraph private[internal] (
  val rootType: RootType,
  private val runtimeTypesByName: Map[String, Set[String]],
  private val routes: Map[(OperationType, String), List[String]],
  private val fieldRoutes: Map[(String, String), List[String]],
  private val sourceFields: Map[(String, String, String), __Field],
  private val entityLookups: Map[(String, String), List[ComposedGraph.EntityLookup]],
  private val requirements: Map[(String, String, String), List[Selection]],
  private val provisions: Map[(String, String, String), List[Selection]],
  private val interfaceObjects: Set[(String, String)],
  private val sourceRuntimeTypes: Map[(String, String), Set[String]],
  private[internal] val mappings: Map[String, SchemaMapping],
  private val securityApplications: List[ComposedGraph.SecurityApplication],
  private[gateway] val schemaDirectives: List[Directive]
) {
  private val hasUnsupportedPolicies   = securityApplications.exists(_.directive == SecurityDirective.UnsupportedPolicy)
  private val requirementsByCoordinate =
    if (!hasUnsupportedPolicies) Map.empty[(String, String), List[(String, List[Selection])]]
    else
      requirements.toList.map { case ((source, owner, name), selections) => (owner -> name) -> (source -> selections) }
        .groupBy(_._1)
        .map { case (coordinate, values) => coordinate -> values.map(_._2) }
  private val securityByCoordinate     =
    securityApplications
      .groupBy(application => application.typeName -> application.fieldName)
      .map { case (coordinate, values) => coordinate -> values.map(_.directive).distinct }
  private val securedFieldTypes        = securityApplications.iterator
    .flatMap(application => application.fieldName.map(_ -> application.typeName))
    .toList
    .groupBy(_._1)
    .map { case (fieldName, values) => fieldName -> values.map(_._2).distinct.sorted }
  private val securedTypes             = securityApplications.collect {
    case application if application.fieldName.isEmpty =>
      application.typeName
  }.distinct.sorted
  private val sourceFieldSources       = sourceFields.keysIterator.map { case (source, owner, name) =>
    (owner -> name) -> source
  }.toList
    .groupBy(_._1)
    .map { case (coordinate, values) => coordinate -> values.map(_._2).distinct.sorted }
  private val lookupSourcesByType      = entityLookups.keysIterator.collect {
    case (source, typeName) if !interfaceObjects.contains(source -> typeName) => typeName -> source
  }.toList
    .groupBy(_._1)
    .map { case (typeName, values) => typeName -> values.map(_._2).distinct.sorted }
  private val lookupTypes              = entityLookups.keysIterator.map(_._2).toSet

  def sources(operation: OperationType, field: String): List[String] =
    routes.getOrElse(operation -> field, Nil)

  def fieldSources(typeName: String, field: String, preferred: String): List[String] =
    fieldRoutes.getOrElse(typeName -> field, Nil) match {
      case sources if sources.contains(preferred) => preferred :: sources.filterNot(_ == preferred)
      case sources                                => sources
    }

  def lookups(source: String, typeName: String): List[ComposedGraph.EntityLookup] =
    entityLookups.getOrElse(source -> typeName, Nil)

  def owns(source: String, typeName: String, field: String): Boolean =
    fieldRoutes.getOrElse(typeName -> field, Nil).contains(source)

  def declares(source: String, typeName: String, field: String): Boolean =
    sourceFields.contains((source, typeName, field))

  def field(source: String, typeName: String, field: String): Option[__Field] =
    sourceFields.get((source, typeName, field))

  def required(source: String, typeName: String, field: String): List[Selection] =
    requirements.getOrElse((source, typeName, field), Nil)

  def provided(source: String, typeName: String, field: String): List[Selection] =
    provisions.getOrElse((source, typeName, field), Nil)

  def mapping(source: String): Option[SchemaMapping] =
    mappings.get(source)

  def hasSecurityRequirements: Boolean = securityApplications.exists(_.directive != SecurityDirective.UnsupportedPolicy)

  def securityPolicyDiagnostics: List[String] =
    securityApplications
      .filterNot(_.directive == SecurityDirective.UnsupportedPolicy)
      .map(application =>
        s"[${application.source}] Federation ${application.directiveName} at '${application.coordinate}' requires an operation policy."
      )
      .distinct
      .sorted

  def securityRequirements(plan: OperationPlan): List[SecurityRequirement] =
    if (securityApplications.isEmpty) Nil
    else {
      def runtimeTypes(typeName: String): Set[String] =
        runtimeTypesByName.getOrElse(typeName, Set.empty)

      def overlaps(selectedType: String, candidateType: String, selection: Option[Set[String]]): Boolean =
        selection.getOrElse(runtimeTypes(selectedType)).exists(runtimeTypes(candidateType))

      def selected(typeName: String, fieldName: Option[String]): List[SecurityRequirement] = {
        val values = securityByCoordinate.getOrElse(typeName -> fieldName, Nil)
        if (values.isEmpty) Nil else SecurityRequirement(typeName, fieldName, values) :: Nil
      }

      // Composition validates these field sets against source definitions, including hidden fields and roots.
      def requiredFields(source: String, parent: String, selections: List[Selection]): List[Field] =
        selections.flatMap {
          case field: Selection.Field             =>
            sourceFields.get((source, parent, field.name)).toList.map { definition =>
              Field(
                field.name,
                definition._type,
                Some(__Type(kind = __TypeKind.OBJECT, name = Some(parent))),
                fields = requiredFields(source, definition._type.innerType.name.getOrElse(""), field.selectionSet)
              )
            }
          case fragment: Selection.InlineFragment =>
            requiredFields(source, fragment.typeCondition.fold(parent)(_.name), fragment.selectionSet)
          case _: Selection.FragmentSpread        => Nil
        }

      def loop(
        fields: List[Field],
        root: Boolean,
        visited: Set[(String, String)] = Set.empty
      ): List[SecurityRequirement] =
        fields.flatMap { field =>
          if (isMetaField(field)) Nil
          else {
            val parentType       = field.parentType.flatMap(_.innerType.name).getOrElse("")
            val outputType       = field.fieldType.innerType.name.getOrElse("")
            val direct           = selected(parentType, Some(field.name))
            val rootRequirements = if (root) selected(parentType, None) else Nil
            val relatedFields    = securedFieldTypes.getOrElse(field.name, Nil).flatMap { typeName =>
              if (typeName != parentType && overlaps(parentType, typeName, field._condition))
                selected(typeName, Some(field.name))
              else Nil
            }
            val output           = selected(outputType, None)
            val relatedOutput    = securedTypes.flatMap { typeName =>
              if (typeName != outputType && overlaps(outputType, typeName, None)) selected(typeName, None)
              else Nil
            }
            // Only @policy expands runtime checks to implicit dependencies. Auth/scopes retain their existing
            // client-selection checks and composition-time @requires checks; injected keys add neither.
            val dependencies     =
              if (!hasUnsupportedPolicies) Nil
              else {
                val coordinate = parentType -> field.name
                if (visited(coordinate)) Nil
                else
                  requirementsByCoordinate.getOrElse(coordinate, Nil).flatMap { case (source, selections) =>
                    loop(requiredFields(source, parentType, selections), root = false, visited + coordinate)
                      .filter(_.directives.contains(SecurityDirective.UnsupportedPolicy))
                  }
              }
            rootRequirements ::: direct ::: relatedFields ::: output ::: relatedOutput ::: dependencies ::: loop(
              field.fields,
              root = false,
              visited
            )
          }
        }

      val requested = loop(plan.fields, root = true)
      // Include implicit fetches and the lookup roots/correlation fields generated later by EntityLookup.
      val injected  =
        if (!hasUnsupportedPolicies) Nil
        else {
          def generatedFields(source: String, parent: String, names: List[String]): List[Field] =
            requiredFields(source, parent, names.map(name => Selection.Field(None, name, Map.empty, Nil, Nil, 0)))

          val lookups = plan.entities.flatMap { fetch =>
            val (root, correlation) = fetch.lookup.operation match {
              case LookupOperation.GraphQLQuery(name, _, LookupResult.ByKey(fields)) => name        -> fields.keys.toList
              case LookupOperation.GraphQLQuery(name, _, LookupResult.Single)        => name        -> Nil
              case _: LookupOperation.FederationEntities                             => "_entities" -> Nil
            }
            selected("Query", None) ::: selected("Query", Some(root)) ::: loop(
              generatedFields(fetch.source, "Query", root :: Nil) :::
                generatedFields(fetch.source, fetch.entityType, correlation),
              root = false
            )
          }
          (lookups ::: loop(plan.roots.flatMap(_.downstream) ::: plan.entities.flatMap(_.fields), root = false))
            .filter(_.directives.contains(SecurityDirective.UnsupportedPolicy))
        }
      (requested ::: injected).distinct
    }

  def isInterfaceObject(source: String, typeName: String): Boolean =
    interfaceObjects.contains(source -> typeName)

  def runtimeTypeSource(typeName: String, preferred: String): Option[String] = {
    val candidates = lookupSourcesByType.getOrElse(typeName, Nil)
    if (candidates.contains(preferred)) Some(preferred) else candidates.headOption
  }

  def runtimeTypes(source: String, typeName: String): List[String] =
    sourceRuntimeTypes.getOrElse(source -> typeName, Set.empty).toList.sorted

  def runtimeSources(current: Set[String], parentType: String, field: String): Set[String] = {
    val providers = fieldRoutes.getOrElse(parentType -> field, Nil).toSet
    if (providers.isEmpty) current
    else {
      val constrained = current intersect providers
      if (lookupTypes.contains(parentType) || constrained.isEmpty) providers else constrained
    }
  }

  def runtimeTypesForField(
    sources: Set[String],
    source: String,
    parentType: String,
    field: String,
    outputType: String
  ): Set[String] = {
    val candidates =
      if (sources.nonEmpty) sources.toList.sorted
      else source :: Nil
    val available  = candidates.flatMap { candidate =>
      sourceFields
        .get((candidate, parentType, field))
        .flatMap(_._type.innerType.name)
        .map(name => sourceRuntimeTypes.getOrElse(candidate -> name, Set.empty))
        .filter(_.nonEmpty)
    }
    available.reduceOption(_ intersect _).getOrElse(sourceRuntimeTypes.getOrElse(source -> outputType, Set.empty))
  }

  def isObjectType(typeName: String): Boolean =
    rootType.types.get(typeName).exists(_.kind == __TypeKind.OBJECT)

  def isObjectType(source: String, typeName: String): Boolean =
    sourceRuntimeTypes.get(source -> typeName).exists(_.contains(typeName))

  def appliesOnSource(source: String, parentType: String, field: Field): Boolean =
    field._condition.forall(condition =>
      isInterfaceObject(source, parentType) ||
        sourceRuntimeTypes.getOrElse(source -> parentType, Set.empty).exists(condition)
    )

  def executableField(source: String, field: Field): Field =
    executableField(source, None, field)

  def executableEntityField(
    source: String,
    entityType: String,
    field: Field
  ): Field =
    executableField(source, Some(entityType), field)

  private def executableField(
    source: String,
    parentType: Option[String],
    field: Field
  ): Field = {
    val parent      = parentType.orElse(field.parentType.flatMap(_.innerType.name)).getOrElse("")
    val targets     = field.targets.flatMap { original =>
      if (isInterfaceObject(source, parent)) None
      else {
        field._condition
          .map(
            _.filter(sourceRuntimeTypes.getOrElse(source -> parent, Set.empty))
              .filter(isObjectType(source, _))
          )
          .orElse(Some(original))
      }
    }
    val childParent = sourceFields
      .get((source, parent, field.name))
      .flatMap(_._type.innerType.name)
      .orElse(field.fieldType.innerType.name)
    val children    = disambiguate(source, field.fields.map(executableField(source, childParent, _)))
    field.copy(targets = targets, fields = children)
  }

  private def disambiguate(
    source: String,
    fields: List[Field]
  ): List[Field] = {
    def responseTypes(field: Field): Set[String] = {
      val sourceDefinitions = field.targets.toList
        .flatMap(_.toList)
        .flatMap(target => this.field(source, target, field.name))
        .map(value => DocumentRenderer.renderTypeName(value._type))
        .toSet
      if (sourceDefinitions.nonEmpty) sourceDefinitions else Set(DocumentRenderer.renderTypeName(field.fieldType))
    }

    val conflicts = fields
      .groupBy(_.aliasedName)
      .collect {
        case (name, values) if values.iterator.flatMap(responseTypes).toSet.size > 1 => name
      }
      .toSet
    if (conflicts.isEmpty) fields
    else {
      val initial = fields.iterator.map(_.aliasedName).toSet
      fields
        .foldLeft((List.empty[Field], initial)) { case ((values, used), field) =>
          if (!conflicts.contains(field.aliasedName)) (field :: values, used)
          else {
            val alias = privateResponseName(field.aliasedName, used)
            (field.copy(alias = Some(alias)) :: values, used + alias)
          }
        }
        ._1
        .reverse
    }
  }

  private def privateResponseName(responseName: String, used: Set[String]): String = {
    val base                     = s"_caliban_gateway_$responseName"
    def loop(index: Int): String = {
      val candidate = if (index == 0) base else s"${base}_$index"
      if (used.contains(candidate)) loop(index + 1) else candidate
    }
    loop(0)
  }

  def sourcesForKey(typeName: String, fields: List[ComposedGraph.KeyField]): List[String] =
    fields match {
      case Nil          => Nil
      case head :: tail =>
        val first = sourcesForKeyField(typeName, head)
        first.filter(source => tail.forall(field => sourcesForKeyField(typeName, field).contains(source)))
    }

  private def sourcesForKeyField(typeName: String, field: ComposedGraph.KeyField): List[String] = {
    val sources = sourceFieldSources.getOrElse(typeName -> field.name, Nil)
    if (field.children.isEmpty) sources
    else
      sources.filter(source =>
        sourceFields
          .get((source, typeName, field.name))
          .flatMap(_._type.innerType.name)
          .exists(name => field.children.forall(child => sourcesForKeyField(name, child).contains(source)))
      )
  }
}

private[gateway] object ComposedGraph {
  final case class KeyField(name: String, children: List[KeyField])

  private[internal] final case class SecurityApplication(
    source: String,
    typeName: String,
    fieldName: Option[String],
    directive: SecurityDirective
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
    final case class FederationEntities(correlationKey: Option[List[KeyField]]) extends LookupOperation {
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
}
