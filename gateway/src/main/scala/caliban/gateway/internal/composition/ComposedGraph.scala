package caliban.gateway.internal.composition

import caliban.execution.{ isMetaField, ExecutionRequest, Field }
import caliban.gateway.OperationPolicy.{ RuntimeTypeCondition, SecurityDirective, SecurityRequirement }
import caliban.introspection.adt._
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
  private val securityByCoordinate =
    securityApplications
      .groupBy(application => application.typeName -> application.fieldName)
      .map { case (coordinate, values) => coordinate -> values.map(_.directive).distinct }
  private val securedFieldTypes    = securityApplications.iterator
    .flatMap(application => application.fieldName.map(_ -> application.typeName))
    .toList
    .groupBy(_._1)
    .map { case (fieldName, values) => fieldName -> values.map(_._2).distinct.sorted }
  private val securedTypes         = securityApplications.collect {
    case application if application.fieldName.isEmpty =>
      application.typeName
  }.distinct.sorted
  private val sourceFieldSources   = sourceFields.keysIterator.map { case (source, owner, name) =>
    (owner -> name) -> source
  }.toList
    .groupBy(_._1)
    .map { case (coordinate, values) => coordinate -> values.map(_._2).distinct.sorted }
  private val lookupSourcesByType  = entityLookups.keysIterator.collect {
    case (source, typeName) if !interfaceObjects.contains(source -> typeName) => typeName -> source
  }.toList
    .groupBy(_._1)
    .map { case (typeName, values) => typeName -> values.map(_._2).distinct.sorted }
  private val lookupTypes          = entityLookups.keysIterator.map(_._2).toSet

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

  def hasSecurityRequirements: Boolean = securityApplications.nonEmpty

  def securityPolicyDiagnostics: List[String] =
    securityApplications
      .map(application =>
        s"[${application.source}] Federation ${application.directiveName} at '${application.coordinate}' requires an operation policy."
      )
      .distinct
      .sorted

  def securityRequirements(execution: ExecutionRequest): List[SecurityRequirement] =
    if (securityApplications.isEmpty) Nil
    else {
      def applications(typeName: String, fieldName: Option[String]): List[SecurityDirective] =
        securityByCoordinate.getOrElse(typeName -> fieldName, Nil)

      def condition(path: List[String], types: Option[Set[String]]): List[RuntimeTypeCondition] =
        types.filter(_.nonEmpty).map(RuntimeTypeCondition(path, _) :: Nil).getOrElse(Nil)

      def runtimeTypes(typeName: String): Set[String] =
        runtimeTypesByName.getOrElse(typeName, Set.empty)

      def candidateConditions(
        path: List[String],
        selectedType: String,
        candidateType: String,
        selection: Option[Set[String]]
      ): Option[List[RuntimeTypeCondition]] = {
        val selected   = selection.getOrElse(runtimeTypes(selectedType))
        val applicable = selected intersect runtimeTypes(candidateType)
        if (applicable.isEmpty) None
        else if (applicable == selected) Some(Nil)
        else Some(RuntimeTypeCondition(path, applicable) :: Nil)
      }

      def selected(
        responsePath: List[String],
        typeName: String,
        fieldName: Option[String],
        runtimeTypeConditions: List[RuntimeTypeCondition]
      ): List[SecurityRequirement] = {
        val values = applications(typeName, fieldName)
        if (values.isEmpty) Nil
        else SecurityRequirement(responsePath, typeName, fieldName, runtimeTypeConditions, values) :: Nil
      }

      def loop(
        fields: List[Field],
        parentPath: Vector[String],
        root: Boolean,
        inheritedConditions: List[RuntimeTypeCondition]
      ): List[SecurityRequirement] =
        fields.flatMap { field =>
          if (isMetaField(field)) Nil
          else {
            val responsePath     = parentPath :+ field.aliasedName
            val parentType       = field.parentType.flatMap(_.innerType.name).getOrElse("")
            val outputType       = field.fieldType.innerType.name.getOrElse("")
            val parentConditions = (inheritedConditions ::: condition(parentPath.toList, field._condition)).distinct
            val direct           = selected(responsePath.toList, parentType, Some(field.name), parentConditions)
            val rootRequirements =
              if (root) selected(responsePath.toList, parentType, None, parentConditions)
              else Nil
            val relatedFields    = securedFieldTypes.getOrElse(field.name, Nil).flatMap { typeName =>
              if (typeName == parentType) Nil
              else
                candidateConditions(parentPath.toList, parentType, typeName, field._condition).toList.flatMap {
                  conditions =>
                    selected(
                      responsePath.toList,
                      typeName,
                      Some(field.name),
                      (parentConditions ::: conditions).distinct
                    )
                }
            }
            val output           = selected(responsePath.toList, outputType, None, parentConditions)
            val relatedOutput    = securedTypes.flatMap { typeName =>
              if (typeName == outputType) Nil
              else
                candidateConditions(responsePath.toList, outputType, typeName, None).toList.flatMap { conditions =>
                  selected(
                    responsePath.toList,
                    typeName,
                    None,
                    (parentConditions ::: conditions).distinct
                  )
                }
            }

            rootRequirements ::: direct ::: relatedFields ::: output ::: relatedOutput ::: loop(
              field.fields,
              responsePath,
              root = false,
              parentConditions
            )
          }
        }

      loop(execution.field.fields, Vector.empty, root = true, Nil)
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
      if (sources.contains(source)) source :: Nil
      else if (sources.nonEmpty) sources.toList.sorted
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
      case SecurityDirective.Authenticated     => "@authenticated"
      case _: SecurityDirective.RequiresScopes => "@requiresScopes"
      case _: SecurityDirective.Policy         => "@policy"
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

    sealed trait ListResult extends LookupResult

    case object Ordered                                 extends ListResult
    final case class ByKey(fields: Map[String, String]) extends ListResult
  }
}
