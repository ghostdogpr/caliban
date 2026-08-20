package caliban.gateway.internal

import caliban.{ PathValue, ResponseValue }
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Value.{ BooleanValue, NullValue, StringValue }
import caliban.execution.{ ExecutionRequest, Field }
import caliban.gateway.Lookup
import caliban.gateway.OperationPolicy.{ RuntimeTypeCondition, SecurityDirective, SecurityRequirement }
import caliban.introspection.adt._
import caliban.parsing.{ Parser, SourceMapper }
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection }
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType
import caliban.validation.{ SchemaValidator, Validator }

import scala.collection.immutable.ListMap

private[gateway] final case class SchemaContribution(
  name: String,
  rootType: RootType,
  document: Document,
  federation: Boolean,
  lookups: List[Lookup],
  mapping: SchemaCoordinateMapping
)

private[gateway] final class ComposedGraph private[internal] (
  val rootType: RootType,
  private val routes: Map[(OperationType, String), List[String]],
  private val fieldRoutes: Map[(String, String), List[String]],
  private val sourceFields: Map[(String, String, String), __Field],
  private val entityLookups: Map[(String, String), List[ComposedGraph.EntityLookup]],
  private val requirements: Map[(String, String, String), List[Selection]],
  private val provisions: Map[(String, String, String), List[Selection]],
  private val interfaceObjects: Set[(String, String)],
  private val sourceRuntimeTypes: Map[(String, String), Set[String]],
  private val mappings: Map[String, SchemaCoordinateMapping],
  private val securityApplications: List[ComposedGraph.SecurityApplication]
) {
  private val securityByCoordinate =
    securityApplications.groupBy(application => application.typeName -> application.fieldName)
  private val runtimeTypesByName   = rootType.types.iterator.map { case (name, tpe) =>
    val runtimeTypes =
      if (tpe.kind == __TypeKind.OBJECT) Set(name)
      else tpe.possibleTypes.getOrElse(Nil).flatMap(_.name).toSet
    name -> runtimeTypes
  }.toMap
  private val securedFieldTypes    = securityApplications.iterator
    .flatMap(application => application.fieldName.map(_ -> application.typeName))
    .toList
    .groupBy(_._1)
    .map { case (fieldName, values) => fieldName -> values.map(_._2).distinct.sorted }
  private val securedTypes         = securityApplications.collect {
    case application if application.fieldName.isEmpty =>
      application.typeName
  }.distinct.sorted

  def sources(operation: OperationType, field: String): List[String] =
    routes.getOrElse(operation -> field, Nil)

  def source(typeName: String, field: String, preferred: String): Option[String] =
    fieldRoutes.get(typeName -> field).flatMap { sources =>
      if (sources.contains(preferred)) Some(preferred) else sources.headOption
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

  def mapping(source: String): Option[SchemaCoordinateMapping] =
    mappings.get(source)

  def hasSecurityRequirements: Boolean = securityApplications.nonEmpty

  def securityPolicyDiagnostics: List[String] =
    securityApplications
      .map(application =>
        s"[${application.source}] Federation ${application.directiveName} at '${application.coordinate}' requires an operation policy."
      )
      .distinct
      .sorted

  def securityRequirements(execution: ExecutionRequest): List[SecurityRequirement] = {
    def applications(typeName: String, fieldName: Option[String]): List[SecurityDirective] =
      securityByCoordinate.getOrElse(typeName -> fieldName, Nil).map(_.directive)

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
        if (ExecutionRequest.isMetaField(field)) Nil
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
    val candidates = entityLookups.keysIterator.collect {
      case (source, `typeName`) if !isInterfaceObject(source, typeName) => source
    }.toList.distinct.sorted
    if (candidates.contains(preferred)) Some(preferred) else candidates.headOption
  }

  def runtimeTypes(source: String, typeName: String): List[String] =
    sourceRuntimeTypes.getOrElse(source -> typeName, Set.empty).toList.sorted

  def runtimeSources(current: Set[String], parentType: String, field: String): Set[String] = {
    val providers = fieldRoutes.getOrElse(parentType -> field, Nil).toSet
    if (providers.isEmpty) current
    else {
      val constrained = current intersect providers
      if (entityLookups.keysIterator.exists(_._2 == parentType) || constrained.isEmpty) providers else constrained
    }
  }

  def runtimeTypesForField(
    sources: Set[String],
    source: String,
    parentType: String,
    field: String,
    outputType: String
  ): Set[String] = {
    val candidates = if (sources.nonEmpty) sources.toList.sorted else source :: Nil
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

  def appliesOnSource(source: String, parentType: String, field: caliban.execution.Field): Boolean =
    field._condition.forall(condition =>
      isInterfaceObject(source, parentType) ||
        sourceRuntimeTypes.getOrElse(source -> parentType, Set.empty).exists(condition)
    )

  def executableField(source: String, field: caliban.execution.Field): caliban.execution.Field =
    executableField(source, None, field)

  def executableEntityField(
    source: String,
    entityType: String,
    field: caliban.execution.Field
  ): caliban.execution.Field =
    executableField(source, Some(entityType), field)

  def restoreResponseNames(
    clientFields: List[caliban.execution.Field],
    executableFields: List[caliban.execution.Field],
    value: ResponseValue
  ): ResponseValue =
    value match {
      case ObjectValue(fields) =>
        val selected = executableFields
          .zip(clientFields)
          .groupBy(_._1.aliasedName)
          .flatMap { case (responseName, matches) =>
            for {
              executable <- matches.iterator.map(_._1).reduceOption(_.combine(_))
              client     <- matches.iterator.map(_._2).reduceOption(_.combine(_))
            } yield (responseName, (client, executable))
          }
        val restored = fields.map { case (name, nested) =>
          selected.get(name) match {
            case Some((client, executable)) =>
              client.aliasedName -> restoreResponseNames(client.fields, executable.fields, nested)
            case None                       => name -> nested
          }
        }
        val merged   = restored.foldLeft(ListMap.empty[String, ResponseValue]) { case (values, (name, nested)) =>
          values.updated(name, values.get(name).fold(nested)(mergeResponseValues(_, nested)))
        }
        ObjectValue(merged.toList)
      case ListValue(values)   => ListValue(values.map(restoreResponseNames(clientFields, executableFields, _)))
      case other               => other
    }

  def restoreResponsePath(
    clientFields: List[caliban.execution.Field],
    executableFields: List[caliban.execution.Field],
    path: List[PathValue]
  ): List[PathValue] =
    path match {
      case PathValue.Key(name) :: tail    =>
        executableFields.zip(clientFields).find(_._1.aliasedName == name) match {
          case Some((executable, client)) =>
            PathValue.Key(client.aliasedName) :: restoreResponsePath(client.fields, executable.fields, tail)
          case None                       => path
        }
      case PathValue.Index(index) :: tail =>
        PathValue.Index(index) :: restoreResponsePath(clientFields, executableFields, tail)
      case _ :: _                         => path
      case Nil                            => Nil
    }

  private def mergeResponseValues(left: ResponseValue, right: ResponseValue): ResponseValue =
    (left, right) match {
      case (ObjectValue(leftFields), ObjectValue(rightFields))                                    =>
        val merged = rightFields.foldLeft(ListMap(leftFields: _*)) { case (values, (name, nested)) =>
          values.updated(name, values.get(name).fold(nested)(mergeResponseValues(_, nested)))
        }
        ObjectValue(merged.toList)
      case (ListValue(leftValues), ListValue(rightValues)) if leftValues.size == rightValues.size =>
        ListValue(leftValues.zip(rightValues).map { case (leftValue, rightValue) =>
          mergeResponseValues(leftValue, rightValue)
        })
      case (NullValue, value)                                                                     => value
      case (value, NullValue)                                                                     => value
      case (_, value)                                                                             => value
    }

  private def executableField(
    source: String,
    parentType: Option[String],
    field: caliban.execution.Field
  ): caliban.execution.Field = {
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
    fields: List[caliban.execution.Field]
  ): List[caliban.execution.Field] = {
    def responseTypes(field: caliban.execution.Field): Set[String] = {
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
        .foldLeft((List.empty[caliban.execution.Field], initial)) { case ((values, used), field) =>
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
    val sources = sourceFields.keysIterator.collect {
      case (source, owner, name) if owner == typeName && name == field.name => source
    }.toList.sorted
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

private[gateway] object SchemaComposition {

  def isFederation(document: Document): Boolean =
    hasFederationTransport(document)

  def fieldSetDirectiveNames(document: Document): (Set[String], Set[String]) = {
    val names = federationDirectiveNames(document)
    (names.key ++ names.requires ++ names.provides) -> names.provides
  }

  def federationTransportTypes(document: Document, federation: Boolean): Set[String] =
    if (federation) federationDirectiveNames(document).hiddenTypes else Set.empty

  private def hasFederationTransport(document: Document): Boolean =
    federationLinks(document).nonEmpty || {
      val typeNames = document.typeDefinitions.iterator.map(_.name).toSet
      typeNames.contains("_Any") && typeNames.contains("_Entity") &&
      document.objectTypeDefinitions.exists(_.fields.exists(_.name == "_entities"))
    }

  def compose(contributions: List[SchemaContribution]): Either[List[String], ComposedGraph] = {
    val schemas           = contributions.sortBy(_.name)
    val prepared          = schemas.map { schema =>
      val names = federationDirectiveNames(schema.document)
      PreparedSchema(
        schema,
        names,
        federationKeyCoordinates(schema, names),
        federation1ExtensionKeyCoordinates(schema, names)
      )
    }
    val queryEntries      = rootFields(prepared, OperationType.Query)
    val mutationEntries   = rootFields(prepared, OperationType.Mutation)
    val types             = nonRootTypes(prepared)
    val enumUsages        = enumUsageByName(schemas)
    val directives        = prepared.flatMap { metadata =>
      metadata.schema.rootType.additionalDirectives
        .filterNot(directive => metadata.schema.federation && metadata.directives.hidden.contains(directive.name))
        .map(metadata.schema.name -> _)
    }
    val compiledFieldSets = prepared.map(federationFieldSets)
    val compiledSecurity  = prepared.map(securityApplications)
    val diagnostics       =
      (lookupDiagnostics(schemas) :::
        prepared.flatMap(federationKeyDiagnostics) :::
        compiledFieldSets.flatMap(_.fold(identity, _ => Nil)) :::
        compiledSecurity.flatMap(_.fold(identity, _ => Nil)) :::
        prepared.flatMap(unsupportedFederationDiagnostics) :::
        rootDiagnostics(OperationType.Query, queryEntries) :::
        rootDiagnostics(OperationType.Mutation, mutationEntries) :::
        incompatibleTypeDiagnostics(types, enumUsages) :::
        visibilityDiagnostics(queryEntries ::: mutationEntries, types) :::
        incompatibleDirectiveDiagnostics(directives)).distinct.sorted

    if (diagnostics.nonEmpty) Left(diagnostics)
    else {
      val queryFields                                        = chooseRootFields(queryEntries)
      val mutationFields                                     = chooseRootFields(mutationEntries)
      lazy val additionalByName: Map[String, __Type]         = chooseCompatible(types, enumUsages, additionalByName)
      def rewrite(tpe: __Type): __Type                       = rewriteType(tpe, additionalByName)
      val query                                              = makeRootType("Query", queryFields, rewrite)
      val mutation                                           =
        if (mutationFields.nonEmpty) Some(makeRootType("Mutation", mutationFields, rewrite))
        else None
      val additional                                         = additionalByName.toList.sortBy(_._1).map(_._2)
      val rootType                                           = RootType(
        query,
        mutation,
        None,
        additional,
        chooseCompatibleDirectives(directives)
      )
      val transformationDiagnostics                          = invalidTransformationDiagnostics(schemas, rootType)
      val allSecurity                                        = compiledSecurity.flatMap(_.toOption).flatten
      val securityVisibilityDiagnostics                      = hiddenSecurityDiagnostics(allSecurity, rootType)
      val routes: Map[(OperationType, String), List[String]] = rootRoutes(OperationType.Query, queryEntries) ++
        rootRoutes(OperationType.Mutation, mutationEntries)
      val fieldDefinitions                                   = types
        .flatMap(entry => entry.tpe.allFields.map(field => (entry.name -> field.name) -> entry))
        .groupBy(_._1)
      val fieldRoutes                                        = fieldDefinitions.flatMap { case (coordinate, definitions) =>
        val declared        = definitions.map(_._2)
        val overrideTargets = interfaceOverrideTargets(coordinate._1, coordinate._2, types)
        val owned           =
          effectiveFieldProviders(coordinate._2, declared).filterNot(entry => overrideTargets.contains(entry.source))
        if (owned.nonEmpty) Some(coordinate -> owned.map(_.source).distinct.sorted) else None
      }
      val sourceFields                                       = types.flatMap { entry =>
        entry.tpe.allFields.map(field => (entry.source, entry.name, field.name) -> field)
      }.toMap
      val lookups                                            = types
        .flatMap(entry => entry.entity.toList.flatMap(_.lookups).map((entry.source -> entry.name) -> _))
        .groupBy(_._1)
        .map { case (coordinate, values) => coordinate -> values.map(_._2) }
      val fieldSets                                          = compiledFieldSets.flatMap(_.toOption)
      val requirements                                       = fieldSets.flatMap(_.requirements).toMap
      val provisions                                         = fieldSets.flatMap(_.provisions).toMap
      val transitiveSecurityDiagnostics                      = missingTransitiveSecurityDiagnostics(
        requirements,
        allSecurity,
        rootType,
        schemas
      )
      if (
        transformationDiagnostics.nonEmpty || securityVisibilityDiagnostics.nonEmpty ||
        transitiveSecurityDiagnostics.nonEmpty
      )
        Left(
          (transformationDiagnostics ::: securityVisibilityDiagnostics ::: transitiveSecurityDiagnostics).distinct.sorted
        )
      else
        SchemaValidator
          .validateRootType(rootType)
          .left
          .map(error => List(s"[composition] ${error.getMessage}"))
          .map(_ =>
            new ComposedGraph(
              rootType,
              routes,
              fieldRoutes,
              sourceFields,
              lookups,
              requirements,
              provisions,
              types.iterator.filter(_.interfaceObject).map(entry => entry.source -> entry.name).toSet,
              schemas.iterator.flatMap { schema =>
                schema.rootType.types.iterator.map { case (name, tpe) =>
                  val runtimeTypes =
                    if (tpe.kind == __TypeKind.OBJECT) Set(name)
                    else tpe.possibleTypes.getOrElse(Nil).flatMap(_.name).toSet
                  (schema.name -> name) -> runtimeTypes
                }
              }.toMap,
              schemas.iterator.map(schema => schema.name -> schema.mapping).toMap,
              allSecurity
            )
          )
    }
  }

  private def hiddenSecurityDiagnostics(
    applications: List[ComposedGraph.SecurityApplication],
    rootType: RootType
  ): List[String] =
    applications.collect {
      case application if !rootType.types.get(application.typeName).exists { tpe =>
            application.fieldName.forall(name => tpe.allFields.exists(_.name == name))
          } =>
        s"[${application.source}] Federation ${application.directiveName} at '${application.coordinate}' cannot be enforced because the coordinate is not client-visible."
    }

  private final case class SecurityProfile(
    authenticated: Boolean,
    scopes: Option[List[Set[String]]],
    policies: Option[List[Set[String]]]
  ) {
    def implies(required: SecurityProfile): Boolean =
      (authenticated || !required.authenticated) &&
        SecurityProfile.implies(scopes, required.scopes) &&
        SecurityProfile.implies(policies, required.policies)
  }

  private object SecurityProfile {
    def apply(applications: List[ComposedGraph.SecurityApplication]): SecurityProfile =
      SecurityProfile(
        applications.exists(_.directive == SecurityDirective.Authenticated),
        conjunction(applications.flatMap { application =>
          application.directive match {
            case SecurityDirective.RequiresScopes(values) => Some(values)
            case _                                        => None
          }
        }),
        conjunction(applications.flatMap { application =>
          application.directive match {
            case SecurityDirective.Policy(values) => Some(values)
            case _                                => None
          }
        })
      )

    private def conjunction(expressions: List[List[List[String]]]): Option[List[Set[String]]] =
      if (expressions.isEmpty) None
      else
        Some(
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
        )

    private def implies(actual: Option[List[Set[String]]], required: Option[List[Set[String]]]): Boolean = {
      val actualValues   = actual.getOrElse(List(Set.empty[String]))
      val requiredValues = required.getOrElse(List(Set.empty[String]))
      actualValues.forall(value => requiredValues.exists(_.subsetOf(value)))
    }
  }

  private def missingTransitiveSecurityDiagnostics(
    requirements: Map[(String, String, String), List[Selection]],
    applications: List[ComposedGraph.SecurityApplication],
    rootType: RootType,
    schemas: List[SchemaContribution]
  ): List[String] = {
    val schemaByName       = schemas.iterator.map(schema => schema.name -> schema).toMap
    val runtimeTypesByName = rootType.types.iterator.map { case (name, tpe) =>
      val runtimeTypes =
        if (tpe.kind == __TypeKind.OBJECT) Set(name)
        else tpe.possibleTypes.getOrElse(Nil).flatMap(_.name).toSet
      name -> runtimeTypes
    }.toMap

    def applicable(selectedType: String, candidateType: String): Boolean =
      selectedType == candidateType || {
        val selected  = runtimeTypesByName.getOrElse(selectedType, Set.empty)
        val candidate = runtimeTypesByName.getOrElse(candidateType, Set.empty)
        selected.nonEmpty && candidate.nonEmpty && (selected intersect candidate).nonEmpty
      }

    def typeApplications(typeName: String): List[ComposedGraph.SecurityApplication] =
      applications.filter(application => application.fieldName.isEmpty && applicable(typeName, application.typeName))

    def fieldApplications(typeName: String, fieldName: String): List[ComposedGraph.SecurityApplication] =
      applications.filter(application =>
        application.fieldName.contains(fieldName) && applicable(typeName, application.typeName)
      )

    def profile(typeName: String, fieldName: Option[String]): SecurityProfile =
      SecurityProfile(typeApplications(typeName) ::: fieldName.toList.flatMap(fieldApplications(typeName, _)))

    def dependencies(
      selections: List[Selection],
      parentType: String
    ): List[(String, SecurityProfile)] =
      selections.flatMap {
        case field: Selection.Field             =>
          rootType.types
            .get(parentType)
            .flatMap(_.allFields.find(_.name == field.name))
            .toList
            .flatMap { definition =>
              val outputType = definition._type.innerType.name
              val required   = SecurityProfile(
                fieldApplications(parentType, field.name) ::: outputType.toList.flatMap(typeApplications)
              )
              (s"$parentType.${field.name}" -> required) ::
                outputType.toList.flatMap(dependencies(field.selectionSet, _))
            }
        case fragment: Selection.InlineFragment =>
          val selectedType = fragment.typeCondition.fold(parentType)(_.name)
          val required     = SecurityProfile(typeApplications(selectedType))
          (selectedType -> required) :: dependencies(fragment.selectionSet, selectedType)
        case _: Selection.FragmentSpread        => Nil
      }

    requirements.toList.flatMap { case ((source, sourceType, fieldName), selections) =>
      val typeName  = schemaByName.get(source).fold(sourceType)(composedTypeName(_, sourceType))
      val available = profile(typeName, Some(fieldName))
      dependencies(selections, typeName).collect {
        case (dependency, required) if !available.implies(required) =>
          s"[$source] Field '$typeName.$fieldName' does not specify sufficient Federation security requirements for @requires dependency '$dependency'."
      }
    }.distinct.sorted
  }

  private def invalidTransformationDiagnostics(
    schemas: List[SchemaContribution],
    rootType: RootType
  ): List[String] = {
    val types = rootType.types

    def emptyType(name: String, kind: __TypeKind): Boolean =
      types.get(name).exists { tpe =>
        tpe.kind == kind && (kind match {
          case __TypeKind.OBJECT | __TypeKind.INTERFACE => tpe.allFields.isEmpty
          case __TypeKind.INPUT_OBJECT                  => tpe.allInputFields.isEmpty
          case __TypeKind.ENUM                          => tpe.allEnumValues.isEmpty
          case _                                        => false
        })
      }

    schemas.flatMap { schema =>
      val mapping = schema.mapping
      val fields  = mapping.hiddenFields.collect {
        case (tpe, _) if emptyType(mapping.composedType(tpe), __TypeKind.OBJECT)    =>
          s"[${schema.name}] Transformation leaves object '${mapping.composedType(tpe)}' with no visible fields."
        case (tpe, _) if emptyType(mapping.composedType(tpe), __TypeKind.INTERFACE) =>
          s"[${schema.name}] Transformation leaves interface '${mapping.composedType(tpe)}' with no visible fields."
      }
      val inputs  = mapping.hiddenInputFields.collect {
        case (tpe, _) if emptyType(mapping.composedType(tpe), __TypeKind.INPUT_OBJECT) =>
          s"[${schema.name}] Transformation leaves input object '${mapping.composedType(tpe)}' with no visible fields."
      }
      val enums   = mapping.hiddenEnumValues.collect {
        case (tpe, _) if emptyType(mapping.composedType(tpe), __TypeKind.ENUM) =>
          s"[${schema.name}] Transformation leaves enum '${mapping.composedType(tpe)}' with no visible values."
      }
      fields.toList ::: inputs.toList ::: enums.toList
    }.distinct.sorted
  }

  private def lookupDiagnostics(schemas: List[SchemaContribution]): List[String] =
    schemas.flatMap { schema =>
      val sourceKind =
        if (schema.federation && schema.lookups.nonEmpty)
          List(s"[${schema.name}] Ordinary GraphQL lookups cannot be declared on a Federation subgraph.")
        else Nil
      val duplicates = schema.lookups
        .groupBy(_.typeName)
        .collect {
          case (typeName, values) if values.size > 1 =>
            s"[${schema.name}] More than one lookup is declared for type '$typeName'."
        }
        .toList
      sourceKind ::: duplicates ::: schema.lookups.flatMap(validateLookup(schema, _))
    }

  private def validateLookup(schema: SchemaContribution, lookup: Lookup): List[String] = {
    val prefix      = s"[${schema.name}]"
    val targetType  = schema.rootType.types.get(lookup.typeName)
    val rootName    = schema.rootType.queryType.name.getOrElse("Query")
    val sourceField = schema.rootType.queryType.allFields.find(_.name == lookup.field)
    val keyNames    = lookup.keyFields
    val keys        = targetType.toList
      .flatMap(target => keyNames.flatMap(name => target.allFields.find(_.name == name).map(name -> _)))
      .toMap

    val targetDiagnostics      = targetType match {
      case None                                             =>
        List(s"$prefix Lookup target type '${lookup.typeName}' does not exist.")
      case Some(target) if target.kind != __TypeKind.OBJECT =>
        List(s"$prefix Lookup target type '${lookup.typeName}' must be an object type.")
      case Some(_)                                          => Nil
    }
    val keyDiagnostics         =
      (if (keyNames.isEmpty) List(s"$prefix Lookup for '${lookup.typeName}' must declare at least one key field.")
       else Nil) :::
        keyNames
          .groupBy(identity)
          .collect {
            case (name, values) if values.size > 1 =>
              s"$prefix Lookup key field '${lookup.typeName}.$name' is declared more than once."
          }
          .toList :::
        targetType.toList.flatMap { target =>
          keyNames.flatMap { name =>
            target.allFields.find(_.name == name) match {
              case None        => List(s"$prefix Lookup key field '${lookup.typeName}.$name' does not exist.")
              case Some(field) =>
                val kind = nullableType(field._type).kind
                if (kind == __TypeKind.SCALAR || kind == __TypeKind.ENUM) Nil
                else List(s"$prefix Lookup key field '${lookup.typeName}.$name' must be a scalar or enum.")
            }
          }
        }
    val fieldDiagnostics       = sourceField match {
      case None        => List(s"$prefix Lookup field '$rootName.${lookup.field}' does not exist.")
      case Some(field) =>
        val resultType  = nullableType(field._type)
        val shapeValid  = lookup match {
          case _: Lookup.Single     => resultType.kind != __TypeKind.LIST && resultType.name.contains(lookup.typeName)
          case _: Lookup.ListLookup =>
            resultType.kind == __TypeKind.LIST && resultType.ofType
              .map(nullableType)
              .exists(element => element.kind != __TypeKind.LIST && element.name.contains(lookup.typeName))
        }
        val shape       = lookup match {
          case _: Lookup.Single     => s"'${lookup.typeName}'"
          case _: Lookup.ListLookup => s"a list of '${lookup.typeName}'"
        }
        val shapeErrors =
          if (shapeValid) Nil else List(s"$prefix Lookup field '$rootName.${lookup.field}' must return $shape.")
        shapeErrors ::: validateLookupArguments(prefix, rootName, lookup, field, keys)
    }
    val correlationDiagnostics = (lookup, targetType, sourceField) match {
      case (list: Lookup.ListLookup, Some(target), Some(field)) =>
        validateCorrelation(prefix, rootName, list, field, target, keys)
      case _                                                    => Nil
    }

    targetDiagnostics ::: keyDiagnostics ::: fieldDiagnostics ::: correlationDiagnostics
  }

  private def validateLookupArguments(
    prefix: String,
    rootName: String,
    lookup: Lookup,
    field: __Field,
    keys: Map[String, __Field]
  ): List[String] = {
    val arguments   = field.allArgs.map(argument => argument.name -> argument).toMap
    val unknown     = lookup.arguments.keysIterator
      .filterNot(arguments.contains)
      .map(name => s"$prefix Lookup field '$rootName.${lookup.field}' has no argument '$name'.")
      .toList
    val missing     = field.allArgs.collect {
      case argument
          if !lookup.arguments.contains(argument.name) && !argument._type.isNullable && argument.defaultValue.isEmpty =>
        s"$prefix Required lookup argument '${lookup.field}.${argument.name}' has no mapping."
    }
    val mappings    = lookup.arguments.toList.flatMap { case (name, mapping) =>
      arguments.get(name).toList.flatMap(argument => validateArgument(prefix, name, mapping, argument._type, keys))
    }
    val batch       = lookup match {
      case _: Lookup.Single if lookup.arguments.values.exists(containsBatch)                               =>
        List(s"$prefix Single lookup argument mappings cannot contain a batch mapping.")
      case _: Lookup.ListLookup if !lookup.arguments.values.exists(containsBatch)                          =>
        List(s"$prefix List lookup argument mappings must contain a batch mapping.")
      case _: Lookup.ListLookup if lookup.arguments.values.exists(keyOutsideBatch(_, insideBatch = false)) =>
        List(s"$prefix List lookup key mappings must be nested inside a batch mapping.")
      case _                                                                                               => Nil
    }
    val mappedKeys  = lookup.arguments.valuesIterator.flatMap(argumentKeys).toSet
    val keyCoverage =
      if (mappedKeys == lookup.keyFields.toSet) Nil
      else List(s"$prefix Lookup argument mappings must use every declared key field.")

    unknown ::: missing ::: mappings ::: batch ::: keyCoverage
  }

  private def validateArgument(
    prefix: String,
    path: String,
    mapping: Lookup.Argument,
    expected: __Type,
    keys: Map[String, __Field]
  ): List[String] = {
    val valueType = nullableType(expected)
    mapping match {
      case Lookup.Argument.Key(field)            =>
        keys.get(field) match {
          case None           => List(s"$prefix Lookup argument '$path' references undeclared key field '$field'.")
          case Some(keyField) =>
            if (compatibleValueType(keyField._type, valueType)) Nil
            else
              List(
                s"$prefix Lookup argument '$path' is incompatible with key field '${keyField.name}'."
              )
        }
      case Lookup.Argument.ObjectMapping(fields) =>
        if (valueType.kind != __TypeKind.INPUT_OBJECT)
          List(s"$prefix Lookup argument '$path' maps an object into a non-input-object value.")
        else {
          val inputFields = valueType.allInputFields.map(field => field.name -> field).toMap
          val duplicates  = fields
            .groupBy(_._1)
            .collect {
              case (name, values) if values.size > 1 =>
                s"$prefix Lookup argument '$path.$name' is mapped more than once."
            }
            .toList
          val unknown     = fields.collect {
            case (name, _) if !inputFields.contains(name) =>
              s"$prefix Lookup input field '$path.$name' does not exist."
          }
          val names       = fields.iterator.map(_._1).toSet
          val missing     = valueType.allInputFields.collect {
            case input if !names.contains(input.name) && !input._type.isNullable && input.defaultValue.isEmpty =>
              s"$prefix Required lookup input field '$path.${input.name}' has no mapping."
          }
          duplicates ::: unknown ::: missing ::: fields.flatMap { case (name, value) =>
            inputFields
              .get(name)
              .toList
              .flatMap(input => validateArgument(prefix, s"$path.$name", value, input._type, keys))
          }
        }
      case Lookup.Argument.Batch(value)          =>
        if (containsBatch(value)) List(s"$prefix Lookup argument '$path' cannot nest a batch mapping.")
        else if (!valueType.isList) List(s"$prefix Lookup argument '$path' maps a batch into a non-list value.")
        else validateArgument(prefix, path, value, valueType.ofType.map(nullableType).getOrElse(valueType), keys)
    }
  }

  private def validateCorrelation(
    prefix: String,
    rootName: String,
    lookup: Lookup.ListLookup,
    field: __Field,
    target: __Type,
    keys: Map[String, __Field]
  ): List[String] =
    lookup.correlation match {
      case Lookup.Correlation.Ordered       => Nil
      case Lookup.Correlation.ByKey(fields) =>
        val nullability = nullableType(field._type).ofType match {
          case Some(element) if !element.isNullable => Nil
          case _                                    =>
            List(s"$prefix By-key lookup field '$rootName.${lookup.field}' must return non-null items.")
        }
        val coverage    =
          if (fields.values.toList.sorted == lookup.keyFields.sorted) Nil
          else List(s"$prefix By-key lookup correlation must map every declared key field exactly once.")
        val values      = fields.toList.flatMap { case (responseField, keyField) =>
          target.allFields.find(_.name == responseField) match {
            case None                =>
              List(s"$prefix Lookup correlation field '${lookup.typeName}.$responseField' does not exist.")
            case Some(responseValue) =>
              keys.get(keyField) match {
                case None           => List(s"$prefix Lookup correlation references undeclared key field '$keyField'.")
                case Some(keyValue) =>
                  if (compatibleValueType(responseValue._type, keyValue._type)) Nil
                  else
                    List(
                      s"$prefix Lookup correlation field '${lookup.typeName}.$responseField' is incompatible with key '$keyField'."
                    )
              }
          }
        }
        nullability ::: coverage ::: values
    }

  private def containsBatch(argument: Lookup.Argument): Boolean =
    argument match {
      case _: Lookup.Argument.Key                => false
      case Lookup.Argument.ObjectMapping(fields) => fields.exists(value => containsBatch(value._2))
      case _: Lookup.Argument.Batch              => true
    }

  private def keyOutsideBatch(argument: Lookup.Argument, insideBatch: Boolean): Boolean =
    argument match {
      case _: Lookup.Argument.Key                => !insideBatch
      case Lookup.Argument.ObjectMapping(fields) => fields.exists(value => keyOutsideBatch(value._2, insideBatch))
      case Lookup.Argument.Batch(value)          => keyOutsideBatch(value, insideBatch = true)
    }

  private def argumentKeys(argument: Lookup.Argument): List[String] =
    argument match {
      case Lookup.Argument.Key(field)            => field :: Nil
      case Lookup.Argument.ObjectMapping(fields) => fields.flatMap(value => argumentKeys(value._2))
      case Lookup.Argument.Batch(value)          => argumentKeys(value)
    }

  private def nullableType(tpe: __Type): __Type =
    if (tpe.kind == __TypeKind.NON_NULL) tpe.ofType.map(nullableType).getOrElse(tpe) else tpe

  private def compatibleValueType(left: __Type, right: __Type): Boolean = {
    val a = nullableType(left)
    val b = nullableType(right)
    a.kind == b.kind && a.name == b.name
  }

  private def rootFields(
    schemas: List[PreparedSchema],
    operation: OperationType
  ): List[RootFieldEntry] =
    schemas.flatMap { metadata =>
      val schema = metadata.schema
      val root   = operation match {
        case OperationType.Query        => Some(schema.rootType.queryType)
        case OperationType.Mutation     => schema.rootType.mutationType
        case OperationType.Subscription => None
      }
      val names  = metadata.directives
      root.toList.flatMap { rootType =>
        val typeShareable = schema.federation && hasDirective(rootType.directives, names.shareable)
        rootType.allFields
          .filterNot(field => schema.federation && isTransportField(field.name))
          .map { field =>
            RootFieldEntry(
              schema.name,
              operation,
              field,
              typeShareable || schema.federation && hasDirective(field.directives, names.shareable),
              schema.federation && hasDirective(field.directives, names.external),
              schema.mapping.hiddenFields.contains(rootType.name.getOrElse("") -> field.name) ||
                schema.federation && hasDirective(field.directives, names.inaccessible),
              if (schema.federation) directiveString(field.directives, names.overrideDirective, "from") else None,
              schema.federation,
              federationLinks(schema.document).nonEmpty,
              (field.allArgs
                .filter(argument => schema.federation && hasDirective(argument.directives, names.inaccessible))
                .map(_.name)
                .toSet ++ schema.mapping.hiddenArguments.collect {
                case (owner, fieldName, argument) if owner == rootType.name.getOrElse("") && fieldName == field.name =>
                  argument
              }),
              if (schema.federation) names.hidden else Set.empty
            )
          }
      }
    }.sortBy(entry => entry.field.name -> entry.source)

  private def rootDiagnostics(
    operation: OperationType,
    fields: List[RootFieldEntry]
  ): List[String] =
    fields
      .groupBy(_.field.name)
      .toList
      .flatMap { case (field, entries) =>
        val providers  = effectiveRootProviders(entries)
        val prefix     = s"[${operation.toString.toLowerCase}.$field]"
        val compatible = fieldsCompatible(entries.map(_.field))
        overrideDiagnostics(prefix, entries.map(entry => entry.source -> entry.overrideFrom)) :::
          (if (compatible && providers.size > 1 && providers.exists(entry => !entry.federation)) {
             val sources = formatSources(providers.map(_.source))
             List(s"$prefix Field is resolved by multiple ordinary subgraphs: $sources.")
           } else Nil) :::
          (if (
             compatible && providers.size > 1 && providers.forall(_.federation) && providers.exists(_.federation2) &&
             !providers.forall(entry => !entry.federation2 || entry.shareable)
           ) {
             val sources = formatSources(providers.map(_.source))
             List(
               s"$prefix Field is resolved by multiple subgraphs without compatible @shareable declarations: $sources."
             )
           } else Nil) :::
          (if (!compatible) {
             val sources = formatSources(entries.map(_.source))
             List(s"$prefix Definitions are incompatible between subgraphs: $sources.")
           } else Nil)
      }

  private def chooseRootFields(fields: List[RootFieldEntry]): List[RootFieldEntry] =
    fields
      .groupBy(_.field.name)
      .toList
      .sortBy(_._1)
      .flatMap { case (_, entries) =>
        if (entries.exists(_.inaccessible)) Nil
        else {
          val providers = effectiveRootProviders(entries).sortBy(_.source)
          providers.headOption.map { selected =>
            val mergedType       = providers.map(_.field._type).reduceOption(mergeOutputType).getOrElse(selected.field._type)
            val inaccessibleArgs = entries.iterator.flatMap(_.inaccessibleArguments).toSet
            selected.copy(
              field = selected.field.copy(`type` = () => mergedType),
              inaccessibleArguments = inaccessibleArgs
            )
          }.toList
        }
      }

  private def rootRoutes(
    operation: OperationType,
    fields: List[RootFieldEntry]
  ): Map[(OperationType, String), List[String]] =
    fields
      .groupBy(_.field.name)
      .flatMap { case (name, entries) =>
        val providers = effectiveRootProviders(entries).sortBy(_.source)
        val sources   = providers.headOption.toList.flatMap { first =>
          first.field._type.innerType.kind match {
            case __TypeKind.OBJECT | __TypeKind.INTERFACE | __TypeKind.UNION => providers.map(_.source).distinct
            case _                                                           => first.source :: Nil
          }
        }
        if (entries.exists(_.inaccessible)) None else Some((operation -> name) -> sources)
      }

  private final case class RootFieldEntry(
    source: String,
    operation: OperationType,
    field: __Field,
    shareable: Boolean,
    external: Boolean,
    inaccessible: Boolean,
    overrideFrom: Option[String],
    federation: Boolean,
    federation2: Boolean,
    inaccessibleArguments: Set[String],
    hiddenDirectives: Set[String]
  )

  private final case class PreparedSchema(
    schema: SchemaContribution,
    directives: FederationDirectiveNames,
    keyCoordinates: Set[(String, String)],
    federation1ExtensionKeyCoordinates: Set[(String, String)]
  )

  private final case class TypeEntry(
    source: String,
    name: String,
    tpe: __Type,
    interfaceObject: Boolean,
    entity: Option[EntityDefinition],
    ownedFields: Set[String],
    shareableFields: Set[String],
    inaccessible: Boolean,
    inaccessibleFields: Set[String],
    inaccessibleArguments: Set[(String, String)],
    inaccessibleInputFields: Set[String],
    inaccessibleEnumValues: Set[String],
    overrideFields: Map[String, String],
    federation2: Boolean,
    inaccessibleDirectives: Set[String],
    hiddenDirectives: Set[String]
  )

  private final case class EntityDefinition(
    keyFields: Set[String],
    lookups: List[ComposedGraph.EntityLookup]
  )

  private final case class FederationKey(fields: List[ComposedGraph.KeyField], resolvable: Boolean)

  private final case class FederationFieldSets(
    requirements: List[((String, String, String), List[Selection])],
    provisions: List[((String, String, String), List[Selection])]
  )

  private final case class EnumUsage(input: Boolean, output: Boolean)

  private final case class SecurityCoordinate(typeName: String, fieldName: Option[String])

  private final case class TypeSystemDirectiveApplication(
    coordinate: String,
    directives: List[Directive],
    securityCoordinate: Option[SecurityCoordinate],
    supportsOverride: Boolean
  )

  private def objectLikeEntries(document: Document): List[(String, List[Directive], List[FieldDefinition])] =
    document.typeDefinitions.collect {
      case definition: ObjectTypeDefinition    => (definition.name, definition.directives, definition.fields)
      case definition: InterfaceTypeDefinition => (definition.name, definition.directives, definition.fields)
    } ::: document.typeExtensions.collect {
      case extension: ObjectTypeExtension    => (extension.name, extension.directives, extension.fields)
      case extension: InterfaceTypeExtension => (extension.name, extension.directives, extension.fields)
    }

  private def typeSystemDirectiveApplications(
    document: Document,
    composedName: String => String
  ): List[TypeSystemDirectiveApplication] = {
    def unsupported(coordinate: String, directives: List[Directive]): TypeSystemDirectiveApplication =
      TypeSystemDirectiveApplication(coordinate, directives, None, supportsOverride = false)

    def typeApplication(
      name: String,
      directives: List[Directive],
      supportsSecurity: Boolean
    ): TypeSystemDirectiveApplication = {
      val typeName = composedName(name)
      TypeSystemDirectiveApplication(
        typeName,
        directives,
        if (supportsSecurity) Some(SecurityCoordinate(typeName, None)) else None,
        supportsOverride = false
      )
    }

    def fieldApplications(typeName: String, fields: List[FieldDefinition]): List[TypeSystemDirectiveApplication] = {
      val parent = composedName(typeName)
      fields.flatMap { field =>
        TypeSystemDirectiveApplication(
          s"$parent.${field.name}",
          field.directives,
          Some(SecurityCoordinate(parent, Some(field.name))),
          supportsOverride = true
        ) :: field.args.map(argument => unsupported(s"$parent.${field.name}(${argument.name}:)", argument.directives))
      }
    }

    def inputApplications(
      typeName: String,
      fields: List[InputValueDefinition]
    ): List[TypeSystemDirectiveApplication] = {
      val parent = composedName(typeName)
      fields.map(field => unsupported(s"$parent.${field.name}", field.directives))
    }

    def enumApplications(
      typeName: String,
      values: List[EnumValueDefinition]
    ): List[TypeSystemDirectiveApplication] = {
      val parent = composedName(typeName)
      values.map(value => unsupported(s"$parent.${value.enumValue}", value.directives))
    }

    val schemas            = document.schemaDefinition.toList.map(definition => unsupported("schema", definition.directives)) :::
      document.typeExtensions.collect { case extension: SchemaExtension =>
        unsupported("schema", extension.directives)
      }
    val types              = document.typeDefinitions.flatMap {
      case definition: ScalarTypeDefinition      =>
        typeApplication(definition.name, definition.directives, supportsSecurity = true) :: Nil
      case definition: ObjectTypeDefinition      =>
        typeApplication(definition.name, definition.directives, supportsSecurity = true) ::
          fieldApplications(definition.name, definition.fields)
      case definition: InterfaceTypeDefinition   =>
        typeApplication(definition.name, definition.directives, supportsSecurity = true) ::
          fieldApplications(definition.name, definition.fields)
      case definition: UnionTypeDefinition       =>
        typeApplication(definition.name, definition.directives, supportsSecurity = false) :: Nil
      case definition: EnumTypeDefinition        =>
        typeApplication(definition.name, definition.directives, supportsSecurity = true) ::
          enumApplications(definition.name, definition.enumValuesDefinition)
      case definition: InputObjectTypeDefinition =>
        typeApplication(definition.name, definition.directives, supportsSecurity = false) ::
          inputApplications(definition.name, definition.fields)
    }
    val extensions         = document.typeExtensions.flatMap {
      case extension: SchemaExtension          => Nil
      case extension: ScalarTypeExtension      =>
        typeApplication(extension.name, extension.directives, supportsSecurity = true) :: Nil
      case extension: ObjectTypeExtension      =>
        typeApplication(extension.name, extension.directives, supportsSecurity = true) ::
          fieldApplications(extension.name, extension.fields)
      case extension: InterfaceTypeExtension   =>
        typeApplication(extension.name, extension.directives, supportsSecurity = true) ::
          fieldApplications(extension.name, extension.fields)
      case extension: UnionTypeExtension       =>
        typeApplication(extension.name, extension.directives, supportsSecurity = false) :: Nil
      case extension: EnumTypeExtension        =>
        typeApplication(extension.name, extension.directives, supportsSecurity = true) ::
          enumApplications(extension.name, extension.enumValuesDefinition)
      case extension: InputObjectTypeExtension =>
        typeApplication(extension.name, extension.directives, supportsSecurity = false) ::
          inputApplications(extension.name, extension.fields)
    }
    val directiveArguments = document.directiveDefinitions.flatMap { definition =>
      definition.args.map(argument => unsupported(s"@${definition.name}(${argument.name}:)", argument.directives))
    }

    schemas ::: types ::: extensions ::: directiveArguments
  }

  private def securityApplications(
    metadata: PreparedSchema
  ): Either[List[String], List[ComposedGraph.SecurityApplication]] = {
    val schema   = metadata.schema
    val names    = metadata.directives
    val compiled = typeSystemDirectiveApplications(schema.document, composedTypeName(schema, _)).flatMap {
      application =>
        application.securityCoordinate.toList.flatMap { securityCoordinate =>
          application.directives.flatMap(directive =>
            compileSecurityDirective(schema.name, application.coordinate, directive, names).map(
              _.map { value =>
                ComposedGraph.SecurityApplication(
                  schema.name,
                  securityCoordinate.typeName,
                  securityCoordinate.fieldName,
                  value
                )
              }
            )
          )
        }
    }
    val errors   = compiled.collect { case Left(error) => error }

    if (errors.nonEmpty) Left(errors)
    else Right(compiled.collect { case Right(application) => application })
  }

  private def compileSecurityDirective(
    source: String,
    coordinate: String,
    directive: Directive,
    names: FederationDirectiveNames
  ): Option[Either[String, SecurityDirective]] =
    if (names.authenticated.contains(directive.name))
      Some(
        if (directive.arguments.isEmpty) Right(SecurityDirective.Authenticated)
        else Left(s"[$source] Invalid Federation @authenticated application at '$coordinate'.")
      )
    else if (names.requiresScopes.contains(directive.name))
      Some(
        groupedStrings(directive.arguments, "scopes")
          .map(SecurityDirective.RequiresScopes.apply)
          .toRight(s"[$source] Invalid Federation @requiresScopes application at '$coordinate'.")
      )
    else if (names.policy.contains(directive.name))
      Some(
        groupedStrings(directive.arguments, "policies")
          .map(SecurityDirective.Policy.apply)
          .toRight(s"[$source] Invalid Federation @policy application at '$coordinate'.")
      )
    else None

  private def groupedStrings(arguments: Map[String, caliban.InputValue], name: String): Option[List[List[String]]] =
    if (arguments.keySet != Set(name)) None
    else
      arguments.get(name).collect { case caliban.InputValue.ListValue(groups) => groups }.flatMap { groups =>
        val values = groups.map {
          case caliban.InputValue.ListValue(entries) =>
            val strings = entries.collect { case StringValue(value) => value }
            if (strings.size == entries.size) Some(strings) else None
          case _                                     => None
        }
        if (values.forall(_.nonEmpty)) Some(values.flatten) else None
      }

  private def composedTypeName(schema: SchemaContribution, typeName: String): String =
    if (schema.rootType.queryType.name.contains(typeName)) "Query"
    else if (schema.rootType.mutationType.flatMap(_.name).contains(typeName)) "Mutation"
    else if (schema.rootType.subscriptionType.flatMap(_.name).contains(typeName)) "Subscription"
    else typeName

  private def unsupportedFederationDiagnostics(metadata: PreparedSchema): List[String] = {
    val schema = metadata.schema
    val names  = metadata.directives
    typeSystemDirectiveApplications(schema.document, composedTypeName(schema, _)).flatMap { application =>
      application.directives.flatMap { directive =>
        val security          = securityDirectiveName(directive.name, names).collect {
          case name if application.securityCoordinate.isEmpty =>
            s"[${schema.name}] Federation $name is not supported at '${application.coordinate}'."
        }
        val unavailable       = names.unavailableSecurity
          .get(directive.name)
          .map(name =>
            s"[${schema.name}] Federation $name is not available in the linked feature version at '${application.coordinate}'."
          )
        val context           =
          if (names.context.contains(directive.name))
            Some(s"[${schema.name}] Federation @context is not supported at '${application.coordinate}'.")
          else None
        val from              =
          if (names.fromContext.contains(directive.name))
            Some(s"[${schema.name}] Federation @fromContext is not supported at '${application.coordinate}'.")
          else None
        val overrideDirective =
          if (!names.overrideDirective.contains(directive.name)) None
          else if (directive.arguments.contains("label"))
            Some(s"[${schema.name}] Federation @override(label:) is not supported at '${application.coordinate}'.")
          else if (!application.supportsOverride)
            Some(s"[${schema.name}] Federation @override is not supported at '${application.coordinate}'.")
          else None

        security.toList ::: unavailable.toList ::: context.toList ::: from.toList ::: overrideDirective.toList
      }
    }
  }

  private def securityDirectiveName(name: String, names: FederationDirectiveNames): Option[String] =
    if (names.authenticated.contains(name)) Some("@authenticated")
    else if (names.requiresScopes.contains(name)) Some("@requiresScopes")
    else if (names.policy.contains(name)) Some("@policy")
    else None

  private def federationFieldSets(
    metadata: PreparedSchema
  ): Either[List[String], FederationFieldSets] = {
    val schema = metadata.schema
    if (!schema.federation) Right(FederationFieldSets(Nil, Nil))
    else {
      val names        = metadata.directives
      val fields       = objectLikeEntries(schema.document).flatMap { case (name, _, fields) =>
        fields.map(name -> _)
      }
      val requirements = fields.flatMap { case (typeName, field) =>
        val parent = schema.rootType.types.get(typeName)
        compileFieldSet(schema, typeName, field.name, field.directives, names.requires, parent)
      }
      val provisions   = fields.flatMap { case (typeName, field) =>
        val parent   = schema.rootType.types.get(typeName)
        val provided = parent.flatMap(tpe => Option(tpe.getFieldOrNull(field.name))).map(_._type.innerType)
        compileFieldSet(schema, typeName, field.name, field.directives, names.provides, provided)
      }
      val errors       = (requirements ::: provisions).collect { case Left(error) => error }

      if (errors.nonEmpty) Left(errors)
      else {
        val compiledRequirements = requirements.collect { case Right(selections) =>
          (schema.name, selections._1, selections._2) -> selections._3
        }
        val compiledProvisions   = provisions.collect { case Right(selections) =>
          (schema.name, selections._1, selections._2) -> selections._3
        }
        Right(FederationFieldSets(compiledRequirements, compiledProvisions))
      }
    }
  }

  private def compileFieldSet(
    schema: SchemaContribution,
    typeName: String,
    fieldName: String,
    directives: List[Directive],
    names: Set[String],
    startType: Option[__Type]
  ): Option[Either[String, (String, String, List[Selection])]] =
    directives.find(directive => names.contains(directive.name)).map { directive =>
      val prefix = s"[${schema.name}] Invalid @${directive.name} field set on '$typeName.$fieldName'"
      for {
        selections <- directiveFieldSet(directive).left.map(error => s"$prefix: $error")
        parent     <- startType.toRight(s"$prefix: the selected parent type does not exist.")
        _          <- validateFieldSetSelections(schema, parent, selections).left.map(error => s"$prefix: $error")
      } yield (typeName, fieldName, selections)
    }

  private def federationKeyDiagnostics(metadata: PreparedSchema): List[String] = {
    val schema = metadata.schema
    if (!schema.federation) Nil
    else {
      val definitions = objectLikeEntries(schema.document).map { case (name, directives, _) => name -> directives }

      definitions.flatMap { case (typeName, directives) =>
        directives.filter(directive => metadata.directives.key.contains(directive.name)).flatMap { directive =>
          val prefix = s"[${schema.name}] Invalid @${directive.name} field set on '$typeName'"
          val result = for {
            selections <- directiveFieldSet(directive).left.map(error => s"$prefix: $error")
            _          <- keyFields(selections)
                            .toRight(
                              s"$prefix: only fields without aliases, arguments, or directives can be selected."
                            )
            parent     <- schema.rootType.types
                            .get(typeName)
                            .toRight(s"$prefix: the selected parent type does not exist.")
            _          <- validateFieldSetSelections(schema, parent, selections).left.map(error => s"$prefix: $error")
          } yield ()

          result.fold(_ :: Nil, _ => Nil)
        }
      }
    }
  }

  private def validateFieldSetSelections(
    schema: SchemaContribution,
    parent: __Type,
    selections: List[Selection]
  ): Either[String, Unit] = {
    val document = Document(
      caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition(
        OperationType.Query,
        None,
        Nil,
        Nil,
        selections
      ) :: Nil,
      SourceMapper.empty
    )

    Validator.validateAll(document, schema.rootType.copy(queryType = parent)).left.map(_.msg)
  }

  private def nonRootTypes(schemas: List[PreparedSchema]): List[TypeEntry] =
    schemas.flatMap { metadata =>
      val schema    = metadata.schema
      val rootNames =
        schema.rootType.queryType.name.toSet ++
          schema.rootType.mutationType.flatMap(_.name).toSet ++
          schema.rootType.subscriptionType.flatMap(_.name).toSet

      schema.rootType.types.valuesIterator
        .filterNot(tpe =>
          tpe.name.exists(name =>
            rootNames.contains(name) ||
              schema.federation && metadata.directives.hiddenTypes.contains(name)
          )
        )
        .flatMap(tpe => tpe.name.map(name => typeEntry(metadata, name, tpe)))
        .toList
    }

  private def typeEntry(metadata: PreparedSchema, name: String, tpe: __Type): TypeEntry = {
    val schema          = metadata.schema
    val definitions     = schema.document.typeDefinitions.filter(_.name == name)
    val extensions      = schema.document.typeExtensions.collect {
      case extension: ObjectTypeExtension if extension.name == name    =>
        extension.directives -> extension.fields
      case extension: InterfaceTypeExtension if extension.name == name =>
        extension.directives -> extension.fields
    }
    val directives      = definitions.flatMap(_.directives) ::: extensions.flatMap(_._1)
    val fields          = definitions.flatMap {
      case definition: ObjectTypeDefinition    => definition.fields
      case definition: InterfaceTypeDefinition => definition.fields
      case _                                   => Nil
    } ::: extensions.flatMap(_._2)
    val names           = metadata.directives
    val interfaceObject = schema.federation && hasDirective(directives, names.interfaceObject)
    val composedType    =
      if (interfaceObject && tpe.kind == __TypeKind.OBJECT) tpe.copy(kind = __TypeKind.INTERFACE)
      else tpe
    val entity          =
      if (schema.federation) {
        val keys = directives.flatMap(keyDirective(_, names))
        if (keys.nonEmpty) {
          val lookups =
            if (hasEntityLookup(schema, name))
              keys.collect {
                case key if key.resolvable =>
                  ComposedGraph.EntityLookup(
                    key.fields,
                    ComposedGraph.LookupOperation.FederationEntities(
                      if (declaresEntityLookup(schema, name)) Some(key.fields) else None
                    ),
                    if (interfaceObject) Some(name) else None
                  )
              }
            else Nil
          Some(EntityDefinition(keys.flatMap(_.fields.map(_.name)).toSet, lookups))
        } else None
      } else
        schema.lookups
          .find(_.typeName == name)
          .map { lookup =>
            val key = lookup.keyFields.map(ComposedGraph.KeyField(_, Nil))
            EntityDefinition(
              lookup.keyFields.toSet,
              compileLookup(schema, lookup).toList.map(ComposedGraph.EntityLookup(key, _))
            )
          }
    val typeExternal    = schema.federation && hasDirective(directives, names.external)
    val fed1Owned       = metadata.federation1ExtensionKeyCoordinates.collect { case (`name`, field) => field }
    val external        = (fields.collect {
      case field if schema.federation && hasDirective(field.directives, names.external) =>
        field.name
    }.toSet ++ (if (typeExternal) fields.map(_.name) else Nil)) -- fed1Owned
    val typeShareable   = schema.federation && hasDirective(directives, names.shareable)
    val keyFields       = entity.fold(Set.empty[String])(_.keyFields) ++ metadata.keyCoordinates.collect {
      case (`name`, field) => field
    }
    val shareable       = fields.collect {
      case field if schema.federation && hasDirective(field.directives, names.shareable) =>
        field.name
    }.toSet ++ keyFields ++ (if (typeShareable) fields.map(_.name) else Nil)
    val inaccessible    = schema.mapping.hiddenTypes.contains(name) ||
      schema.federation && hasDirective(directives, names.inaccessible)
    val hiddenFields    = fields.collect {
      case field if schema.federation && hasDirective(Some(field.directives), names.inaccessible) => field.name
    }.toSet ++ schema.mapping.hiddenFields.collect { case (`name`, field) => field }
    val hiddenArgs      = schema.mapping.hiddenArguments.collect { case (`name`, field, argument) =>
      field -> argument
    } ++ fields.iterator
      .flatMap(field =>
        field.args.iterator.collect {
          case argument if hasDirective(argument.directives, names.inaccessible) => field.name -> argument.name
        }
      )
      .toSet
    val hiddenInputs    = schema.mapping.hiddenInputFields.collect { case (`name`, field) => field } ++
      tpe.allInputFields.iterator.collect {
        case field if hasDirective(field.directives, names.inaccessible) => field.name
      }
    val hiddenEnums     = schema.mapping.hiddenEnumValues.collect { case (`name`, value) => value } ++
      tpe.allEnumValues.iterator.collect {
        case value if hasDirective(value.directives, names.inaccessible) => value.name
      }
    val overrides       = fields.flatMap { field =>
      directiveString(Some(field.directives), names.overrideDirective, "from").map(field.name -> _)
    }.toMap
    TypeEntry(
      schema.name,
      name,
      composedType,
      interfaceObject,
      entity,
      tpe.allFields.map(_.name).toSet -- external,
      shareable,
      inaccessible,
      hiddenFields,
      hiddenArgs,
      hiddenInputs,
      hiddenEnums,
      overrides,
      federationLinks(schema.document).nonEmpty,
      names.inaccessible,
      if (schema.federation) names.hidden else Set.empty
    )
  }

  private def compileLookup(
    schema: SchemaContribution,
    lookup: Lookup
  ): Option[ComposedGraph.LookupOperation.GraphQLQuery] =
    schema.rootType.queryType.allFields.find(_.name == lookup.field).flatMap { field =>
      val argumentTypes = field.allArgs.map(argument => argument.name -> argument._type).toMap
      val arguments     = lookup.arguments.toList.foldLeft(Option(List.empty[(String, ComposedGraph.LookupArgument)])) {
        case (compiled, (name, mapping)) =>
          for {
            values   <- compiled
            expected <- argumentTypes.get(name)
            value    <- compileArgument(mapping, expected)
          } yield (name -> value) :: values
      }
      val result        = lookup match {
        case _: Lookup.Single         => ComposedGraph.LookupResult.Single
        case value: Lookup.ListLookup =>
          value.correlation match {
            case Lookup.Correlation.Ordered       => ComposedGraph.LookupResult.Ordered
            case Lookup.Correlation.ByKey(fields) => ComposedGraph.LookupResult.ByKey(fields)
          }
      }
      arguments.map(values => ComposedGraph.LookupOperation.GraphQLQuery(lookup.field, values.reverse.toMap, result))
    }

  private def compileArgument(
    mapping: Lookup.Argument,
    expected: __Type
  ): Option[ComposedGraph.LookupArgument] = {
    val valueType = nullableType(expected)
    mapping match {
      case Lookup.Argument.Key(field)            =>
        Some(ComposedGraph.LookupArgument.Key(field, valueType))
      case Lookup.Argument.ObjectMapping(fields) =>
        val inputFields = valueType.allInputFields.map(field => field.name -> field._type).toMap
        fields
          .foldLeft(Option(List.empty[(String, ComposedGraph.LookupArgument)])) { case (compiled, (name, value)) =>
            for {
              values    <- compiled
              inputType <- inputFields.get(name)
              nested    <- compileArgument(value, inputType)
            } yield (name -> nested) :: values
          }
          .map(values => ComposedGraph.LookupArgument.ObjectMapping(values.reverse))
      case Lookup.Argument.Batch(value)          =>
        valueType.ofType
          .map(nullableType)
          .flatMap(compileArgument(value, _))
          .map(ComposedGraph.LookupArgument.Batch.apply)
    }
  }

  private def incompatibleTypeDiagnostics(
    types: List[TypeEntry],
    enumUsages: Map[String, EnumUsage]
  ): List[String] =
    types
      .groupBy(_.name)
      .toList
      .flatMap { case (name, entries) =>
        val kinds = entries.map(_.tpe.kind).distinct
        if (kinds.size > 1)
          List(s"[type $name] Kinds are incompatible between subgraphs: ${sources(entries)}.")
        else
          kinds.headOption.toList.flatMap {
            case __TypeKind.OBJECT | __TypeKind.INTERFACE => incompatibleObjectDiagnostics(name, entries)
            case __TypeKind.INPUT_OBJECT                  => incompatibleInputDiagnostics(name, entries)
            case __TypeKind.ENUM                          =>
              incompatibleEnumDiagnostics(
                name,
                entries,
                enumUsages.getOrElse(name, EnumUsage(input = false, output = false))
              )
            case __TypeKind.SCALAR                        => exactTypeDiagnostics(name, entries)
            case _                                        => Nil
          }
      }

  private def incompatibleObjectDiagnostics(name: String, entries: List[TypeEntry]): List[String] = {
    val fields =
      entries.flatMap(entry => entry.tpe.allFields.map(field => field.name -> (entry -> field))).groupBy(_._1)

    fields.toList.flatMap { case (fieldName, definitions) =>
      val values       = definitions.map(_._2)
      val ownedEntries = effectiveFieldProviders(fieldName, values.map(_._1))
      val owned        = values.filter(value => ownedEntries.exists(_.source == value._1.source))
      val shareable    = owned.nonEmpty && owned.forall { case (entry, _) =>
        !entry.federation2 || entry.shareableFields.contains(fieldName)
      }
      val compatible   = fieldsCompatible(values.map(_._2))
      val prefix       = s"[type $name.$fieldName]"
      overrideDiagnostics(
        prefix,
        values.map { case (entry, _) => entry.source -> entry.overrideFields.get(fieldName) }
      ) :::
        (if (!compatible) List(s"$prefix Definitions are incompatible between subgraphs: ${sources(values.map(_._1))}.")
         else Nil) :::
        (if (
           compatible && owned.size > 1 && owned.exists(_._1.federation2) &&
           entries.exists(_.tpe.kind == __TypeKind.OBJECT) && !shareable
         )
           List(
             s"$prefix Field is resolved by multiple subgraphs without compatible @shareable declarations: ${sources(owned.map(_._1))}."
           )
         else Nil)
    }
  }

  private def incompatibleInputDiagnostics(name: String, entries: List[TypeEntry]): List[String] = {
    val fields =
      entries.flatMap(entry => entry.tpe.allInputFields.map(field => field.name -> (entry -> field))).groupBy(_._1)

    fields.toList.flatMap { case (fieldName, definitions) =>
      val values      = definitions.map(_._2)
      val signatures  = values.map(value => value._2._type.toType() -> value._2.defaultValue).distinct
      val omittedFrom = entries.map(_.source).toSet -- values.map(_._1.source).toSet
      val required    = values.exists(value => !value._2._type.isNullable && value._2.defaultValue.isEmpty)
      if (signatures.size > 1)
        List(
          s"[type $name.$fieldName] Input field definitions are incompatible between subgraphs: ${sources(values.map(_._1))}."
        )
      else if (omittedFrom.nonEmpty && required)
        omittedFrom.toList.sorted.map(source =>
          s"[$source] Required input field '$name.$fieldName' is not declared by this subgraph."
        )
      else Nil
    }
  }

  private def incompatibleEnumDiagnostics(
    name: String,
    entries: List[TypeEntry],
    usage: EnumUsage
  ): List[String] =
    if (!usage.input || !usage.output) Nil
    else {
      val hiddenNames = entries.iterator.flatMap(_.inaccessibleEnumValues).toSet
      val valueSets   = entries.map(entry => entry.tpe.allEnumValues.map(_.name).toSet -- hiddenNames)
      if (valueSets.distinct.size > 1)
        List(s"[type $name] Input/output enum values are incompatible between subgraphs: ${sources(entries)}.")
      else Nil
    }

  private def exactTypeDiagnostics(name: String, entries: List[TypeEntry]): List[String] = {
    val hidden = entries.iterator.flatMap(_.hiddenDirectives).toSet
    if (entries.map(entry => typeSignature(sanitizeType(entry.tpe, identity, hidden))).distinct.size > 1)
      List(s"[type $name] Definitions are incompatible between subgraphs: ${sources(entries)}.")
    else Nil
  }

  private def incompatibleDirectiveDiagnostics(directives: List[(String, __Directive)]): List[String] =
    directives
      .groupBy(_._2.name)
      .collect {
        case (name, definitions) if definitions.map(entry => directiveSignature(entry._2)).distinct.size > 1 =>
          val sources = formatSources(definitions.map(_._1))
          s"[directive @$name] Definitions are incompatible between subgraphs: $sources."
      }
      .toList

  private def chooseCompatible(
    types: List[TypeEntry],
    enumUsages: Map[String, EnumUsage],
    all: => Map[String, __Type]
  ): Map[String, __Type] = {
    val inaccessibleTypes = types.iterator.filter(_.inaccessible).map(_.name).toSet
    val chosen            = types
      .groupBy(_.name)
      .flatMap { case (name, entries) =>
        val sorted = entries.sortBy(_.source)
        if (entries.exists(_.inaccessible)) None
        else
          sorted.headOption.map { base =>
            val rewrite = rewriteType(_: __Type, all)
            val chosen  = base.tpe.kind match {
              case __TypeKind.OBJECT | __TypeKind.INTERFACE => mergeObject(sorted, rewrite, inaccessibleTypes)
              case __TypeKind.UNION                         => mergeUnion(sorted, rewrite, inaccessibleTypes)
              case __TypeKind.INPUT_OBJECT                  => mergeInputObject(sorted, rewrite)
              case __TypeKind.ENUM                          =>
                mergeEnum(sorted, enumUsages.getOrElse(name, EnumUsage(input = false, output = false)), rewrite)
              case _                                        => sanitizeType(base.tpe, rewrite, hiddenDirectives(sorted))
            }
            name -> chosen
          }
      }
    val interfaceObjects  = types.iterator.filter(_.interfaceObject).map(_.name).toSet
    val expanded          = chosen.map { case (name, tpe) =>
      if (tpe.kind != __TypeKind.OBJECT) name -> tpe
      else {
        val inherited = tpe
          .interfaces()
          .getOrElse(Nil)
          .flatMap(_.name)
          .filter(interfaceObjects)
          .flatMap(interfaceName => chosen.get(interfaceName).toList.flatMap(_.allFields))
        val existing  = tpe.allFields.map(_.name).toSet
        val fields    = tpe.allFields ::: inherited.filterNot(field => existing.contains(field.name))
        name -> tpe.copy(fields =
          args => Some(if (args.includeDeprecated.getOrElse(false)) fields else fields.filterNot(_.isDeprecated))
        )
      }
    }
    resolveComposedReferences(expanded)
  }

  private def resolveComposedReferences(types: Map[String, __Type]): Map[String, __Type] = {
    def resolve(references: List[__Type], candidates: => Map[String, __Type]): List[__Type] =
      references.flatMap(_.name).distinct.sorted.flatMap(candidates.get)

    lazy val objects: Map[String, __Type]    = types.collect {
      case (name, tpe) if tpe.kind == __TypeKind.OBJECT =>
        name -> tpe.copy(interfaces = () => tpe.interfaces().map(resolve(_, composed)))
    }
    lazy val interfaces: Map[String, __Type] = types.collect {
      case (name, tpe) if tpe.kind == __TypeKind.INTERFACE =>
        name -> tpe.copy(
          interfaces = () => tpe.interfaces().map(resolve(_, composed)),
          possibleTypes = tpe.possibleTypes.map(resolve(_, objects))
        )
    }
    lazy val unions: Map[String, __Type]     = types.collect {
      case (name, tpe) if tpe.kind == __TypeKind.UNION =>
        name -> tpe.copy(possibleTypes = tpe.possibleTypes.map(resolve(_, objects)))
    }
    lazy val composed: Map[String, __Type]   =
      types.filterNot { case (_, tpe) =>
        tpe.kind == __TypeKind.OBJECT || tpe.kind == __TypeKind.INTERFACE || tpe.kind == __TypeKind.UNION
      } ++ objects ++ interfaces ++ unions

    composed
  }

  private def mergeObject(
    entries: List[TypeEntry],
    rewrite: __Type => __Type,
    inaccessibleTypes: Set[String]
  ): __Type = {
    val base          = entries.headOption.map(_.tpe).getOrElse(__Type(__TypeKind.OBJECT))
    val hidden        = hiddenDirectives(entries)
    val fields        = entries
      .flatMap(entry => entry.tpe.allFields.map(field => field.name -> (entry -> field)))
      .groupBy(_._1)
      .toList
      .sortBy(_._1)
      .flatMap { case (fieldName, definitions) =>
        val values     = definitions.map(_._2)
        val visible    =
          if (values.exists { case (entry, _) => entry.inaccessibleFields.contains(fieldName) }) Nil else values
        val providers  = effectiveFieldProviders(fieldName, visible.map(_._1))
        val ordered    = visible.sortBy { case (entry, _) => (!providers.exists(_.source == entry.source), entry.source) }
        val hiddenArgs = values.iterator.flatMap { case (entry, _) =>
          entry.inaccessibleArguments.collect { case (`fieldName`, argument) => argument }
        }.toSet
        (if (providers.nonEmpty) ordered.headOption else None).map { case (_, field) =>
          val mergedType = ordered.map(_._2._type).reduceOption(mergeOutputType).getOrElse(field._type)
          sanitizeField(field.copy(`type` = () => mergedType), rewrite, hidden, hiddenArgs)
        }
      }
    val interfaces    = mergeReferencedTypes(entries, _.interfaces().getOrElse(Nil), inaccessibleTypes)
    val possibleTypes = mergeReferencedTypes(entries, _.possibleTypes.getOrElse(Nil), inaccessibleTypes)

    sanitizeType(base, rewrite, hidden).copy(
      fields = args => Some(if (args.includeDeprecated.getOrElse(false)) fields else fields.filterNot(_.isDeprecated)),
      interfaces = () => Some(interfaces),
      possibleTypes = if (base.kind == __TypeKind.INTERFACE) Some(possibleTypes) else base.possibleTypes
    )
  }

  private def mergeUnion(
    entries: List[TypeEntry],
    rewrite: __Type => __Type,
    inaccessibleTypes: Set[String]
  ): __Type = {
    val base    = entries.headOption.map(_.tpe).getOrElse(__Type(__TypeKind.UNION))
    val hidden  = hiddenDirectives(entries)
    val members = mergeReferencedTypes(entries, _.possibleTypes.getOrElse(Nil), inaccessibleTypes)
    sanitizeType(base, rewrite, hidden).copy(possibleTypes = Some(members))
  }

  private def mergeReferencedTypes(
    entries: List[TypeEntry],
    references: __Type => List[__Type],
    inaccessibleTypes: Set[String]
  ): List[__Type] = {
    val values = entries.flatMap(entry => references(entry.tpe))
    values
      .flatMap(_.name)
      .distinct
      .sorted
      .filterNot(inaccessibleTypes)
      .flatMap(name => values.find(_.name.contains(name)))
  }

  private def mergeInputObject(entries: List[TypeEntry], rewrite: __Type => __Type): __Type = {
    val base        = entries.headOption.map(_.tpe).getOrElse(__Type(__TypeKind.INPUT_OBJECT))
    val hidden      = hiddenDirectives(entries)
    val hiddenNames = entries.iterator.flatMap(_.inaccessibleInputFields).toSet
    val commonNames =
      entries.map(_.tpe.allInputFields.map(_.name).toSet).reduceOption(_ intersect _).getOrElse(Set.empty)
    val fields      = commonNames.toList.sorted.flatMap { name =>
      entries.iterator
        .flatMap(_.tpe.allInputFields)
        .find(field => field.name == name && !hiddenNames.contains(name))
        .map(field =>
          field.copy(
            `type` = () => rewrite(field._type),
            directives = filterFederationDirectives(field.directives, hidden)
          )
        )
    }
    sanitizeType(base, rewrite, hidden).copy(
      inputFields =
        args => Some(if (args.includeDeprecated.getOrElse(false)) fields else fields.filterNot(_.isDeprecated))
    )
  }

  private def mergeEnum(
    entries: List[TypeEntry],
    usage: EnumUsage,
    rewrite: __Type => __Type
  ): __Type = {
    val base        = entries.headOption.map(_.tpe).getOrElse(__Type(__TypeKind.ENUM))
    val hidden      = hiddenDirectives(entries)
    val hiddenNames = entries.iterator.flatMap(_.inaccessibleEnumValues).toSet
    val visible     = entries.map(_.tpe.allEnumValues.filterNot(value => hiddenNames.contains(value.name)))
    val names       =
      if (usage.input) visible.map(_.map(_.name).toSet).reduceOption(_ intersect _).getOrElse(Set.empty)
      else visible.flatMap(_.map(_.name)).toSet
    val values      = names.toList.sorted
      .flatMap(name => visible.iterator.flatten.find(_.name == name))
      .map(value => value.copy(directives = filterFederationDirectives(value.directives, hidden)))
    sanitizeType(base, rewrite, hidden).copy(
      enumValues =
        args => Some(if (args.includeDeprecated.getOrElse(false)) values else values.filterNot(_.isDeprecated))
    )
  }

  private def effectiveRootProviders(entries: List[RootFieldEntry]): List[RootFieldEntry] = {
    val overridden = entries.flatMap(_.overrideFrom).toSet
    entries.filterNot(entry => entry.external || overridden.contains(entry.source))
  }

  private def effectiveFieldProviders(field: String, entries: List[TypeEntry]): List[TypeEntry] = {
    val owned      = entries.filter(_.ownedFields.contains(field))
    val overridden = owned.flatMap(_.overrideFields.get(field)).toSet
    owned.filterNot(entry => overridden.contains(entry.source))
  }

  private def interfaceOverrideTargets(parent: String, field: String, entries: List[TypeEntry]): Set[String] = {
    val implementations = entries.collect {
      case entry
          if entry.tpe.kind == __TypeKind.OBJECT && entry.tpe.interfaces().exists(_.exists(_.name.contains(parent))) =>
        entry.name
    }.toSet
    entries.iterator
      .filter(entry => implementations.contains(entry.name))
      .flatMap(_.overrideFields.get(field))
      .toSet
  }

  private def overrideDiagnostics(prefix: String, declarations: List[(String, Option[String])]): List[String] = {
    val overrides         = declarations.collect { case (source, Some(from)) => source -> from }
    val invalid           = overrides.collect {
      case (source, from) if source == from => s"$prefix Subgraph '$source' cannot @override itself."
    }
    val overridingSources = overrides.map(_._1).distinct.sorted
    val competing         =
      if (overridingSources.size > 1)
        List(
          s"$prefix Subgraphs ${formatSources(overridingSources)} declare @override for the field."
        )
      else Nil
    invalid ::: competing
  }

  private def visibilityDiagnostics(
    roots: List[RootFieldEntry],
    types: List[TypeEntry]
  ): List[String] = {
    val inaccessibleTypes   = types.filter(_.inaccessible).map(_.name).toSet
    val inaccessibleFields  = types.iterator.flatMap(entry => entry.inaccessibleFields.map(entry.name -> _)).toSet
    val inaccessibleInputs  =
      types.iterator.flatMap(entry => entry.inaccessibleInputFields.map(entry.name -> _)).toSet
    val inaccessibleRoots   = roots.iterator
      .filter(_.inaccessible)
      .map(entry => entry.operation -> entry.field.name)
      .toSet
    val rootHiddenArguments = roots
      .groupBy(entry => entry.operation -> entry.field.name)
      .map { case (coordinate, entries) => coordinate -> entries.iterator.flatMap(_.inaccessibleArguments).toSet }
    val hiddenArguments     = types.iterator
      .flatMap(entry =>
        entry.inaccessibleArguments.map { case (field, argument) =>
          (entry.name, field, argument)
        }
      )
      .toSet
    val rootOutputErrors    = roots.collect {
      case entry
          if !inaccessibleRoots.contains(entry.operation -> entry.field.name) &&
            entry.field._type.innerType.name.exists(inaccessibleTypes.contains) =>
        s"[${entry.source}] Field '${entry.field.name}' must be @inaccessible because its return type is inaccessible."
    }
    val rootArgumentErrors  = roots.flatMap { entry =>
      entry.field.allArgs.collect {
        case argument
            if !rootHiddenArguments.getOrElse(entry.operation -> entry.field.name, Set.empty).contains(argument.name) &&
              argument._type.innerType.name.exists(inaccessibleTypes.contains) =>
          s"[${entry.source}] Argument '${entry.field.name}.${argument.name}' must be @inaccessible because its input type is inaccessible."
      }
    }
    val fieldOutputErrors   = types.flatMap { entry =>
      entry.tpe.allFields.collect {
        case field
            if !inaccessibleFields.contains(entry.name -> field.name) &&
              field._type.innerType.name.exists(inaccessibleTypes.contains) =>
          s"[${entry.source}] Field '${entry.name}.${field.name}' must be @inaccessible because its return type is inaccessible."
      }
    }
    val fieldArgumentErrors = types.flatMap { entry =>
      entry.tpe.allFields.flatMap { field =>
        field.allArgs.collect {
          case argument
              if !hiddenArguments.contains((entry.name, field.name, argument.name)) &&
                argument._type.innerType.name.exists(inaccessibleTypes.contains) =>
            s"[${entry.source}] Argument '${entry.name}.${field.name}.${argument.name}' must be @inaccessible because its input type is inaccessible."
        }
      }
    }
    val inputFieldErrors    = types.flatMap { entry =>
      entry.tpe.allInputFields.collect {
        case field
            if !inaccessibleInputs.contains(entry.name -> field.name) &&
              field._type.innerType.name.exists(inaccessibleTypes.contains) =>
          s"[${entry.source}] Input field '${entry.name}.${field.name}' must be @inaccessible because its input type is inaccessible."
      }
    }
    rootOutputErrors ::: rootArgumentErrors ::: fieldOutputErrors ::: fieldArgumentErrors ::: inputFieldErrors
  }

  private def compatibleField(left: __Field, right: __Field): Boolean = {
    val leftArgs  = left.allArgs.map(argument => argument.name -> argument).toMap
    val rightArgs = right.allArgs.map(argument => argument.name -> argument).toMap
    compatibleOutputType(left._type, right._type) && leftArgs.keySet == rightArgs.keySet &&
    leftArgs.forall { case (name, argument) =>
      rightArgs
        .get(name)
        .exists(other => argument._type.toType() == other._type.toType() && argument.defaultValue == other.defaultValue)
    }
  }

  private def fieldsCompatible(fields: List[__Field]): Boolean =
    fields.combinations(2).forall {
      case left :: right :: Nil => compatibleField(left, right)
      case _                    => true
    }

  private def compatibleOutputType(left: __Type, right: __Type): Boolean =
    (left.kind, right.kind) match {
      case (__TypeKind.NON_NULL, _)                    => left.ofType.exists(compatibleOutputType(_, right))
      case (_, __TypeKind.NON_NULL)                    => right.ofType.exists(compatibleOutputType(left, _))
      case (__TypeKind.LIST, __TypeKind.LIST)          =>
        (left.ofType, right.ofType) match {
          case (Some(a), Some(b)) => compatibleOutputType(a, b)
          case _                  => false
        }
      case (__TypeKind.LIST, _) | (_, __TypeKind.LIST) => false
      case _                                           =>
        val leftPossible  = left.possibleTypeNames
        val rightPossible = right.possibleTypeNames
        left.name == right.name && left.kind == right.kind ||
        leftPossible.nonEmpty && rightPossible.nonEmpty && (leftPossible intersect rightPossible).nonEmpty
    }

  private def mergeOutputType(left: __Type, right: __Type): __Type =
    (left.kind, right.kind) match {
      case (__TypeKind.NON_NULL, __TypeKind.NON_NULL) =>
        (left.ofType, right.ofType) match {
          case (Some(a), Some(b)) => left.copy(ofType = Some(mergeOutputType(a, b)))
          case _                  => left
        }
      case (__TypeKind.NON_NULL, _)                   => left.ofType.map(mergeOutputType(_, right)).getOrElse(right)
      case (_, __TypeKind.NON_NULL)                   => right.ofType.map(mergeOutputType(left, _)).getOrElse(left)
      case (__TypeKind.LIST, __TypeKind.LIST)         =>
        (left.ofType, right.ofType) match {
          case (Some(a), Some(b)) => left.copy(ofType = Some(mergeOutputType(a, b)))
          case _                  => left
        }
      case _                                          =>
        val leftPossible  = left.possibleTypeNames
        val rightPossible = right.possibleTypeNames
        if (leftPossible.nonEmpty && leftPossible.subsetOf(rightPossible)) right
        else if (rightPossible.nonEmpty && rightPossible.subsetOf(leftPossible)) left
        else left
    }

  private def enumUsageByName(schemas: List[SchemaContribution]): Map[String, EnumUsage] = {
    val allTypes = schemas.flatMap(_.rootType.types.values)
    val inputs   = allTypes.iterator.flatMap { tpe =>
      tpe.allInputFields.iterator.flatMap(_._type.innerType.name) ++
        tpe.allFields.iterator.flatMap(_.allArgs.iterator.flatMap(_._type.innerType.name))
    }.toSet
    val outputs  = allTypes.iterator.flatMap(_.allFields.iterator.flatMap(_._type.innerType.name)).toSet

    (inputs ++ outputs).iterator.map(name => name -> EnumUsage(inputs.contains(name), outputs.contains(name))).toMap
  }

  private def hiddenDirectives(entries: List[TypeEntry]): Set[String] =
    entries.iterator.flatMap(_.hiddenDirectives).toSet

  private def sources(entries: List[TypeEntry]): String =
    formatSources(entries.map(_.source))

  private def formatSources(sources: Iterable[String]): String =
    sources.toList.distinct.sorted.map(source => s"'$source'").mkString(", ")

  private def hasDirective(directives: List[Directive], names: Set[String]): Boolean =
    directives.exists(directive => names.contains(directive.name))

  private def hasDirective(directives: Option[List[Directive]], names: Set[String]): Boolean =
    directives.exists(_.exists(directive => names.contains(directive.name)))

  private def directiveString(
    directives: Option[List[Directive]],
    names: Set[String],
    argument: String
  ): Option[String] =
    directives.iterator.flatten
      .find(directive => names.contains(directive.name))
      .flatMap(_.arguments.get(argument))
      .collect { case StringValue(value) => value }

  private def sanitizeType(tpe: __Type, rewrite: __Type => __Type, hiddenDirectives: Set[String]): __Type =
    tpe.copy(
      directives = filterFederationDirectives(tpe.directives, hiddenDirectives),
      fields = args => tpe.fields(args).map(_.map(sanitizeField(_, rewrite, hiddenDirectives)))
    )

  private def sanitizeField(
    field: __Field,
    rewrite: __Type => __Type,
    hiddenDirectives: Set[String],
    inaccessibleArguments: Set[String] = Set.empty
  ): __Field =
    field.copy(
      `type` = () => rewrite(field.`type`()),
      args = args =>
        field.args(args).filterNot(value => inaccessibleArguments.contains(value.name)).map { value =>
          value.copy(
            `type` = () => rewrite(value._type),
            directives = filterFederationDirectives(value.directives, hiddenDirectives)
          )
        },
      directives = filterFederationDirectives(field.directives, hiddenDirectives)
    )

  private def filterFederationDirectives(
    directives: Option[List[Directive]],
    hiddenDirectives: Set[String]
  ): Option[List[Directive]] =
    if (hiddenDirectives.isEmpty) directives
    else directives.map(_.filterNot(directive => hiddenDirectives.contains(directive.name))).filter(_.nonEmpty)

  private def rewriteType(tpe: __Type, types: => Map[String, __Type]): __Type =
    tpe.ofType match {
      case Some(ofType) => tpe.copy(ofType = Some(rewriteType(ofType, types)))
      case None         => tpe.name.flatMap(types.get).getOrElse(tpe)
    }

  private def chooseCompatibleDirectives(directives: List[(String, __Directive)]): List[__Directive] =
    directives
      .groupBy(_._2.name)
      .toList
      .sortBy(_._1)
      .flatMap { case (_, definitions) => definitions.sortBy(_._1).headOption.map(_._2) }

  private def makeRootType(name: String, fields: List[RootFieldEntry], rewrite: __Type => __Type): __Type = {
    val sorted = fields
      .map(entry => sanitizeField(entry.field, rewrite, entry.hiddenDirectives, entry.inaccessibleArguments))
      .sortBy(_.name)
    __Type(
      kind = __TypeKind.OBJECT,
      name = Some(name),
      fields = args => Some(if (args.includeDeprecated.getOrElse(false)) sorted else sorted.filterNot(_.isDeprecated))
    )
  }

  private def typeSignature(tpe: __Type): String =
    tpe.toTypeDefinition.fold(tpe.kind.toString)(definition => render(normalize(definition)))

  private def directiveSignature(directive: __Directive): String =
    render(
      directive.toDirectiveDefinition.copy(
        description = None,
        args = directive.toDirectiveDefinition.args.map(normalize).sortBy(_.name)
      )
    )

  private def normalize(definition: TypeDefinition): TypeDefinition =
    definition match {
      case ObjectTypeDefinition(_, name, interfaces, directives, fields)    =>
        ObjectTypeDefinition(
          None,
          name,
          interfaces.sortBy(_.name),
          normalizeDirectives(directives),
          fields.map(normalize).sortBy(_.name)
        )
      case InterfaceTypeDefinition(_, name, interfaces, directives, fields) =>
        InterfaceTypeDefinition(
          None,
          name,
          interfaces.sortBy(_.name),
          normalizeDirectives(directives),
          fields.map(normalize).sortBy(_.name)
        )
      case InputObjectTypeDefinition(_, name, directives, fields)           =>
        InputObjectTypeDefinition(None, name, normalizeDirectives(directives), fields.map(normalize).sortBy(_.name))
      case EnumTypeDefinition(_, name, directives, values)                  =>
        EnumTypeDefinition(
          None,
          name,
          normalizeDirectives(directives),
          values
            .map(value => value.copy(description = None, directives = normalizeDirectives(value.directives)))
            .sortBy(
              _.enumValue
            )
        )
      case UnionTypeDefinition(_, name, directives, members)                =>
        UnionTypeDefinition(None, name, normalizeDirectives(directives), members.sorted)
      case ScalarTypeDefinition(_, name, directives)                        =>
        ScalarTypeDefinition(None, name, normalizeDirectives(directives))
    }

  private def normalize(field: FieldDefinition): FieldDefinition =
    field.copy(
      description = None,
      args = field.args.map(normalize).sortBy(_.name),
      directives = normalizeDirectives(field.directives)
    )

  private def normalize(input: InputValueDefinition): InputValueDefinition =
    input.copy(description = None, directives = normalizeDirectives(input.directives))

  private def normalizeDirectives(directives: List[Directive]): List[Directive] =
    directives
      .map(directive => directive.copy(arguments = ListMap(directive.arguments.toList.sortBy(_._1): _*)))
      .sortBy(directive =>
        directive.name -> directive.arguments.iterator.map { case (name, value) =>
          s"$name=${value.toInputString}"
        }.mkString
      )

  private def render(definition: caliban.parsing.adt.Definition): String =
    DocumentRenderer.renderCompact(Document(definition :: Nil, SourceMapper.empty))

  private final case class FederationDirectiveNames(
    key: Set[String],
    external: Set[String],
    extendsDirective: Set[String],
    shareable: Set[String],
    inaccessible: Set[String],
    overrideDirective: Set[String],
    requires: Set[String],
    provides: Set[String],
    interfaceObject: Set[String],
    authenticated: Set[String],
    requiresScopes: Set[String],
    policy: Set[String],
    unavailableSecurity: Map[String, String],
    context: Set[String],
    fromContext: Set[String],
    hidden: Set[String],
    hiddenTypes: Set[String]
  )

  private final case class ImportedName(name: String, alias: String, directive: Boolean)

  private final case class FeatureVersion(major: Int, minor: Int) {
    def atLeast(requiredMajor: Int, requiredMinor: Int): Boolean =
      major > requiredMajor || major == requiredMajor && minor >= requiredMinor
  }

  private final case class LinkedFeature(
    directive: Directive,
    identity: String,
    name: String,
    version: FeatureVersion,
    namespace: String,
    imports: List[ImportedName]
  ) {
    def directiveNames(name: String): Set[String] = {
      val imported  = imports.collect { case value if value.directive && value.name == name => value.alias }.toSet
      val qualified = if (this.name == name) Set(namespace) else Set(s"${namespace}__$name")
      imported ++ qualified
    }
  }

  private def federationDirectiveNames(document: Document): FederationDirectiveNames = {
    val links                                                                    = linkedFeatures(document)
    val federation                                                               = links.filter(_.identity == FederationIdentity)
    val security                                                                 = links.filter(feature => SecurityFeatureIdentities.contains(feature.identity))
    val relevant                                                                 = federation ::: security
    val imports                                                                  = relevant.flatMap(_.imports)
    val namespaces                                                               = relevant.iterator.map(_.namespace).toSet ++
      (if (links.nonEmpty) Set("link") else Set.empty)
    val namespacePrefix                                                          = namespaces.map(_ + "__")
    val hiddenTypes                                                              = document.typeDefinitions.iterator
      .map(_.name)
      .filter(name => namespacePrefix.exists(name.startsWith))
      .toSet ++ imports.collect { case value if !value.directive => value.alias } ++
      Set("_Any", "_Entity", "_FieldSet", "_Service")
    def federationNames(name: String, federation1: Boolean = false): Set[String] =
      federation.iterator.flatMap(_.directiveNames(name)).toSet ++
        (if (federation1 && federation.isEmpty) Set(name) else Set.empty)
    def securityNames(
      name: String,
      federationMajor: Int,
      federationMinor: Int,
      identity: String
    ): Set[String] =
      federation.iterator
        .filter(_.version.atLeast(federationMajor, federationMinor))
        .flatMap(_.directiveNames(name))
        .toSet ++
        security.iterator
          .filter(feature => feature.identity == identity && feature.version == FeatureVersion(0, 1))
          .flatMap(_.directiveNames(name))
          .toSet
    def unavailableSecurityNames(
      name: String,
      federationMajor: Int,
      federationMinor: Int,
      identity: String
    ): Set[String] =
      federation.iterator
        .filterNot(_.version.atLeast(federationMajor, federationMinor))
        .flatMap(_.directiveNames(name))
        .toSet ++
        security.iterator
          .filter(feature => feature.identity == identity && feature.version != FeatureVersion(0, 1))
          .flatMap(_.directiveNames(name))
          .toSet

    val keyNames            = federationNames("key", federation1 = true)
    val externalNames       = federationNames("external", federation1 = true)
    val extendsNames        = federationNames("extends", federation1 = true)
    val shareableNames      = federationNames("shareable")
    val inaccessibleNames   = federationNames("inaccessible")
    val overrideNames       = federationNames("override")
    val requiresNames       = federationNames("requires", federation1 = true)
    val providesNames       = federationNames("provides", federation1 = true)
    val interfaceObjects    = federationNames("interfaceObject")
    val authenticated       = securityNames("authenticated", 2, 5, AuthenticatedIdentity)
    val requiresScopes      = securityNames("requiresScopes", 2, 5, RequiresScopesIdentity)
    val policy              = securityNames("policy", 2, 6, PolicyIdentity)
    val unavailableSecurity =
      unavailableSecurityNames("authenticated", 2, 5, AuthenticatedIdentity).map(_ -> "@authenticated").toMap ++
        unavailableSecurityNames("requiresScopes", 2, 5, RequiresScopesIdentity)
          .map(_ -> "@requiresScopes")
          .toMap ++
        unavailableSecurityNames("policy", 2, 6, PolicyIdentity).map(_ -> "@policy").toMap
    val context             = federationNames("context")
    val fromContext         = federationNames("fromContext")
    val hiddenDirectives    = Set("link") ++ keyNames ++ externalNames ++ extendsNames ++ shareableNames ++
      inaccessibleNames ++ overrideNames ++ requiresNames ++ providesNames ++ interfaceObjects ++
      authenticated ++ requiresScopes ++ policy ++ unavailableSecurity.keySet ++ context ++ fromContext ++
      document.directiveDefinitions.iterator.map(_.name).filter(name => namespacePrefix.exists(name.startsWith))

    FederationDirectiveNames(
      keyNames,
      externalNames,
      extendsNames,
      shareableNames,
      inaccessibleNames,
      overrideNames,
      requiresNames,
      providesNames,
      interfaceObjects,
      authenticated,
      requiresScopes,
      policy,
      unavailableSecurity,
      context,
      fromContext,
      hiddenDirectives,
      hiddenTypes
    )
  }

  private val FederationIdentity        = "https://specs.apollo.dev/federation"
  private val AuthenticatedIdentity     = "https://specs.apollo.dev/authenticated"
  private val RequiresScopesIdentity    = "https://specs.apollo.dev/requiresScopes"
  private val PolicyIdentity            = "https://specs.apollo.dev/policy"
  private val SecurityFeatureIdentities = Set(AuthenticatedIdentity, RequiresScopesIdentity, PolicyIdentity)

  private def schemaLinkDirectives(document: Document): List[Directive] = {
    val schemaDirectives = document.schemaDefinition.toList.flatMap(_.directives) :::
      document.typeExtensions.collect { case extension: SchemaExtension => extension }.flatMap(_.directives)

    schemaDirectives.filter(_.name == "link")
  }

  private def linkedFeatures(document: Document): List[LinkedFeature] =
    schemaLinkDirectives(document).flatMap { directive =>
      directive.arguments.get("url").collect { case StringValue(url) => url }.flatMap { url =>
        val normalized   = url.takeWhile(character => character != '?' && character != '#').stripSuffix("/")
        val versionStart = normalized.lastIndexOf('/')
        val identity     = if (versionStart < 0) "" else normalized.substring(0, versionStart)
        val nameStart    = identity.lastIndexOf('/')
        val name         = if (nameStart < 0) "" else identity.substring(nameStart + 1)
        val version      = if (versionStart < 0) "" else normalized.substring(versionStart + 1)

        version match {
          case VersionPattern(major, minor) if name.nonEmpty =>
            val imports   = directive.arguments.get("import").toList.flatMap {
              case caliban.InputValue.ListValue(values) => values.flatMap(importedName)
              case _                                    => Nil
            }
            val namespace = directive.arguments
              .get("as")
              .collect { case StringValue(value) => value.stripPrefix("@") }
              .getOrElse(name)
            Some(
              LinkedFeature(
                directive,
                identity,
                name,
                FeatureVersion(major.toInt, minor.toInt),
                namespace,
                imports
              )
            )
          case _                                             => None
        }
      }
    }

  private val VersionPattern = "v([0-9]+)\\.([0-9]+)".r

  private def importedName(value: caliban.InputValue): Option[ImportedName] =
    value match {
      case StringValue(name)                      =>
        Some(ImportedName(name.stripPrefix("@"), name.stripPrefix("@"), name.startsWith("@")))
      case caliban.InputValue.ObjectValue(fields) =>
        fields.get("name").collect { case StringValue(name) => name }.map { name =>
          val alias = fields.get("as").collect { case StringValue(value) => value }.getOrElse(name)
          ImportedName(name.stripPrefix("@"), alias.stripPrefix("@"), name.startsWith("@"))
        }
      case _                                      => None
    }

  private def federationLinks(document: Document): List[Directive] =
    linkedFeatures(document).collect { case feature if feature.identity == FederationIdentity => feature.directive }

  private def keyDirective(
    directive: Directive,
    names: FederationDirectiveNames
  ): Option[FederationKey] =
    if (!names.key.contains(directive.name)) None
    else
      directiveFieldSet(directive).toOption.flatMap { selections =>
        keyFields(selections)
          .map(fields => FederationKey(fields, !directive.arguments.get("resolvable").contains(BooleanValue(false))))
      }

  private def federationKeyCoordinates(
    schema: SchemaContribution,
    names: FederationDirectiveNames
  ): Set[(String, String)] = {
    val definitions = objectLikeEntries(schema.document).map { case (name, directives, _) => name -> directives }

    definitions.iterator.flatMap { case (typeName, directives) =>
      directives.iterator
        .flatMap(keyDirective(_, names))
        .flatMap(key => keyCoordinates(schema.rootType, typeName, key.fields))
    }.toSet
  }

  private def federation1ExtensionKeyCoordinates(
    schema: SchemaContribution,
    names: FederationDirectiveNames
  ): Set[(String, String)] =
    if (federationLinks(schema.document).nonEmpty) Set.empty
    else {
      val extensions          = schema.document.typeExtensions.collect {
        case extension: ObjectTypeExtension    => extension.name -> extension.directives
        case extension: InterfaceTypeExtension => extension.name -> extension.directives
      }
      val extendedDefinitions = schema.document.typeDefinitions.collect {
        case definition: ObjectTypeDefinition if hasDirective(Some(definition.directives), names.extendsDirective)    =>
          definition.name -> definition.directives
        case definition: InterfaceTypeDefinition if hasDirective(Some(definition.directives), names.extendsDirective) =>
          definition.name -> definition.directives
      }

      (extensions ::: extendedDefinitions).iterator.flatMap { case (typeName, directives) =>
        directives.iterator
          .flatMap(keyDirective(_, names))
          .flatMap(_.fields.map(field => typeName -> field.name))
      }.toSet
    }

  private def keyCoordinates(
    rootType: RootType,
    typeName: String,
    fields: List[ComposedGraph.KeyField]
  ): List[(String, String)] =
    fields.flatMap { field =>
      val child = rootType.types
        .get(typeName)
        .flatMap(_.allFields.find(_.name == field.name))
        .flatMap(_._type.innerType.name)
      (typeName -> field.name) :: child.toList.flatMap(keyCoordinates(rootType, _, field.children))
    }

  private def directiveFieldSet(directive: Directive): Either[String, List[Selection]] =
    for {
      value      <- directive.arguments
                      .get("fields")
                      .collect { case StringValue(value) => value }
                      .toRight("the 'fields' argument must be a string.")
      selections <- parseFieldSet(value).toRight("the selection could not be parsed.")
    } yield selections

  private[gateway] def parseFieldSet(value: String): Option[List[Selection]] =
    Parser.parseQuery(s"{ $value }") match {
      case Right(document) => document.operationDefinition(None).map(_.selectionSet)
      case Left(_)         => None
    }

  private def keyFields(selections: List[Selection]): Option[List[ComposedGraph.KeyField]] =
    selections
      .foldLeft(Option(List.empty[ComposedGraph.KeyField])) { case (result, selection) =>
        for {
          fields <- result
          field  <- selection match {
                      case Selection.Field(None, name, arguments, directives, children, _)
                          if arguments.isEmpty && directives.isEmpty =>
                        keyFields(children).map(ComposedGraph.KeyField(name, _))
                      case _ => None
                    }
        } yield field :: fields
      }
      .map(_.reverse)

  private def hasEntityLookup(schema: SchemaContribution, entityType: String): Boolean =
    declaresEntityLookup(schema, entityType) ||
      schema.federation && !schema.rootType.queryType.allFields.exists(_.name == "_entities")

  private def declaresEntityLookup(schema: SchemaContribution, entityType: String): Boolean =
    schema.rootType.queryType.allFields.find(_.name == "_entities") match {
      case None        => false
      case Some(field) =>
        val acceptsRepresentations = field.allArgs.find(_.name == "representations").exists { argument =>
          argument._type.isList && argument._type.innerType.name.contains("_Any")
        }
        val returnsEntities        = field._type.isList && field._type.innerType.name.contains("_Entity")
        val includesEntity         = field._type.innerType.possibleTypes.exists(_.exists(_.name.contains(entityType)))
        acceptsRepresentations && returnsEntities && includesEntity
    }

  private def isTransportField(name: String): Boolean =
    name == "_entities" || name == "_service"

}
