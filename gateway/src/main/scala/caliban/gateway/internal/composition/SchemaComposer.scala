package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.GatewayBuildError.{ SchemaCompositionFailed, SubgraphLoadingFailed }
import caliban.gateway.SubgraphBuildError.{ InvalidTransformations, SchemaValidationFailed }
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.FederationCompilation._
import caliban.gateway.internal.composition.FederationCompilation.FederationDirective._
import caliban.introspection.adt._
import caliban.parsing.SourceMapper
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection }
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.Type.NamedType
import caliban.schema.RootType
import caliban.tools.RemoteSchema
import caliban.validation.{ SchemaValidator, Validator }
import caliban.Value.BooleanValue

import scala.collection.compat._

private[gateway] object SchemaComposer {
  import DirectiveComposition._
  import TypeComposition._

  def compose[R](subgraphs: List[(Subgraph[R], Document)]): Either[GatewayBuildError, ComposedGraph] = {
    val (failures, prepared) = subgraphs.partitionMap { case (subgraph, document) =>
      prepare(subgraph, document).left.map(SubgraphError(subgraph.name, _))
    }
    if (failures.nonEmpty) Left(SubgraphLoadingFailed(failures.sortBy(_.diagnostics.mkString("\n"))))
    else new SchemaComposer(prepared).compose.left.map(errors => SchemaCompositionFailed(errors.distinct.sorted))
  }

  private def prepare[R](subgraph: Subgraph[R], document: Document): Either[SubgraphBuildError, PreparedSubgraph] = {
    val federation   = subgraph.source.federation
    val rootDocument = if (federation) withFederationQueryRoot(document) else document
    for {
      normalized    <-
        RemoteSchema
          .normalize(rootDocument, extensionsCanDefineTypes = federation)
          .left
          .map(SchemaValidationFailed(_))
      _             <- Either.cond(
                         federation || !normalized.rootType.queryType.allFields.exists(isEntityLookup),
                         (),
                         SubgraphBuildError.InvalidConfiguration(
                           List(
                             "The query root declares Federation entity transport. Use Subgraph.federation instead of Subgraph.graphql."
                           )
                         )
                       )
      names          = federationDirectiveNames(normalized.document, federation)
      mapping       <-
        SchemaMapping
          .compile(subgraph.name, normalized.rootType, names, subgraph.transformations)
          .left
          .map(InvalidTransformations(_))
      extensionTypes = federation1ExtensionTypes(document).map(mapping.clientType)
      transformed   <-
        if (mapping.nonEmpty)
          RemoteSchema
            .normalize(mapping.transform(normalized.document, names), extensionsCanDefineTypes = federation)
            .left
            .map(SchemaValidationFailed(_))
        else Right(normalized)
    } yield PreparedSubgraph(
      subgraph.name,
      transformed.rootType,
      DirectiveComposition.schemaDirectives(transformed.document),
      subgraph.lookups.map(mapping.transform),
      mapping,
      extensionTypes,
      names
    )
  }

  private[composition] final case class PreparedSubgraph(
    name: String,
    rootType: RootType,
    schemaDirectives: List[Directive],
    lookups: List[Lookup],
    mapping: SchemaMapping,
    federation1ExtensionTypes: Set[String],
    directiveNames: FederationDirectiveNames
  ) {
    lazy val directiveApplications: List[TypeSystemDirectiveApplication] =
      FederationCompilation.directiveApplications(rootType, schemaDirectives)

    lazy val keys: SubgraphKeys = subgraphKeys(this)

    def federation: Boolean = directiveNames.mode != SubgraphMode.Ordinary

    lazy val (federationErrors, federationApplications) =
      (for {
        application <- directiveApplications
        directive   <- application.directives
        coordinate   = application.coordinate
        result      <-
          directiveNames.resolved.get(directive.name).map(validateApplication(name, coordinate, _, directive)) ++
            directiveNames.unsupported.get(directive.name).map(message => Left(List(s"[$name] ${message(coordinate)}")))
      } yield result).partitionMap(identity)

    lazy val repeatedDirectives: List[String] = {
      val repeatable =
        rootType.additionalDirectives.map(definition => definition.name -> definition.isRepeatable).toMap ++
          directiveNames.resolved.map { case (local, member) => local -> member.definition.isRepeatable }
      directiveApplications.flatMap { application =>
        duplicates(application.directives.map(_.name))
          .filter(repeatable.get(_).contains(false))
          .map(directive =>
            s"[$name] Non-repeatable directive '@$directive' is applied more than once at '${application.coordinate.display}'."
          )
      }
    }

    def applications(member: FederationDirective): List[FederationApplication] =
      federationApplications.filter(_.member == member)

    private lazy val appliedAt: Set[(FederationDirective, Coordinate)] =
      federationApplications.map(application => application.member -> application.coordinate).toSet

    def applied(member: FederationDirective, coordinate: Coordinate): Boolean = appliedAt(member -> coordinate)

    lazy val overrides: Map[Coordinate, (FieldOverride, Option[String])] =
      applications(Override).map(application => application.coordinate -> fieldOverride(this, application)).toMap

    lazy val compiledLookups: List[Either[List[String], (String, EntityLookup)]] =
      lookups.map(lookup =>
        LookupCompilation
          .compile(this, lookup)
          .map(operation => lookup.typeName -> EntityLookup(lookup.keyFields.map(KeyField(_, Nil)), operation))
      )

    def isInterfaceObject(name: String): Boolean =
      !RootOperations.contains(name) && applied(InterfaceObject, TypeCoordinate(name, __DirectiveLocation.OBJECT))

    def entityLookups(name: String): List[EntityLookup] =
      if (RootOperations.contains(name)) Nil
      else if (!federation) compiledLookups.collect { case Right((`name`, lookup)) => lookup }
      else if (!hasEntityLookup(this, name)) Nil
      else
        keys.all.collect { case FederationKey(`name`, fields, true) =>
          EntityLookup(fields, LookupOperation.FederationEntities)
        }

    lazy val requiredFieldSets = fieldSets(Requires)
    lazy val providedFieldSets = fieldSets(Provides)

    private def fieldSets(member: FederationDirective): List[Either[String, (FieldCoordinate, List[Selection])]] =
      applications(member).flatMap {
        case application @ FederationApplication(at @ FieldCoordinate(typeName, fieldName), _, _) =>
          for {
            parent <- rootType.types.get(typeName)
            field  <- fieldDefinition(parent, fieldName)
            target  = if (member == Provides) field._type.innerType else parent
          } yield validateFieldSet(this, application, target)(Right(_)).map(at -> _)
        case _                                                                                    => None
      }

    lazy val contexts: Either[List[String], ContextCompilation.FederationContexts] = ContextCompilation.compile(this)

    lazy val costs: Either[List[String], CostMetadata] = CostCompilation.compile(this)

    lazy val diagnostics: List[String] =
      keys.diagnostics ::: overrides.values.toList.flatMap(_._2) ::: federationErrors.flatten :::
        repeatedDirectives ::: LookupCompilation.declarationDiagnostics(this) :::
        compiledLookups.flatMap(_.left.getOrElse(Nil)) ::: (requiredFieldSets ::: providedFieldSets).flatMap(
          _.left.toOption
        ) :::
        contexts.left.getOrElse(Nil) ::: costs.left.getOrElse(Nil)

    lazy val hidden: Set[Coordinate] =
      mapping.hidden ++ (applications(Inaccessible) ::: applications(FromContext)).map(_.coordinate)
  }

  private def withFederationQueryRoot(document: Document): Document = {
    val names         = document.typeDefinitions.iterator.map(_.name).toSet ++ document.typeExtensions.collect {
      case extension: ObjectTypeExtension => extension.name
    }
    val extendedQuery = document.typeExtensions.exists {
      case extension: SchemaExtension => extension.query.nonEmpty
      case _                          => false
    }

    def freeName(index: Int): String = {
      val name = if (index == 1) "Query" else s"Query$index"
      if (names.contains(name)) freeName(index + 1) else name
    }

    val rootName = document.schemaDefinition match {
      case Some(schema) if schema.query.isEmpty && !extendedQuery => Some(freeName(1))
      case None if !extendedQuery && !names("Query")              => Some("Query")
      case _                                                      => None
    }
    rootName.fold(document) { name =>
      val root        = ObjectTypeDefinition(
        None,
        name,
        Nil,
        Nil,
        List(FieldDefinition(None, ServiceField, Nil, NamedType("String", nonNull = true), Nil))
      )
      val definitions = document.definitions.map {
        case definition: SchemaDefinition if definition.query.isEmpty => definition.copy(query = Some(name))
        case definition                                               => definition
      }
      Document(definitions :+ root, document.sourceMapper)
    }
  }

  private def federation1ExtensionTypes(document: Document): Set[String] =
    document.typeExtensions.iterator.collect {
      case extension: ObjectTypeExtension    => extension.name
      case extension: InterfaceTypeExtension => extension.name
    }.toSet

  private val ProgressiveOverridePattern = raw"percent\((\d{1,2}(?:\.\d{1,8})?|100)\)".r
  private val CustomOverrideLabelPattern = raw"[a-zA-Z][a-zA-Z0-9_\-:./]*".r

  private def parseProgressiveOverrideLabel(label: String): Either[String, ProgressiveOverride] =
    label match {
      case ProgressiveOverridePattern(value) =>
        Right(ProgressiveOverride(OverrideLabel(label), Some(BigDecimal(value))))
      case CustomOverrideLabelPattern()      => Right(ProgressiveOverride(OverrideLabel(label), None))
      case _                                 => Left(s"Invalid Federation @override label '$label'.")
    }

  private def fieldOverride(subgraph: PreparedSubgraph, application: FederationApplication) = {
    val arguments = application.directive.arguments
    val at        = application.coordinate.display
    val label     = stringArgument(arguments, "label").map(parseProgressiveOverrideLabel)
    val error     =
      if (label.nonEmpty && !subgraph.directiveNames.supportsProgressiveOverride)
        Some(s"Federation @override(label:) is not available in the linked feature version at '$at'.")
      else label.flatMap(_.left.toOption).map(error => s"$error At '$at'.")
    FieldOverride(stringArgument(arguments, "from").getOrElse(""), label.flatMap(_.toOption)) ->
      error.map(error => s"[${subgraph.name}] $error")
  }

  private def validateFieldSet[A](subgraph: PreparedSubgraph, application: FederationApplication, parent: __Type)(
    shape: List[Selection] => Either[String, A]
  ): Either[String, A] = {
    val directive = application.directive
    val result    = for {
      selections <- stringArgument(directive.arguments, "fields")
                      .flatMap(parseFieldSet)
                      .toRight("the selection could not be parsed.")
      shaped     <- shape(selections)
      _          <- validateFieldSetSelections(subgraph, parent, selections)
    } yield shaped
    result.left.map(error =>
      s"[${subgraph.name}] Invalid @${directive.name} field set on '${application.coordinate.display}': $error"
    )
  }

  private def validateFieldSetSelections(
    subgraph: PreparedSubgraph,
    parent: __Type,
    selections: List[Selection]
  ): Either[String, Unit] = {
    val document = Document(
      OperationDefinition(OperationType.Query, None, Nil, Nil, selections) :: Nil,
      SourceMapper.empty
    )
    Validator.validateAll(document, subgraph.rootType.copy(queryType = parent)).left.map(_.msg)
  }

  private def subgraphKeys(subgraph: PreparedSubgraph): SubgraphKeys = {
    val (errors, keys) = subgraph
      .applications(Key)
      .collect { case application @ FederationApplication(TypeCoordinate(name, _), _, directive) =>
        subgraph.rootType.types.get(name).map { parent =>
          validateFieldSet(subgraph, application, parent)(
            plainFieldSet(_).toRight("only fields without aliases, arguments, or directives can be selected.")
          ).map(FederationKey(name, _, !directive.arguments.get("resolvable").contains(BooleanValue(false))))
        }
      }
      .flatten
      .partitionMap(identity)
    val keyFields      = keys.flatMap(key => collectKeyFields(subgraph.rootType, key.typeName, key.fields))
    val lookupKeys     = subgraph.lookups.flatMap(lookup => lookup.keyFields.map(FieldCoordinate(lookup.typeName, _)))
    SubgraphKeys(keys, errors, keyFields.toSet ++ lookupKeys, federation1ExtensionKeyFields(subgraph, keys))
  }

  private def federation1ExtensionKeyFields(
    subgraph: PreparedSubgraph,
    keys: List[FederationKey]
  ): Set[FieldCoordinate] =
    if (subgraph.directiveNames.mode != SubgraphMode.Federation1) Set.empty
    else {
      val extended = subgraph.federation1ExtensionTypes ++ subgraph.applications(Extends).map(_.coordinate.display)
      keys.iterator
        .filter(key => extended(key.typeName))
        .flatMap(key => key.fields.map(field => FieldCoordinate(key.typeName, field.name)))
        .toSet
    }

  private def collectKeyFields(rootType: RootType, typeName: String, fields: List[KeyField]): List[FieldCoordinate] =
    fields.flatMap { field =>
      val childType =
        rootType.types.get(typeName).flatMap(fieldDefinition(_, field.name)).flatMap(_._type.innerType.name)
      FieldCoordinate(typeName, field.name) :: childType.toList.flatMap(collectKeyFields(rootType, _, field.children))
    }

  private def hasEntityLookup(subgraph: PreparedSubgraph, entityType: String): Boolean =
    fieldDefinition(subgraph.rootType.queryType, EntitiesField).forall { field =>
      isEntityLookup(field) && field._type.innerType.possibleTypes.exists(_.exists(_.name.contains(entityType)))
    }

  private def isEntityLookup(field: __Field): Boolean =
    field.name == EntitiesField && field._type.isList && field._type.innerType.name.contains("_Entity") &&
      field.allArgs.exists(argument =>
        argument.name == RepresentationsArgument && argument._type.isList && argument._type.innerType.name
          .contains(AnyType)
      )

  private[composition] final case class SubgraphKeys(
    all: List[FederationKey],
    diagnostics: List[String],
    shared: Set[FieldCoordinate],
    federation1Owned: Set[FieldCoordinate]
  )

  private[composition] final case class FederationKey(typeName: String, fields: List[KeyField], resolvable: Boolean)

}

/**
 * Composes loaded subgraphs; all intermediate state is scoped to one composition.
 */
private[gateway] final class SchemaComposer private (subgraphs: List[SchemaComposer.PreparedSubgraph]) {
  import SchemaComposer._
  import DirectiveComposition._
  import TypeComposition._

  def compose: Either[List[String], ComposedGraph] = {
    val errors =
      sortedSubgraphs.flatMap(_.diagnostics) ::: composedDirectives.diagnostics ::: typeComposition.diagnostics
    if (errors.nonEmpty) Left(errors) else composedRootType(typeComposition.composed).flatMap(buildGraph)
  }

  private val sortedSubgraphs    = subgraphs.sortBy(_.name)
  private val composedDirectives = DirectiveComposition.compile(sortedSubgraphs)
  private val (sources, types)   = sortedSubgraphs.map { subgraph =>
    val visible = subgraph.rootType.types.filter { case (name, _) => !subgraph.directiveNames.hiddenTypes(name) }
    val source  = sourceSubgraph(subgraph, visible.keySet)
    source -> visible.map { case (name, tpe) => subgraphType(subgraph, source, name, tpe) }
  }.unzip
  private val typeComposition    =
    new TypeComposition(types.flatten, composedDirectives)

  private def sourceSubgraph(subgraph: PreparedSubgraph, visible: Set[String]): Source =
    new Source(
      subgraph.name,
      subgraph.mapping,
      subgraph.rootType.types,
      visible.iterator.map(name => name -> subgraph.entityLookups(name)).filter(_._2.nonEmpty).toMap,
      visible.filter(subgraph.isInterfaceObject),
      subgraph.requiredFieldSets.collect { case Right(fieldSet) => fieldSet }.toMap,
      subgraph.providedFieldSets.collect { case Right(fieldSet) => fieldSet }.toMap,
      subgraph.contexts.getOrElse(ContextCompilation.FederationContexts(Nil, Map.empty))
    )

  private def subgraphType(subgraph: PreparedSubgraph, source: Source, name: String, tpe: __Type): SubgraphType = {
    val isRoot = RootOperations.contains(name)

    def has(member: FederationDirective): Boolean =
      subgraph.applied(member, TypeCoordinate(name, typeLocation(tpe.kind)))

    val interfaceObject = subgraph.isInterfaceObject(name)
    val typeExternal    = !isRoot && has(FederationDirective.External)
    val typeShareable   = has(FederationDirective.Shareable)
    val composedType    =
      if (isRoot)
        tpe.copy(
          description = None,
          interfaces = () => None,
          fields =
            args => tpe.fields(args).map(_.filterNot(field => subgraph.federation && TransportFields(field.name)))
        )
      else if (interfaceObject) tpe.copy(kind = __TypeKind.INTERFACE)
      else tpe

    def subgraphField(field: __Field): SubgraphField = {
      val at       = FieldCoordinate(name, field.name)
      val external = typeExternal || subgraph.applied(External, at)
      SubgraphField(
        definition = field,
        owned = !external || !isRoot && subgraph.keys.federation1Owned(at),
        shareable = typeShareable || subgraph.applied(Shareable, at) || !isRoot && subgraph.keys.shared(at),
        overrideDirective = subgraph.overrides.get(at).map(_._1),
        contextualArguments = field.allArgs.iterator
          .map(_.name)
          .filter(argument => subgraph.applied(FromContext, ArgumentCoordinate(name, field.name, argument)))
          .toSet
      )
    }

    SubgraphType(
      subgraph = source,
      name = name,
      tpe = composedType,
      fields = composedType.allFields.map(subgraphField),
      subgraphMode = subgraph.directiveNames.mode
    )
  }

  private def buildGraph(rootType: RootType): Either[List[String], ComposedGraph] = {
    val graph       = new ComposedGraph(
      rootType = rootType,
      fieldRoutes = typeComposition.inactiveRoutes,
      progressiveRoutes = typeComposition.progressiveRoutes,
      sources = sources,
      possibleTypesByName = rootType.types.map { case (name, tpe) => name -> tpe.possibleTypeNames },
      costMetadata = CostCompilation.merge(sortedSubgraphs.flatMap(_.costs.toOption)),
      securityApplications = sortedSubgraphs.flatMap(SecurityCompilation.compile)
    )
    val diagnostics = invalidTransformationDiagnostics(rootType) :::
      composedDirectives.schemaDiagnostics(rootType) ::: SecurityCompilation.diagnostics(graph)

    if (diagnostics.nonEmpty) Left(diagnostics)
    else
      SchemaValidator
        .validateRootType(rootType)
        .left
        .map(error => List(s"[composition] ${error.getMessage}"))
        .map(_ => graph)
  }

  private def composedRootType(composedTypes: Map[String, __Type]): Either[List[String], RootType] = {
    val additionalTypes = composedTypes.toList.sortBy(_._1).collect {
      case (name, tpe) if !RootOperations.contains(name) => tpe
    } ::: composedDirectives.additionalTypes.filterNot(tpe => tpe.name.exists(composedTypes.contains))
    composedTypes.get("Query").toRight(List("[composition] No subgraph defines a query root type.")).map { query =>
      RootType(
        query,
        composedTypes.get("Mutation").filter(_.allFields.nonEmpty),
        composedTypes.get("Subscription").filter(_.allFields.nonEmpty),
        additionalTypes,
        composedDirectives.definitions(rewriteType(_, composedTypes))
      )
    }
  }

  private def invalidTransformationDiagnostics(rootType: RootType): List[String] =
    sortedSubgraphs.flatMap { subgraph =>
      subgraph.mapping.hidden.toList.collect {
        case FieldCoordinate(name, _)      => name
        case InputFieldCoordinate(name, _) => name
      }.flatMap { name =>
        rootType.types
          .get(name)
          .filter(tpe => tpe.allFields.isEmpty && tpe.allInputFields.isEmpty)
          .flatMap(tpe => TransformedKinds.get(tpe.kind))
          .map(kind => s"[${subgraph.name}] Transformation leaves $kind '$name' with no visible fields.")
      }
    }

  private val TransformedKinds: Map[__TypeKind, String] =
    Map(__TypeKind.OBJECT -> "object", __TypeKind.INTERFACE -> "interface", __TypeKind.INPUT_OBJECT -> "input object")

}
