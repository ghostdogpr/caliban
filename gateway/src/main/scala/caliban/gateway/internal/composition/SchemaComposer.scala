package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.GatewayBuildError.{ SchemaCompositionFailed, SubgraphLoadingFailed }
import caliban.gateway.SubgraphBuildError.{ InvalidTransformations, SchemaValidationFailed }
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.FederationCompilation._
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
import caliban.Value.{ BooleanValue, NullValue, StringValue }

import scala.collection.compat._
import scala.collection.mutable

private[gateway] object SchemaComposer {
  import DirectiveComposition._
  import TypeComposition._

  def compose[R](subgraphs: List[(Subgraph[R], Document)]): Either[GatewayBuildError, ComposedGraph] = {
    val prepared = subgraphs.map { case (subgraph, document) =>
      prepare(subgraph, document).left.map(SubgraphError(subgraph.name, _))
    }
    val failures = prepared.collect { case Left(error) => error }.sortBy(_.diagnostics.mkString("\n"))
    if (failures.nonEmpty) Left(SubgraphLoadingFailed(failures))
    else
      new SchemaComposer(prepared.collect { case Right(subgraph) => subgraph }).compose.left
        .map(errors => SchemaCompositionFailed(errors.distinct.sorted))
  }

  private def prepare[R](subgraph: Subgraph[R], document: Document): Either[SubgraphBuildError, PreparedSubgraph] = {
    val federation   = subgraph.source.federation
    val rootDocument = if (federation && !hasQueryRoot(document)) addFederationQueryRoot(document) else document
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
      mapping       <-
        SchemaMapping
          .compile(subgraph.name, normalized.rootType, normalized.document, federation, subgraph.transformations)
          .left
          .map(InvalidTransformations(_))
      extensionTypes = federation1ExtensionTypes(document).map(mapping.clientType)
      transformed   <- if (mapping.nonEmpty)
                         RemoteSchema
                           .normalize(mapping.transform(normalized.document), extensionsCanDefineTypes = federation)
                           .left
                           .map(SchemaValidationFailed(_))
                       else Right(normalized)
    } yield PreparedSubgraph(
      subgraph.name,
      transformed.rootType,
      transformed.document,
      federation,
      subgraph.lookups.map(mapping.transform),
      mapping,
      extensionTypes,
      if (mapping.nonEmpty) federationDirectiveNames(transformed.document) else mapping.directiveNames
    )
  }

  private[composition] final case class PreparedSubgraph(
    name: String,
    rootType: RootType,
    document: Document,
    federation: Boolean,
    lookups: List[Lookup],
    mapping: SchemaMapping,
    federation1ExtensionTypes: Set[String],
    directiveNames: FederationDirectiveNames
  ) {
    val rootNames: OperationRootNames = mapping.rootNames
  }

  private def hasQueryRoot(document: Document): Boolean = {
    val schemaExtensions     = document.typeExtensions.collect { case extension: SchemaExtension => extension }
    val hasDeclaredQuery     =
      document.schemaDefinition.flatMap(_.query).nonEmpty || schemaExtensions.exists(_.query.nonEmpty)
    val hasConventionalQuery =
      document.schemaDefinition.isEmpty && (
        document.objectTypeDefinitions.exists(_.name == "Query") ||
          document.typeExtensions.exists {
            case extension: ObjectTypeExtension => extension.name == "Query"
            case _                              => false
          }
      )
    hasDeclaredQuery || hasConventionalQuery
  }

  private def addFederationQueryRoot(document: Document): Document = {
    val names    = document.typeDefinitions.iterator.map(_.name).toSet ++ document.typeExtensions.collect {
      case extension: ObjectTypeExtension => extension.name
    }
    val rootName = Iterator
      .from(1)
      .map(index => if (index == 1) "CalibanGatewayFederationQuery" else s"CalibanGatewayFederationQuery$index")
      .find(name => !names.contains(name))
      .get
    val root     = ObjectTypeDefinition(
      None,
      rootName,
      Nil,
      Nil,
      List(FieldDefinition(None, ServiceField, Nil, NamedType(ServiceType, nonNull = true), Nil))
    )
    val service  =
      if (names.contains(ServiceType)) Nil
      else
        List(
          ObjectTypeDefinition(
            None,
            ServiceType,
            Nil,
            Nil,
            List(FieldDefinition(None, "sdl", Nil, NamedType("String", nonNull = true), Nil))
          )
        )
    val schema   = document.schemaDefinition match {
      case Some(_) =>
        document.definitions.map {
          case definition: SchemaDefinition if definition.query.isEmpty => definition.copy(query = Some(rootName))
          case definition                                               => definition
        }
      case None    =>
        SchemaDefinition(
          Nil,
          Some(rootName),
          if (names.contains("Mutation")) Some("Mutation") else None,
          if (names.contains("Subscription")) Some("Subscription") else None,
          None
        ) :: document.definitions
    }

    Document(schema ::: root :: service, document.sourceMapper)
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

  private def overrideCondition(progressive: ProgressiveOverride, active: Boolean): OverrideCondition =
    OverrideCondition(progressive.label, progressive.percentage, active)

  private def fieldOverride(directives: Option[List[Directive]], names: Set[String]): Option[FieldOverride] =
    directives.iterator.flatten.find(directive => names.contains(directive.name)).flatMap { directive =>
      stringArgument(directive.arguments, "from").map { from =>
        val progressive =
          stringArgument(directive.arguments, "label").flatMap(parseProgressiveOverrideLabel(_).toOption)
        FieldOverride(from, progressive)
      }
    }

  private def unsupportedDirectiveDiagnostics(
    names: FederationDirectiveNames,
    federation: Boolean,
    application: TypeSystemDirectiveApplication,
    directive: Directive
  ): List[String] = {
    val coordinate = application.coordinate.display
    val security   = names.security.get(directive.name).collect {
      case name if application.securityCoordinate.isEmpty => s"Federation $name is not supported at '$coordinate'."
    }
    List(
      security,
      names.unavailableSecurity
        .get(directive.name)
        .map(name => s"Federation $name is not available in the linked feature version at '$coordinate'."),
      Some(directive.name)
        .filter(name => federation && names.unimportedSecurity(name))
        .map(name =>
          s"Federation @$name at '$coordinate' is not imported through a supported @link, so it would not be enforced."
        ),
      names.unavailableCost
        .get(directive.name)
        .map(name => s"Federation $name requires Federation v2.9 or cost spec v0.1 at '$coordinate'."),
      ContextCompilation.diagnostic(names, application, directive.name),
      overrideDiagnostic(names, application, directive)
    ).flatten
  }

  private def overrideDiagnostic(
    names: FederationDirectiveNames,
    application: TypeSystemDirectiveApplication,
    directive: Directive
  ): Option[String] =
    if (!names.overrideDirective.contains(directive.name)) None
    else {
      val coordinate  = application.coordinate.display
      val unsupported =
        if (application.supportsOverride) None else Some(s"Federation @override is not supported at '$coordinate'.")
      directive.arguments.get("label") match {
        case Some(label) if label != NullValue && !names.supportsProgressiveOverride =>
          Some(s"Federation @override(label:) is not available in the linked feature version at '$coordinate'.")
        case Some(StringValue(label))                                                =>
          parseProgressiveOverrideLabel(label).fold(error => Some(s"$error At '$coordinate'."), _ => unsupported)
        case Some(NullValue) | None                                                  => unsupported
        case Some(_)                                                                 =>
          Some(s"Invalid Federation @override label at '$coordinate'.")
      }
    }

  private def validateFieldSet[A](
    subgraph: PreparedSubgraph,
    coordinate: String,
    directive: Directive,
    parent: Option[__Type]
  )(shape: List[Selection] => Either[String, A]): Either[String, A] = {
    val result = for {
      value      <- stringArgument(directive.arguments, "fields").toRight("the 'fields' argument must be a string.")
      selections <- parseFieldSet(value).toRight("the selection could not be parsed.")
      shaped     <- shape(selections)
      parentType <- parent.toRight("the selected parent type does not exist.")
      _          <- validateFieldSetSelections(subgraph, parentType, selections)
    } yield shaped
    result.left.map(error => s"[${subgraph.name}] Invalid @${directive.name} field set on '$coordinate': $error")
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

  private def compileKeys(
    subgraph: PreparedSubgraph,
    names: FederationDirectiveNames,
    schema: SchemaInspection
  ): Map[String, List[Either[String, FederationKey]]] =
    if (!subgraph.federation) Map.empty
    else
      schema.objectLikeTypes.map { definition =>
        definition.name -> definition.directives.filter(directive => names.key.contains(directive.name)).map {
          directive =>
            validateFieldSet(subgraph, definition.name, directive, subgraph.rootType.types.get(definition.name))(
              plainFieldSet(_).toRight("only fields without aliases, arguments, or directives can be selected.")
            ).map(FederationKey(_, !directive.arguments.get("resolvable").contains(BooleanValue(false))))
        }
      }.toMap

  private def federationKeyFields(subgraph: PreparedSubgraph, keys: Map[String, List[FederationKey]]): Set[TypeField] =
    keys.iterator.flatMap { case (typeName, values) =>
      values.flatMap(key => collectKeyFields(subgraph.rootType, typeName, key.fields))
    }.toSet

  private def federation1ExtensionKeyFields(
    subgraph: PreparedSubgraph,
    names: FederationDirectiveNames,
    schema: SchemaInspection,
    keys: Map[String, List[FederationKey]]
  ): Set[TypeField] =
    if (names.federation2) Set.empty
    else
      schema.objectLikeTypes.iterator
        .filter(definition =>
          subgraph.federation1ExtensionTypes(definition.name) ||
            hasDirective(definition.directives, names.extendsDirective)
        )
        .flatMap { definition =>
          keys.getOrElse(definition.name, Nil).flatMap(_.fields.map(field => TypeField(definition.name, field.name)))
        }
        .toSet

  private def collectKeyFields(rootType: RootType, typeName: String, fields: List[KeyField]): List[TypeField] =
    fields.flatMap { field =>
      val children =
        if (field.children.isEmpty) Nil
        else
          rootType.types
            .get(typeName)
            .flatMap(fieldDefinition(_, field.name))
            .flatMap(_._type.innerType.name)
            .toList
            .flatMap(collectKeyFields(rootType, _, field.children))
      TypeField(typeName, field.name) :: children
    }

  private def hasEntityLookup(subgraph: PreparedSubgraph, entityType: String): Boolean =
    declaresEntityLookup(subgraph, entityType) ||
      subgraph.federation && fieldDefinition(subgraph.rootType.queryType, EntitiesField).isEmpty

  private def declaresEntityLookup(subgraph: PreparedSubgraph, entityType: String): Boolean =
    fieldDefinition(subgraph.rootType.queryType, EntitiesField).exists { field =>
      isEntityLookup(field) && field._type.innerType.possibleTypes.exists(_.exists(_.name.contains(entityType)))
    }

  private def isEntityLookup(field: __Field): Boolean =
    field.name == EntitiesField && field._type.isList && field._type.innerType.name.contains("_Entity") &&
      field.allArgs.exists(argument =>
        argument.name == RepresentationsArgument && argument._type.isList && argument._type.innerType.name
          .contains(AnyType)
      )

  private def isTransportField(name: String): Boolean =
    name == EntitiesField || name == ServiceField

  private def hasFederationDirective(
    subgraph: PreparedSubgraph,
    directives: Option[List[Directive]],
    names: Set[String]
  ): Boolean =
    subgraph.federation && hasDirective(directives, names)

  private final case class FieldFlags(
    shareable: Boolean,
    external: Boolean,
    inaccessible: Boolean,
    overrideDirective: Option[FieldOverride],
    inaccessibleArguments: Set[String],
    contextualArguments: Set[String]
  )

  private final case class SubgraphMetadata(
    subgraph: PreparedSubgraph,
    keys: Map[String, List[Either[String, FederationKey]]],
    keyFields: Set[TypeField],
    federation1ExtensionKeyFields: Set[TypeField],
    hiddenDirectives: Set[String],
    schema: SchemaInspection,
    directiveApplications: List[TypeSystemDirectiveApplication]
  ) {
    def directiveNames: FederationDirectiveNames = subgraph.directiveNames
  }

  private final case class FederationKey(fields: List[KeyField], resolvable: Boolean)

  private final case class FederationFieldSets(
    requiredFieldSets: List[(SourceField, List[Selection])],
    providedFieldSets: List[(SourceField, List[Selection])]
  )

}

/**
 * Composes loaded subgraphs; all intermediate state is scoped to one composition.
 */
private[gateway] final class SchemaComposer private (subgraphs: List[SchemaComposer.PreparedSubgraph]) {
  import SchemaComposer._
  import DirectiveComposition._
  import TypeComposition._

  def compose: Either[List[String], ComposedGraph] = {
    val diagnostics =
      (lookupDiagnostics :::
        subgraphMetadata.flatMap(federationKeyDiagnostics) :::
        composedDirectives.diagnostics :::
        compiledFieldSets.flatMap(_.left.getOrElse(Nil)) :::
        compiledContexts.flatMap(_.left.getOrElse(Nil)) :::
        compiledCosts.flatMap(_.left.getOrElse(Nil)) :::
        compiledSecurity.flatMap(_.left.getOrElse(Nil)) :::
        subgraphMetadata.flatMap(unsupportedFederationDiagnostics) :::
        typeComposition.diagnostics :::
        progressiveOverrideSourceDiagnostics :::
        interfaceOverrideDiagnostics :::
        visibilityDiagnostics).distinct.sorted

    if (diagnostics.nonEmpty) Left(diagnostics)
    else buildGraph
  }

  private val sortedSubgraphs    = subgraphs.sortBy(_.name)
  private val lookupResults      = sortedSubgraphs.map { subgraph =>
    subgraph -> subgraph.lookups.map(lookup => lookup -> LookupCompilation.compile(subgraph, lookup))
  }
  private val compiledLookups    = lookupResults.flatMap { case (subgraph, results) =>
    results.collect { case (lookup, Right(operation)) => SourceType(subgraph.name, lookup.typeName) -> operation }
  }.toMap
  private val composedDirectives = DirectiveComposition.compile(sortedSubgraphs)
  private val subgraphMetadata   = sortedSubgraphs.map { subgraph =>
    val names  = subgraph.directiveNames
    val schema = new SchemaInspection(subgraph.document)
    val keys   = compileKeys(subgraph, names, schema)
    val valid  = keys.map { case (typeName, values) => typeName -> values.collect { case Right(key) => key } }
    SubgraphMetadata(
      subgraph,
      keys,
      federationKeyFields(subgraph, valid),
      federation1ExtensionKeyFields(subgraph, names, schema, valid),
      composedDirectives.hiddenNames(subgraph.name),
      schema,
      schema.directiveApplications(subgraph.rootNames.composed)
    )
  }
  private val types              = rootTypes ::: nonRootTypes
  private val typesBySource      = types.map(entry => SourceType(entry.source, entry.name) -> entry).toMap
  private val typeComposition    = new TypeComposition(types, composedDirectives)
  private val compiledFieldSets  = subgraphMetadata.map(compileFieldSets)
  private val compiledContexts   = {
    val contexts = new ContextCompilation(typesBySource)
    subgraphMetadata.map(metadata => contexts.compile(metadata.subgraph, metadata.directiveNames, metadata.schema))
  }
  private val compiledCosts      =
    subgraphMetadata.map(metadata =>
      CostCompilation.compile(metadata.subgraph, metadata.directiveNames, metadata.directiveApplications)
    )
  private val compiledSecurity   = subgraphMetadata.map(metadata =>
    SecurityCompilation.compile(metadata.subgraph.name, metadata.directiveNames, metadata.directiveApplications)
  )

  private def rootTypes: List[SubgraphType] =
    List(OperationType.Query, OperationType.Mutation, OperationType.Subscription).flatMap { operation =>
      subgraphMetadata.flatMap { metadata =>
        val subgraph = metadata.subgraph
        val root     = operation match {
          case OperationType.Query        => Some(subgraph.rootType.queryType)
          case OperationType.Mutation     => subgraph.rootType.mutationType
          case OperationType.Subscription => subgraph.rootType.subscriptionType
        }
        root.map { rootType =>
          val fields = rootType.allFields.filterNot(field => subgraph.federation && isTransportField(field.name))
          val tpe    = rootType.copy(
            name = Some(operation.toString),
            fields = args => Some(includeDeprecated(fields, args.includeDeprecated)(_.isDeprecated))
          )
          subgraphType(metadata, rootType.name.getOrElse(operation.toString), tpe, Some(operation))
        }
      }
    }

  private def nonRootTypes: List[SubgraphType] = {
    val reachable = reachableTypeNames
    subgraphMetadata.flatMap { metadata =>
      val subgraph  = metadata.subgraph
      val rootNames = subgraph.rootNames.sourceNames

      def isComposed(name: String): Boolean =
        reachable.contains(name) && !rootNames.contains(name) &&
          !(subgraph.federation && metadata.directiveNames.hiddenTypes.contains(name))

      subgraph.rootType.types.valuesIterator
        .flatMap(tpe => tpe.name.filter(isComposed).map(subgraphType(metadata, _, tpe, None)))
        .toList
    }
  }

  private def reachableTypeNames: Set[String] = {
    val byName    = subgraphMetadata
      .flatMap(metadata => metadata.subgraph.rootType.types.values.toList)
      .flatMap(tpe => tpe.name.map(_ -> tpe))
      .groupMap(_._1)(_._2)
    val reachable = mutable.Set.empty[String]
    val pending   = mutable.Queue.empty[String]

    def enqueue(name: String): Unit =
      if (reachable.add(name)) pending.enqueue(name)

    def enqueueReferences(tpe: __Type): Unit = {
      tpe.allFields.foreach { field =>
        field._type.innerType.name.foreach(enqueue)
        field.allArgs.foreach(_._type.innerType.name.foreach(enqueue))
      }
      tpe.allInputFields.foreach(_._type.innerType.name.foreach(enqueue))
      tpe.interfaces().getOrElse(Nil).foreach(_.name.foreach(enqueue))
      tpe.possibleTypes.getOrElse(Nil).foreach(_.name.foreach(enqueue))
    }

    composedDirectives.referencedInputTypes.foreach(enqueue)

    subgraphMetadata.foreach { metadata =>
      val rootType = metadata.subgraph.rootType
      rootType.queryType.name.foreach(enqueue)
      rootType.mutationType.flatMap(_.name).foreach(enqueue)
      rootType.subscriptionType.flatMap(_.name).foreach(enqueue)
    }

    while (pending.nonEmpty) {
      val name = pending.dequeue()
      byName.getOrElse(name, Nil).foreach(enqueueReferences)
    }
    reachable.toSet
  }

  private def subgraphType(
    metadata: SubgraphMetadata,
    sourceName: String,
    tpe: __Type,
    operation: Option[OperationType]
  ): SubgraphType = {
    val subgraph = metadata.subgraph
    val names    = metadata.directiveNames
    val isRoot   = operation.nonEmpty
    val name     = operation.fold(sourceName)(_.toString)

    def has(federationDirective: Set[String]): Boolean =
      hasFederationDirective(subgraph, tpe.directives, federationDirective)

    val interfaceObject  = !isRoot && has(names.interfaceObject)
    val typeExternal     = !isRoot && has(names.external)
    val typeShareable    = has(names.shareable)
    val composedType     =
      if (isRoot) tpe.copy(description = None, directives = None, interfaces = () => None, possibleTypes = None)
      else if (interfaceObject && tpe.kind == __TypeKind.OBJECT) tpe.copy(kind = __TypeKind.INTERFACE)
      else tpe
    val entity           = if (isRoot) None else entityDefinition(metadata, name, interfaceObject)
    val flags            = tpe.allFields.map(field => field.name -> fieldFlags(metadata, sourceName, field)).toMap
    val federation1Owned =
      if (isRoot) Set.empty[String]
      else metadata.federation1ExtensionKeyFields.collect { case TypeField(`name`, field) => field }
    val keyFields        = entity.fold(Set.empty[String])(_.keyFields) ++
      (if (isRoot) Set.empty else metadata.keyFields.collect { case TypeField(`name`, field) => field })
    val external         = flags.collect { case (field, value) if typeExternal || value.external => field }.toSet --
      federation1Owned

    SubgraphType(
      source = subgraph.name,
      name = name,
      tpe = composedType,
      operation = operation,
      interfaceObject = interfaceObject,
      entity = entity,
      ownedFields = tpe.allFields.map(_.name).toSet -- external,
      shareableFields = flags.collect {
        case (field, value) if typeShareable || value.shareable => field
      }.toSet ++ keyFields,
      inaccessible = !isRoot && (subgraph.mapping.hiddenTypes.contains(name) || has(names.inaccessible)),
      inaccessibleFields = flags.collect { case (field, value) if value.inaccessible => field }.toSet,
      inaccessibleArguments = flags.iterator.flatMap { case (field, value) =>
        value.inaccessibleArguments.iterator.map(field -> _)
      }.toSet,
      contextualArguments = flags.iterator.flatMap { case (field, value) =>
        value.contextualArguments.iterator.map(field -> _)
      }.toSet,
      inaccessibleInputFields = subgraph.mapping.hiddenInputFields.collect { case TypeField(`name`, field) =>
        field
      } ++
        tpe.allInputFields.iterator.collect {
          case field if hasDirective(field.directives, names.inaccessible) => field.name
        },
      inaccessibleEnumValues = tpe.allEnumValues.iterator.collect {
        case value if hasDirective(value.directives, names.inaccessible) => value.name
      }.toSet,
      overrideFields = flags.flatMap { case (field, value) => value.overrideDirective.map(field -> _) },
      subgraphMode =
        if (!subgraph.federation) SubgraphMode.Ordinary
        else if (names.federation2) SubgraphMode.Federation2
        else SubgraphMode.Federation1,
      hiddenDirectives = metadata.hiddenDirectives
    )
  }

  private def entityDefinition(
    metadata: SubgraphMetadata,
    name: String,
    interfaceObject: Boolean
  ): Option[EntityDefinition] = {
    val subgraph = metadata.subgraph
    if (!subgraph.federation)
      subgraph.lookups.find(_.typeName == name).map { lookup =>
        val key = lookup.keyFields.map(KeyField(_, Nil))
        EntityDefinition(
          lookup.keyFields.toSet,
          compiledLookups.get(SourceType(subgraph.name, name)).toList.map(EntityLookup(key, _))
        )
      }
    else {
      val keys = metadata.keys.getOrElse(name, Nil).collect { case Right(key) => key }
      if (keys.isEmpty) None
      else {
        val lookups =
          if (!hasEntityLookup(subgraph, name)) Nil
          else
            keys.collect {
              case key if key.resolvable =>
                EntityLookup(
                  key.fields,
                  LookupOperation.FederationEntities,
                  if (interfaceObject) Some(name) else None
                )
            }
        Some(EntityDefinition(keys.flatMap(_.fields.map(_.name)).toSet, lookups))
      }
    }
  }

  private def fieldFlags(metadata: SubgraphMetadata, owner: String, field: __Field): FieldFlags = {
    val subgraph = metadata.subgraph
    val names    = metadata.directiveNames

    def has(directives: Option[List[Directive]], federationDirective: Set[String]): Boolean =
      hasFederationDirective(subgraph, directives, federationDirective)

    def argumentsWith(federationDirective: Set[String]): Set[String] =
      field.allArgs.iterator.collect {
        case argument if has(argument.directives, federationDirective) => argument.name
      }.toSet

    val contextual = argumentsWith(names.fromContext)
    val mapped     = subgraph.mapping.hiddenArguments.collect {
      case FieldArgument(`owner`, fieldName, argument) if fieldName == field.name => argument
    }

    FieldFlags(
      shareable = has(field.directives, names.shareable),
      external = has(field.directives, names.external),
      inaccessible = subgraph.mapping.hiddenFields
        .contains(TypeField(owner, field.name)) || has(field.directives, names.inaccessible),
      overrideDirective = if (subgraph.federation) fieldOverride(field.directives, names.overrideDirective) else None,
      inaccessibleArguments = argumentsWith(names.inaccessible) ++ mapped ++ contextual,
      contextualArguments = contextual
    )
  }

  private def owns(source: String, typeName: String, field: String): Boolean =
    typesBySource.get(SourceType(source, typeName)).exists(_.ownedFields.contains(field))

  private def lookupDiagnostics: List[String] =
    lookupResults.flatMap { case (subgraph, results) =>
      LookupCompilation.declarationDiagnostics(subgraph) ::: results.flatMap(_._2.left.getOrElse(Nil))
    }

  private def federationKeyDiagnostics(metadata: SubgraphMetadata): List[String] =
    metadata.keys.valuesIterator.flatten.collect { case Left(error) => error }.toList

  private def compileFieldSets(metadata: SubgraphMetadata): Either[List[String], FederationFieldSets] = {
    val subgraph = metadata.subgraph
    if (!subgraph.federation) Right(FederationFieldSets(Nil, Nil))
    else {
      val names  = metadata.directiveNames
      val fields = metadata.schema.fields

      def compileFieldSet(
        typeName: String,
        field: FieldDefinition,
        directiveNames: Set[String],
        fieldSetType: Option[__Type]
      ): Option[Either[String, (SourceField, List[Selection])]] =
        field.directives.find(directive => directiveNames.contains(directive.name)).map { directive =>
          validateFieldSet(subgraph, s"$typeName.${field.name}", directive, fieldSetType)(Right(_))
            .map(SourceField(subgraph.name, subgraph.rootNames.composed(typeName), field.name) -> _)
        }

      val required = fields.flatMap { case (typeName, field) =>
        compileFieldSet(typeName, field, names.requires, subgraph.rootType.types.get(typeName))
      }
      val provided = fields.flatMap { case (typeName, field) =>
        val returnType = subgraph.rootType.types
          .get(typeName)
          .flatMap(fieldDefinition(_, field.name))
          .map(_._type.innerType)
        compileFieldSet(typeName, field, names.provides, returnType)
      }

      (validateAll(required), validateAll(provided)) match {
        case (Right(requiredSets), Right(providedSets)) => Right(FederationFieldSets(requiredSets, providedSets))
        case (requiredSets, providedSets)               =>
          Left(requiredSets.left.getOrElse(Nil) ::: providedSets.left.getOrElse(Nil))
      }
    }
  }

  private def unsupportedFederationDiagnostics(metadata: SubgraphMetadata): List[String] =
    for {
      application <- metadata.directiveApplications
      directive   <- application.directives
      diagnostic  <- unsupportedDirectiveDiagnostics(
                       metadata.directiveNames,
                       metadata.subgraph.federation,
                       application,
                       directive
                     )
    } yield s"[${metadata.subgraph.name}] $diagnostic"

  private def progressiveOverrideSourceDiagnostics: List[String] =
    types.flatMap { entry =>
      entry.overrideFields.toList.collect {
        case (field, directive) if directive.progressive.nonEmpty && !owns(directive.from, entry.name, field) =>
          s"${fieldDiagnosticPrefix(entry.operation, entry.name, field)} Progressive @override in subgraph '${entry.source}' requires its 'from' subgraph '${directive.from}' to own the field."
      }
    }

  private def interfaceOverrideDiagnostics: List[String] =
    types.filter(entry => entry.tpe.kind == __TypeKind.INTERFACE && !entry.interfaceObject).flatMap { entry =>
      entry.overrideFields.keys.map(field =>
        s"[${entry.source}] Federation @override is not supported at '${entry.name}.$field'."
      )
    }

  private def visibilityDiagnostics: List[String] = {
    val inaccessibleTypes     = types.filter(_.inaccessible).map(_.name).toSet
    val inaccessibleFields    =
      types.iterator.flatMap(entry => entry.inaccessibleFields.map(TypeField(entry.name, _))).toSet
    val inaccessibleInputs    =
      types.iterator.flatMap(entry => entry.inaccessibleInputFields.map(TypeField(entry.name, _))).toSet
    val inaccessibleArguments = types
      .flatMap(entry =>
        entry.inaccessibleArguments.map { case (field, argument) => TypeField(entry.name, field) -> argument }
      )
      .groupMap(_._1)(_._2)
      .map { case (field, arguments) => field -> arguments.toSet }

    def hasInaccessibleType(tpe: __Type): Boolean = tpe.innerType.name.exists(inaccessibleTypes.contains)

    val accessibleTypes  = types.filterNot(entry => inaccessibleTypes.contains(entry.name))
    val fieldErrors      = accessibleTypes.flatMap { entry =>
      entry.tpe.allFields.filterNot(field => inaccessibleFields.contains(TypeField(entry.name, field.name))).flatMap {
        field =>
          val coordinate = fieldCoordinate(entry.operation, entry.name, field.name)
          val hidden     = inaccessibleArguments.getOrElse(TypeField(entry.name, field.name), Set.empty[String])
          val output     = check(
            !hasInaccessibleType(field._type),
            s"[${entry.source}] Field '$coordinate' must be @inaccessible because its return type is inaccessible."
          )
          output ::: field.allArgs.collect {
            case argument if !hidden(argument.name) && hasInaccessibleType(argument._type) =>
              s"[${entry.source}] Argument '$coordinate.${argument.name}' must be @inaccessible because its input type is inaccessible."
            case argument if hidden(argument.name) && isRequiredInput(argument)            =>
              s"[${entry.source}] Required @inaccessible argument '$coordinate.${argument.name}' must define a default value."
          }
      }
    }
    val inputFieldErrors = accessibleTypes.flatMap { entry =>
      entry.tpe.allInputFields.flatMap { field =>
        val inaccessible = inaccessibleInputs.contains(TypeField(entry.name, field.name))
        if (!inaccessible && hasInaccessibleType(field._type))
          Some(
            s"[${entry.source}] Input field '${entry.name}.${field.name}' must be @inaccessible because its input type is inaccessible."
          )
        else if (inaccessible && isRequiredInput(field))
          Some(
            s"[${entry.source}] Required @inaccessible input field '${entry.name}.${field.name}' must define a default value."
          )
        else None
      }
    }
    fieldErrors ::: inputFieldErrors
  }

  private def buildGraph: Either[List[String], ComposedGraph] = {
    val composedTypes       = typeComposition.composed
    val rootType            = composedRootType(composedTypes)
    val possibleTypesByName = rootType.types.map { case (name, tpe) => name -> tpe.possibleTypeNames }
    val security            = compiledSecurity.flatMap(_.toOption).flatten
    val fieldSets           = compiledFieldSets.flatMap(_.toOption)
    val requiredFieldSets   = fieldSets.flatMap(_.requiredFieldSets).toMap
    val contexts            = compiledContexts.flatMap(_.toOption)
    val declaredContexts    = contexts.flatMap(_.declaredContexts).toMap
    val contextBindings     = contexts.flatMap(_.contextBindings).toMap
    val sourceFields        = types.flatMap { entry =>
      entry.tpe.allFields.map(field => SourceField(entry.source, entry.name, field.name) -> field)
    }.toMap
    val diagnostics         =
      invalidTransformationDiagnostics(rootType) ::: composedDirectives.schemaDiagnostics(rootType) :::
        SecurityCompilation.diagnostics(
          security,
          sourceFields,
          requiredFieldSets,
          declaredContexts,
          contextBindings,
          possibleTypesByName,
          rootType
        )

    if (diagnostics.nonEmpty) Left(diagnostics.distinct.sorted)
    else
      SchemaValidator
        .validateRootType(rootType)
        .left
        .map(error => List(s"[composition] ${error.getMessage}"))
        .map { _ =>
          val (rootDefinitions, fieldDefinitions) = types
            .flatMap(entry => entry.tpe.allFields.map(field => TypeField(entry.name, field.name) -> entry))
            .groupMap(_._1)(_._2)
            .partition { case (_, entries) => entries.exists(_.operation.nonEmpty) }

          new ComposedGraph(
            rootType = rootType,
            possibleTypesByName = possibleTypesByName,
            rootRoutes = rootRoutes(composedTypes, rootDefinitions),
            fieldRoutes = fieldRoutes(fieldDefinitions),
            sourceFields = sourceFields,
            entityLookupsByType = types
              .flatMap(entry => entry.entity.toList.flatMap(_.lookups).map(SourceType(entry.source, entry.name) -> _))
              .groupMap(_._1)(_._2),
            requiredFieldSets = requiredFieldSets,
            providedFieldSets = fieldSets.flatMap(_.providedFieldSets).toMap,
            declaredContexts = declaredContexts,
            contextBindings = contextBindings,
            interfaceObjects = types.filter(_.interfaceObject).map(entry => SourceType(entry.source, entry.name)).toSet,
            sourcePossibleTypes = sortedSubgraphs.iterator.flatMap { subgraph =>
              subgraph.rootType.types.iterator.map { case (name, tpe) =>
                SourceType(subgraph.name, name) -> tpe.possibleTypeNames
              }
            }.toMap,
            schemaMappings = sortedSubgraphs.map(subgraph => subgraph.name -> subgraph.mapping).toMap,
            costMetadata = CostCompilation.merge(compiledCosts.flatMap(_.toOption)),
            securityApplications = security,
            schemaDirectives = composedDirectives.schemaDirectives
          )
        }
  }

  private def composedRootType(composedTypes: Map[String, __Type]): RootType = {
    val operationTypeNames = Set("Query", "Mutation", "Subscription")
    val additionalTypes    = composedTypes.toList.sortBy(_._1).collect {
      case (name, tpe) if !operationTypeNames.contains(name) => tpe
    } ::: composedDirectives.additionalTypes.filterNot(tpe => tpe.name.exists(composedTypes.contains))
    RootType(
      composedTypes("Query"),
      composedTypes.get("Mutation").filter(_.allFields.nonEmpty),
      composedTypes.get("Subscription").filter(_.allFields.nonEmpty),
      additionalTypes,
      composedDirectives.definitions(rewriteType(_, composedTypes))
    )
  }

  private def rootRoutes(
    composedTypes: Map[String, __Type],
    definitions: Map[TypeField, List[SubgraphType]]
  ): Map[RootField, RootRoute] =
    definitions.flatMap { case (TypeField(typeName, field), entries) =>
      val operation = entries.flatMap(_.operation).head
      composedTypes.get(typeName).flatMap(fieldDefinition(_, field)).map { composedField =>
        val selectFirst = !isCompositeType(composedField._type.innerType)
        RootField(operation, field) -> RootRoute(fieldRouteCandidates(typeName, field, entries), selectFirst)
      }
    }

  private def fieldRoutes(definitions: Map[TypeField, List[SubgraphType]]): Map[TypeField, List[FieldRoute]] =
    definitions.flatMap { case (field, entries) =>
      val routes = fieldRouteCandidates(field.typeName, field.fieldName, entries)
      if (routes.nonEmpty) Some(field -> routes) else None
    }

  private def fieldRouteCandidates(typeName: String, field: String, entries: List[SubgraphType]): List[FieldRoute] = {
    val sources           = effectiveFieldSources(field, entries).map(_.source)
    val overrideDirective =
      entries.collectFirst(Function.unlift(entry => entry.overrideFields.get(field).map(entry.source -> _)))
    val routes            = overrideDirective match {
      case Some((by, FieldOverride(from, Some(progressive)))) if owns(from, typeName, field) =>
        val overridingRoutes = sources.map { source =>
          FieldRoute(source, if (source == by) Some(overrideCondition(progressive, active = true)) else None)
        }
        overridingRoutes :+ FieldRoute(from, Some(overrideCondition(progressive, active = false)))
      case _                                                                                 => sources.map(FieldRoute(_))
    }
    routes.distinct.sortBy(_.source)
  }

  private def invalidTransformationDiagnostics(rootType: RootType): List[String] = {
    def hasNoVisibleFields(name: String, kind: __TypeKind): Boolean =
      rootType.types.get(name).exists { tpe =>
        tpe.kind == kind &&
        (if (kind == __TypeKind.INPUT_OBJECT) tpe.allInputFields.isEmpty else tpe.allFields.isEmpty)
      }

    sortedSubgraphs.flatMap { subgraph =>
      val mapping = subgraph.mapping

      def diagnostic(sourceType: String, kind: __TypeKind, description: String): Option[String] = {
        val name = mapping.composedType(sourceType)
        if (hasNoVisibleFields(name, kind))
          Some(s"[${subgraph.name}] Transformation leaves $description '$name' with no visible fields.")
        else None
      }

      mapping.hiddenFields.toList.flatMap { case TypeField(tpe, _) =>
        diagnostic(tpe, __TypeKind.OBJECT, "object").orElse(diagnostic(tpe, __TypeKind.INTERFACE, "interface"))
      } ::: mapping.hiddenInputFields.toList.flatMap { case TypeField(tpe, _) =>
        diagnostic(tpe, __TypeKind.INPUT_OBJECT, "input object")
      }
    }.distinct.sorted
  }

}
