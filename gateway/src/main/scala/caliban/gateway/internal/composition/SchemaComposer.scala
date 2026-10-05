package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.CompositionDiagnostic.{ error, Code, Severity }
import caliban.gateway.GatewayBuildError.SchemaCompositionFailed
import caliban.gateway.SubgraphBuildError.{ InvalidTransformations, SchemaValidationFailed }
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.gateway.internal.composition.FederationCompilation._
import caliban.gateway.internal.composition.FederationCompilation.FederationDirective._
import caliban.introspection.adt._
import caliban.parsing.SourceMapper
import caliban.parsing.adt.{ Document, OperationType, Selection }
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

  def compose(prepared: List[Source]): Either[SchemaCompositionFailed, (ComposedGraph, List[CompositionDiagnostic])] =
    new SchemaComposer(prepared).compose

  def prepare[R](subgraph: Subgraph[R], document: Document): Either[SubgraphBuildError, Source] = {
    val federation                    = subgraph.source.federation
    val rootDocument                  = if (federation) withFederationQueryRoot(document) else document
    def normalize(document: Document) =
      RemoteSchema.normalize(document, extensionsCanDefineTypes = federation).left.map(SchemaValidationFailed(_))
    for {
      normalized    <- normalize(rootDocument)
      linked         = linkedFeatures(normalized.document).exists(_.identity == FederationIdentity)
      mode          <-
        if (federation) Right(if (linked) SubgraphMode.Federation2 else SubgraphMode.Federation1)
        else {
          val federated =
            check(
              !normalized.rootType.queryType.allFields.exists(isEntityLookup),
              "The query root declares Federation entity transport."
            ) ::: check(!linked, "The schema links the Federation spec.")
          Either.cond(
            federated.isEmpty,
            SubgraphMode.Composite(!subgraph.introspected),
            SubgraphBuildError.InvalidConfiguration(
              federated.map(found => s"$found Use Subgraph.federation instead of Subgraph.graphql.")
            )
          )
        }
      names          = federationDirectiveNames(normalized.document, mode)
      mapping       <-
        SchemaMapping
          .compile(normalized.rootType, names, subgraph.transformations)
          .left
          .map(InvalidTransformations(_))
      extensionTypes = federation1ExtensionTypes(document).map(mapping.clientType)
      transformed   <-
        if (mapping.nonEmpty) normalize(mapping.transform(normalized.document, names)) else Right(normalized)
    } yield new Source(
      subgraph.name,
      transformed.rootType,
      DirectiveComposition.schemaDirectives(transformed.document),
      subgraph.lookups,
      mapping,
      extensionTypes,
      names
    )
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

    val missing = !extendedQuery && document.schemaDefinition.fold(!names("Query"))(_.query.isEmpty)
    if (!missing) document
    else {
      val name        = freeName(1)
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

  private def parseProgressiveOverrideLabel(label: String): Either[String, OverrideLabel] =
    label match {
      case ProgressiveOverridePattern(value) => Right(OverrideLabel.Percent(label, BigDecimal(value)))
      case CustomOverrideLabelPattern()      => Right(OverrideLabel.Custom(label))
      case _                                 => Left(s"Invalid Federation @override label '$label'.")
    }

  private[composition] def fieldOverride(subgraph: Source, application: FederationApplication) = {
    val arguments = application.directive.arguments
    val at        = application.coordinate.display
    val label     = stringArgument(arguments, "label").map(parseProgressiveOverrideLabel)
    val invalid   =
      if (label.nonEmpty && !subgraph.directiveNames.supportsProgressiveOverride)
        Some(s"Federation @override(label:) is not available in the linked feature version at '$at'.")
      else label.flatMap(_.left.toOption).map(message => s"$message At '$at'.")
    FieldOverride(stringArgument(arguments, "from").getOrElse(""), label.flatMap(_.toOption)) ->
      invalid.map(error(Code.OverrideLabelInvalid, List(subgraph.name), application.coordinate.schemaCoordinate)(_))
  }

  private[composition] def validateFieldSet[A](
    subgraph: Source,
    application: FederationApplication,
    parent: __Type,
    code: Code
  )(shape: List[Selection] => Either[String, A]): Either[CompositionDiagnostic, A] = {
    val directive = application.directive
    val result    = for {
      selections <- stringArgument(directive.arguments, "fields")
                      .flatMap(parseFieldSet)
                      .toRight("the selection could not be parsed.")
      shaped     <- shape(selections)
      _          <- validateFieldSetSelections(subgraph, parent, selections)
    } yield shaped
    result.left.map(message =>
      error(code, List(subgraph.name), application.coordinate.schemaCoordinate)(
        s"Invalid @${directive.name} field set on '${application.coordinate.display}': $message"
      )
    )
  }

  private def validateFieldSetSelections(
    subgraph: Source,
    parent: __Type,
    selections: List[Selection]
  ): Either[String, Unit] = {
    val document = Document(
      OperationDefinition(OperationType.Query, None, Nil, Nil, selections) :: Nil,
      SourceMapper.empty
    )
    Validator.validateAll(document, subgraph.rootType.copy(queryType = parent)).left.map(_.msg)
  }

  private[composition] def subgraphKeys(subgraph: Source): SubgraphKeys = {
    val (errors, keys) = subgraph
      .applications(Key)
      .collect { case application @ FederationApplication(TypeCoordinate(name, _), _, directive) =>
        subgraph.rootType.types.get(name).map { parent =>
          validateFieldSet(subgraph, application, parent, Code.KeyInvalidFields)(
            plainFieldSet(_).toRight("only fields without aliases, arguments, or directives can be selected.")
          ).map { fields =>
            val resolvable = !directive.arguments.get("resolvable").contains(BooleanValue(false))
            FederationKey(name, fields, resolvable && hasEntityLookup(subgraph, parent))
          }
        }
      }
      .flatten
      .partitionMap(identity)
    val keyFields      = keys.flatMap(key => collectKeyFields(subgraph.rootType, key.typeName, key.fields))
    val lookupKeys     = subgraph.lookups.flatMap { lookup =>
      val typeName = subgraph.mapping.clientType(lookup.typeName)
      lookup.keyFields.map(field => FieldCoordinate(typeName, subgraph.mapping.clientField(lookup.typeName, field)))
    } ::: subgraph.lookupFields.lookups.flatMap { case (typeName, lookup) =>
      collectKeyFields(subgraph.rootType, typeName, lookup.key)
    }
    SubgraphKeys(keys, errors, keyFields.toSet ++ lookupKeys, federation1ExtensionKeyFields(subgraph, keys))
  }

  private def federation1ExtensionKeyFields(
    subgraph: Source,
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
      val owner     = field.condition.getOrElse(typeName)
      val childType =
        rootType.types.get(owner).flatMap(fieldDefinition(_, field.name)).flatMap(_._type.innerType.name)
      FieldCoordinate(owner, field.name) :: childType.toList.flatMap(collectKeyFields(rootType, _, field.children))
    }

  // `_Entity` is a union, so it lists an entity interface's implementations rather than the interface.
  private def hasEntityLookup(subgraph: Source, entity: __Type): Boolean =
    fieldDefinition(subgraph.rootType.queryType, EntitiesField).forall { field =>
      isEntityLookup(field) && entity.possibleTypeNames.subsetOf(field._type.innerType.possibleTypeNames)
    }

  private def isEntityLookup(field: __Field): Boolean =
    field.name == EntitiesField && field._type.isList && field._type.innerType.name.contains("_Entity") &&
      field.allArgs.exists(argument =>
        argument.name == RepresentationsArgument && argument._type.isList && argument._type.innerType.name
          .contains(AnyType)
      )

  private[composition] final case class SubgraphKeys(
    all: List[FederationKey],
    diagnostics: List[CompositionDiagnostic],
    shared: Set[FieldCoordinate],
    federation1Owned: Set[FieldCoordinate]
  )

  private[composition] final case class FederationKey(typeName: String, fields: List[KeyField], resolvable: Boolean)

}

/**
 * Composes loaded subgraphs; all intermediate state is scoped to one composition.
 */
private[gateway] final class SchemaComposer private (subgraphs: List[ComposedGraph.Source]) {
  import SchemaComposer._
  import DirectiveComposition._
  import TypeComposition._

  def compose: Either[SchemaCompositionFailed, (ComposedGraph, List[CompositionDiagnostic])] =
    for {
      warnings <-
        checked(
          sortedSubgraphs.flatMap(_.diagnostics) ::: composedDirectives.diagnostics ::: typeComposition.diagnostics :::
            selectionMapDiagnostics
        )
      rootType <- composedRootType(typeComposition.composed).left.map(failed(_, warnings))
      graph     = composedGraph(rootType)
      warnings <- checked(
                    warnings ::: invalidTransformationDiagnostics(rootType) :::
                      composedDirectives.schemaDiagnostics(rootType) ::: SecurityCompilation.diagnostics(graph)
                  )
      _        <- SchemaValidator
                    .validateRootType(rootType)
                    .left
                    .map(invalid => failed(error(Code.InvalidGraphQL, Nil, None)(invalid.getMessage), warnings))
    } yield graph -> warnings

  // Composition stops at the first phase that fails; warnings found so far are reported with the failure.
  private def checked(
    diagnostics: List[CompositionDiagnostic]
  ): Either[SchemaCompositionFailed, List[CompositionDiagnostic]] =
    diagnostics.distinct.sortBy(_.render) match {
      case all @ first :: rest if all.exists(_.severity == Severity.Error) =>
        Left(SchemaCompositionFailed(::(first, rest)))
      case warnings                                                        => Right(warnings)
    }

  private def failed(error: CompositionDiagnostic, warnings: List[CompositionDiagnostic]): SchemaCompositionFailed =
    SchemaCompositionFailed(::(error, warnings))

  private val sortedSubgraphs    = subgraphs.sortBy(_.name)
  private val composedDirectives = DirectiveComposition.compile(sortedSubgraphs)
  private val subgraphTypes      = sortedSubgraphs.flatMap { subgraph =>
    subgraph.rootType.types.collect {
      case (name, tpe) if !subgraph.unmergedTypes(name) => subgraphType(subgraph, name, tpe)
    }
  }
  private val typeComposition    = new TypeComposition(subgraphTypes, composedDirectives)

  /**
   * Every subgraph's types merged by name, inaccessible members included. Selection maps are validated against them,
   * since their fields may come from any subgraph.
   */
  private lazy val selectionMapTypes: Map[String, __Type] =
    groupNonEmpty(subgraphTypes)(_.name).map { case (name, entries) =>
      name -> entries.head.tpe.copy(
        fields = args => Some(entries.flatMap(_.tpe.fields(args).getOrElse(Nil)).distinctBy(_.name)),
        possibleTypes = Some(entries.flatMap(_.tpe.possibleTypes.getOrElse(Nil)).distinctBy(_.name))
      )
    }

  private def selectionMapDiagnostics: List[CompositionDiagnostic] =
    sortedSubgraphs.flatMap { subgraph =>
      (subgraph.lookupFields.maps ::: subgraph.requiredArguments.maps).flatMap { use =>
        val required                                = use.directive == Require
        // As Fusion does, a @require selection map may not read a field that its own subgraph defines.
        val local: (String, String) => List[String] = (owner, field) =>
          check(
            !required || subgraph.sourceField(owner, field).isEmpty,
            s"The required field '$owner.$field' must not be defined by this subgraph."
          )
        selectionMapTypes.get(use.outputType).toList.flatMap { outputType =>
          val errors =
            FieldSelectionMap.validate(use.value, use.inputType, outputType, selectionMapTypes, local).distinct
          check(
            errors.isEmpty,
            error(if (required) Code.RequireInvalidFields else Code.IsInvalidFields, List(subgraph.name), Some(use.at))(
              s"Invalid @${use.directive.name} field selection map on '${use.at.render}': ${errors.mkString(" ")}"
            )
          )
        }
      }
    }

  private def subgraphType(subgraph: Source, name: String, tpe: __Type): SubgraphType = {
    val isRoot = RootOperations.contains(name)

    def has(member: FederationDirective): Boolean =
      subgraph.applied(member, TypeCoordinate(name, typeLocation(tpe.kind)))

    // Internal fields serve only as lookups and stay out of merging.
    def excluded(field: __Field): Boolean =
      subgraph.applied(Internal, FieldCoordinate(name, field.name)) ||
        isRoot && subgraph.federation && TransportFields(field.name)

    val interfaceObject = subgraph.isInterfaceObject(name)
    val typeExternal    = !isRoot && has(FederationDirective.External)
    val typeShareable   = has(FederationDirective.Shareable)
    val visible         = tpe.copy(fields = args => tpe.fields(args).map(_.filterNot(excluded)))
    val composedType    =
      if (isRoot) visible.copy(description = None, interfaces = () => None)
      else if (interfaceObject) visible.copy(kind = __TypeKind.INTERFACE)
      else visible

    def subgraphField(field: __Field): SubgraphField = {
      val at       = FieldCoordinate(name, field.name)
      val external = typeExternal || subgraph.applied(External, at)

      def appliedArguments(directive: FederationDirective): Set[String] =
        field.allArgs.iterator
          .map(_.name)
          .filter(argument => subgraph.applied(directive, ArgumentCoordinate(name, field.name, argument)))
          .toSet
      SubgraphField(
        definition = field,
        owned = !external || !isRoot && subgraph.keys.federation1Owned(at),
        shareable = typeShareable || subgraph.applied(Shareable, at) || !isRoot && subgraph.keys.shared(at),
        overrideDirective = subgraph.overrides.get(at).map(_._1),
        contextualArguments = appliedArguments(FromContext),
        requiredArguments = appliedArguments(Require)
      )
    }

    SubgraphType(
      subgraph = subgraph,
      name = name,
      tpe = composedType,
      fields = composedType.allFields.map(subgraphField)
    )
  }

  private def composedGraph(rootType: RootType): ComposedGraph =
    new ComposedGraph(
      rootType = rootType,
      fieldRoutes = typeComposition.inactiveRoutes,
      progressiveRoutes = typeComposition.progressiveRoutes,
      sources = sortedSubgraphs,
      possibleTypesByName = rootType.types.map { case (name, tpe) => name -> tpe.possibleTypeNames },
      costMetadata = CostCompilation.merge(sortedSubgraphs.flatMap(_.costs.toOption)),
      securityApplications = sortedSubgraphs.flatMap(SecurityCompilation.compile)
    )

  private def composedRootType(composedTypes: Map[String, __Type]): Either[CompositionDiagnostic, RootType] = {
    val additionalTypes = composedTypes.toList.sortBy(_._1).collect {
      case (name, tpe) if !RootOperations.contains(name) => tpe
    } ::: composedDirectives.additionalTypes.filterNot(tpe => tpe.name.exists(composedTypes.contains))
    composedTypes
      .get("Query")
      .toRight(error(Code.NoQueries, Nil, None)("No subgraph defines a query root type."))
      .map { query =>
        RootType(
          query,
          composedTypes.get("Mutation").filter(_.allFields.nonEmpty),
          composedTypes.get("Subscription").filter(_.allFields.nonEmpty),
          additionalTypes,
          composedDirectives.definitions(rewriteType(_, composedTypes))
        )
      }
  }

  private def invalidTransformationDiagnostics(rootType: RootType): List[CompositionDiagnostic] =
    sortedSubgraphs.flatMap { subgraph =>
      subgraph.mapping.hidden.toList.collect {
        case FieldCoordinate(name, _)      => name
        case InputFieldCoordinate(name, _) => name
      }.flatMap { name =>
        rootType.types
          .get(name)
          .filter(tpe => tpe.allFields.isEmpty && tpe.allInputFields.isEmpty)
          .flatMap(tpe => TransformedKinds.get(tpe.kind))
          .map(kind =>
            error(Code.OnlyInaccessibleChildren, List(subgraph.name), Some(SchemaCoordinate.Type(name)))(
              s"Transformation leaves $kind '$name' with no visible fields."
            )
          )
      }
    }

  private val TransformedKinds: Map[__TypeKind, String] =
    Map(__TypeKind.OBJECT -> "object", __TypeKind.INTERFACE -> "interface", __TypeKind.INPUT_OBJECT -> "input object")

}
