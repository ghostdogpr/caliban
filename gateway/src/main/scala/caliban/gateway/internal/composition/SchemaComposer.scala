package caliban.gateway.internal.composition

import caliban.gateway._
import caliban.gateway.PhaseHooks.SecurityDirective
import caliban.gateway.GatewayBuildError.{ SchemaCompositionFailed, SubgraphLoadingFailed }
import caliban.gateway.SubgraphBuildError.{ InvalidTransformations, SchemaValidationFailed }
import caliban.gateway.internal.composition.ComposedGraph._
import caliban.InputValue
import caliban.introspection.adt._
import caliban.parsing.SourceMapper
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection }
import caliban.parsing.adt.Definition.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.Definition.TypeSystemDefinition._
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.Type.NamedType
import caliban.rendering.DocumentRenderer
import caliban.schema.{ RootType, Types }
import caliban.tools.RemoteSchema
import caliban.validation.{ SchemaValidator, Validator }
import caliban.Value.{ BooleanValue, NullValue, StringValue }

import scala.annotation.tailrec
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
      new Composition(prepared.collect { case Right(subgraph) => subgraph }).compose.left
        .map(errors => SchemaCompositionFailed(errors.distinct.sorted))
  }

  private def prepare[R](subgraph: Subgraph[R], document: Document): Either[SubgraphBuildError, PreparedSubgraph] = {
    val federation   = subgraph.source.federation
    val rootDocument = if (federation && !hasQueryRoot(document)) addFederationQueryRoot(document) else document
    for {
      normalized    <-
        RemoteSchema.normalize(rootDocument, promoteOrphans = federation).left.map(SchemaValidationFailed(_))
      mapping       <-
        SchemaMapping
          .compile(subgraph.name, normalized.rootType, normalized.document, federation, subgraph.transformations)
          .left
          .map(InvalidTransformations(_))
      extensionTypes = federation1ExtensionTypes(document).map(mapping.clientType)
      transformed   <- if (mapping.nonEmpty)
                         RemoteSchema
                           .normalize(mapping.transform(normalized.document), promoteOrphans = federation)
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
      extensionTypes
    )
  }

  private[composition] final case class PreparedSubgraph(
    name: String,
    rootType: RootType,
    document: Document,
    federation: Boolean,
    lookups: List[Lookup],
    mapping: SchemaMapping,
    federation1ExtensionTypes: Set[String] = Set.empty
  ) {
    val rootNames: OperationRootNames = mapping.rootNames
  }

  /**
   * Composes loaded subgraphs; all intermediate state is scoped to one composition.
   */
  private final class Composition(subgraphs: List[PreparedSubgraph]) {
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
    private val namesBySubgraph    =
      sortedSubgraphs.map(subgraph => subgraph -> federationDirectiveNames(subgraph.document))
    private val composedDirectives = DirectiveComposition.compile(namesBySubgraph.map { case (subgraph, names) =>
      Source(subgraph, names.hidden, names.features, names.interfaceObject)
    })
    private val subgraphMetadata   = namesBySubgraph.map { case (subgraph, names) =>
      val schema = new SchemaInspection(subgraph.document)
      SubgraphMetadata(
        subgraph,
        names,
        federationKeyFields(subgraph, names, schema),
        federation1ExtensionKeyFields(subgraph, names, schema),
        composedDirectives.hiddenNames(subgraph.name),
        schema,
        schema.directiveApplications(subgraph.rootNames.composed)
      )
    }
    private val types              = rootTypes ::: nonRootTypes
    private val interfaceOverrides = interfaceOverrideTargets(types)
    private val typeComposition    = new TypeComposition(types, enumUsageByName, composedDirectives)
    private val compiledFieldSets  = subgraphMetadata.map(compileFieldSets)
    private val compiledContexts   = subgraphMetadata.map(compileContexts)
    private val compiledCosts      =
      subgraphMetadata.map(metadata => CostCompilation.compile(metadata.subgraph, metadata.directiveNames))
    private val compiledSecurity   = subgraphMetadata.map(compileSecurity)

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
              fields =
                args => Some(if (args.includeDeprecated.getOrElse(false)) fields else fields.filterNot(_.isDeprecated))
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
      val allTypes        = subgraphMetadata.flatMap(metadata => metadata.subgraph.rootType.types.values.toList)
      val byName          = allTypes.flatMap(tpe => tpe.name.map(_ -> tpe)).groupMap(_._1)(_._2)
      val implementations = allTypes
        .filter(_.kind == __TypeKind.OBJECT)
        .flatMap(tpe =>
          tpe.interfaces().getOrElse(Nil).flatMap(_.name).flatMap(interface => tpe.name.map(interface -> _))
        )
        .groupMap(_._1)(_._2)
        .map { case (interface, values) => interface -> values.distinct }
      val reachable       = mutable.Set.empty[String]
      val pending         = mutable.Queue.empty[String]

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
        implementations.getOrElse(name, Nil).foreach(enqueue)
      }
      reachable.toSet
    }

    private def subgraphType(
      metadata: SubgraphMetadata,
      sourceName: String,
      tpe: __Type,
      operation: Option[OperationType]
    ): SubgraphType = {
      val subgraph   = metadata.subgraph
      val names      = metadata.directiveNames
      val directives = tpe.directives.getOrElse(Nil)
      val isRoot     = operation.nonEmpty
      val name       = operation.fold(sourceName)(_.toString)

      def has(federationDirective: Set[String]): Boolean =
        subgraph.federation && hasDirective(directives, federationDirective)

      val interfaceObject  = !isRoot && has(names.interfaceObject)
      val typeExternal     = !isRoot && has(names.external)
      val typeShareable    = has(names.shareable)
      val composedType     =
        if (isRoot) tpe.copy(description = None, directives = None, interfaces = () => None, possibleTypes = None)
        else if (interfaceObject && tpe.kind == __TypeKind.OBJECT) tpe.copy(kind = __TypeKind.INTERFACE)
        else tpe
      val entity           = if (isRoot) None else entityDefinition(metadata, name, directives, interfaceObject)
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
      directives: List[Directive],
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
        val keys = directives.flatMap(keyDirective(_, metadata.directiveNames))
        if (keys.isEmpty) None
        else {
          val lookups =
            if (!hasEntityLookup(subgraph, name)) Nil
            else
              keys.collect {
                case key if key.resolvable =>
                  EntityLookup(
                    key.fields,
                    LookupOperation.FederationEntities(declaresEntityLookup(subgraph, name)),
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
        subgraph.federation && hasDirective(directives, federationDirective)

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

    private def enumUsageByName: Map[String, EnumUsage] = {
      val allTypes = types.map(_.tpe)
      val inputs   = allTypes.iterator.flatMap { tpe =>
        tpe.allInputFields.iterator.flatMap(_._type.innerType.name) ++
          tpe.allFields.iterator.flatMap(_.allArgs.iterator.flatMap(_._type.innerType.name))
      }.toSet
      val outputs  = allTypes.iterator.flatMap(_.allFields.iterator.flatMap(_._type.innerType.name)).toSet

      (inputs ++ outputs).iterator.map(name => name -> EnumUsage(inputs.contains(name), outputs.contains(name))).toMap
    }

    private def isInterfaceObject(source: String, typeName: String): Boolean =
      types.exists(entry => entry.source == source && entry.name == typeName && entry.interfaceObject)

    private def lookupDiagnostics: List[String] =
      lookupResults.flatMap { case (subgraph, results) =>
        LookupCompilation.declarationDiagnostics(subgraph) ::: results.flatMap(_._2.left.getOrElse(Nil))
      }

    private def federationKeyDiagnostics(metadata: SubgraphMetadata): List[String] = {
      val subgraph = metadata.subgraph
      if (!subgraph.federation) Nil
      else
        for {
          definition <- metadata.schema.objectLikeTypes
          directive  <- definition.directives
          if metadata.directiveNames.key.contains(directive.name)
          error      <- validateKey(subgraph, definition.name, directive).left.toOption
        } yield error
    }

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
            val result = for {
              selections <- directiveFieldSet(directive)
              parent     <- fieldSetType.toRight("the selected parent type does not exist.")
              _          <- validateFieldSetSelections(subgraph, parent, selections)
            } yield SourceField(subgraph.name, subgraph.rootNames.composed(typeName), field.name) -> selections
            result.left.map(error =>
              s"[${subgraph.name}] Invalid @${directive.name} field set on '$typeName.${field.name}': $error"
            )
          }

        val required = fields.flatMap { case (typeName, field) =>
          compileFieldSet(typeName, field, names.requires, subgraph.rootType.types.get(typeName))
        }
        val provided = fields.flatMap { case (typeName, field) =>
          val returnType = subgraph.rootType.types
            .get(typeName)
            .flatMap(tpe => Option(tpe.getFieldOrNull(field.name)))
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

    private def compileContexts(metadata: SubgraphMetadata): Either[List[String], FederationContexts] = {
      val subgraph = metadata.subgraph
      val names    = metadata.directiveNames
      if (!subgraph.federation || !names.supportsContexts) Right(FederationContexts(Nil, Nil))
      else {
        val contextDirectives = metadata.schema.contextTypes.flatMap { tpe =>
          tpe.directives.collect { case directive if names.context.contains(directive.name) => tpe.name -> directive }
        }
        val typesByContext    = contextDirectives.flatMap { case (typeName, directive) =>
          directive.arguments.get("name").collect { case StringValue(name) => ContextName(name) -> typeName }
        }.groupMap(_._1)(_._2)

        def compileContextArgument(
          typeName: String,
          field: FieldDefinition,
          argument: InputValueDefinition,
          directive: Directive
        ): Either[String, (SourceField, ContextArgument)] = {
          val parentName = subgraph.rootNames.composed(typeName)
          val coordinate = s"$parentName.${field.name}(${argument.name}:)"
          val result     = for {
            value             <- directive.arguments
                                   .get("field")
                                   .collect { case StringValue(value) => value }
                                   .toRight("the 'field' argument must be a string.")
            parsed            <- parseContextSelection(value).toRight("the context selection could not be parsed.")
            (name, selections) = parsed
            contextTypes       = typesByContext.getOrElse(name, Nil)
            argumentType      <- subgraph.rootType.types
                                   .get(typeName)
                                   .flatMap(tpe => Option(tpe.getFieldOrNull(field.name)))
                                   .flatMap(_.allArgs.find(_.name == argument.name))
                                   .map(_._type)
                                   .toRight("the context argument does not exist in the source schema.")
            _                 <- Either.cond(contextTypes.nonEmpty, (), s"context '${name.value}' is not declared by this subgraph.")
            _                 <- Either.cond(argument.ofType.nullable, (), "context arguments must be nullable.")
            _                 <- Either.cond(argument.defaultValue.isEmpty, (), "context arguments must not define a default value.")
            _                 <- validateContextReceiver(subgraph.name, parentName, field.name)
            _                 <- validateContextSelectionSyntax(selections)
            parents           <- contextParents(subgraph, contextTypes)
            _                 <- validateContextTypeConditions(subgraph.rootType, parents, selections)
            // The last-declared context type is checked first, so its error is the one reported.
            _                 <- traverseEither(parents.reverse)(validateContextValue(subgraph, _, selections, argumentType))
          } yield SourceField(subgraph.name, parentName, field.name) -> ContextArgument(argument.name, name, selections)
          result.left.map(error =>
            s"[${subgraph.name}] Invalid Federation @fromContext application at '$coordinate': $error"
          )
        }

        val declarations = contextDirectives.map { case (typeName, directive) =>
          contextDeclaration(subgraph, typeName, directive)
        }
        val bindings     = metadata.schema.fields.flatMap { case (typeName, field) =>
          for {
            argument  <- field.args
            directive <- argument.directives
            if names.fromContext.contains(directive.name)
          } yield compileContextArgument(typeName, field, argument, directive)
        }

        (validateAll(declarations), validateAll(bindings)) match {
          case (Right(declared), Right(bound)) =>
            val declaredContexts = declared.groupMap(_._1)(_._2).map { case (sourceType, contexts) =>
              sourceType -> contexts.toSet
            }
            Right(FederationContexts(declaredContexts.toList, bound.groupMap(_._1)(_._2).toList))
          case (declared, bound)               => Left(declared.left.getOrElse(Nil) ::: bound.left.getOrElse(Nil))
        }
      }
    }

    private def validateContextReceiver(source: String, typeName: String, fieldName: String): Either[String, Unit] = {
      val receiver        = types.find(entry => entry.source == source && entry.name == typeName)
      val parent          = receiver.map(_.tpe)
      val implementsField = parent.toList.flatMap(_.interfaces().getOrElse(Nil)).exists { interface =>
        Option(interface.getFieldOrNull(fieldName)).nonEmpty
      }
      val isEntityObject  =
        receiver.exists(_.entity.exists(_.lookups.nonEmpty)) && parent.exists(_.kind == __TypeKind.OBJECT)
      if (implementsField) Left("context arguments cannot be used on fields that implement interface fields.")
      else if (!isEntityObject) Left("the containing object must define a resolvable entity lookup.")
      else Right(())
    }

    private def contextParents(subgraph: PreparedSubgraph, contextTypes: List[String]): Either[String, List[__Type]] =
      traverseEither(contextTypes) { contextType =>
        if (isInterfaceObject(subgraph.name, subgraph.rootNames.composed(contextType)))
          Left(s"context type '$contextType' cannot be an @interfaceObject.")
        else subgraph.rootType.types.get(contextType).toRight(s"context type '$contextType' does not exist.")
      }

    private def validateContextValue(
      subgraph: PreparedSubgraph,
      parent: __Type,
      selections: List[Selection],
      argumentType: __Type
    ): Either[String, Unit] =
      contextSelectionTypes(subgraph, parent, selections).flatMap { values =>
        Either.cond(
          values.forall(compatibleContextValueType(_, argumentType)),
          (),
          s"the selected value is incompatible with argument type '${DocumentRenderer.renderTypeName(argumentType)}'."
        )
      }

    private def contextSelectionTypes(
      subgraph: PreparedSubgraph,
      parent: __Type,
      selections: List[Selection]
    ): Either[String, List[__Type]] = {
      val rootType = subgraph.rootType

      def isInterfaceObjectType(tpe: __Type): Boolean = tpe.name.exists(isInterfaceObject(subgraph.name, _))

      def validateFragments(values: List[Selection], topLevel: Boolean): Either[String, Unit] =
        traverseEither(values) {
          case field: Selection.Field             => validateFragments(field.selectionSet, topLevel = false)
          case fragment: Selection.InlineFragment =>
            val condition = fragment.typeCondition.flatMap(value => rootType.types.get(value.name))
            if (!topLevel) Left("inline fragments are only allowed at the top level of a context selection.")
            else if (!condition.exists(_.kind == __TypeKind.OBJECT))
              Left("top-level context type conditions must name concrete object types.")
            else if (condition.exists(isInterfaceObjectType)) Left(InterfaceObjectContextSelection)
            else validateFragments(fragment.selectionSet, topLevel = false)
          case _: Selection.FragmentSpread        => Right(())
        }.map(_ => ())

      def projectType(container: __Type, selected: __Type): __Type =
        contextValueType(container) match {
          case list if list.kind == __TypeKind.LIST => list.copy(ofType = list.ofType.map(projectType(_, selected)))
          case _                                    => selected
        }

      def applies(fragment: Selection.InlineFragment, runtime: __Type): Boolean =
        fragment.typeCondition.forall(condition =>
          rootType.types.get(condition.name).exists(contextPossibleTypes(rootType, _).exists(_.name == runtime.name))
        )

      def resolveField(staticType: __Type, field: Selection.Field): Either[String, List[__Type]] =
        if (field.name == TypenameField && field.selectionSet.isEmpty) Right(Types.string :: Nil)
        else
          Option(staticType.getFieldOrNull(field.name))
            .toRight(s"field '${field.name}' does not exist on context type '${staticType.name.getOrElse("")}'.")
            .flatMap { definition =>
              val output = definition._type.innerType
              if (field.selectionSet.isEmpty) Right(contextValueType(definition._type) :: Nil)
              else if (isInterfaceObjectType(output)) Left(InterfaceObjectContextSelection)
              else
                traverseEither(contextPossibleTypes(rootType, output))(resolve(output, _, field.selectionSet))
                  .map(_.flatten.map(projectType(definition._type, _)))
            }

      def resolve(staticType: __Type, runtime: __Type, values: List[Selection]): Either[String, List[__Type]] = {
        val selected = values.filter {
          case _: Selection.Field                 => true
          case fragment: Selection.InlineFragment => applies(fragment, runtime)
          case _: Selection.FragmentSpread        => false
        }
        selected match {
          case Nil                                         => Left("the context selection does not match this context type.")
          case (field: Selection.Field) :: Nil             => resolveField(staticType, field)
          case (fragment: Selection.InlineFragment) :: Nil =>
            val narrowed =
              fragment.typeCondition.flatMap(condition => rootType.types.get(condition.name)).getOrElse(staticType)
            resolve(narrowed, runtime, fragment.selectionSet)
          case _                                           => Left("the context selection resolves to multiple fields.")
        }
      }

      for {
        _      <- Either.cond(!isInterfaceObjectType(parent), (), InterfaceObjectContextSelection)
        _      <- validateFragments(selections, topLevel = true)
        values <- traverseEither(contextPossibleTypes(rootType, parent))(resolve(parent, _, selections))
      } yield values.flatten
    }

    private def compileSecurity(
      metadata: SubgraphMetadata
    ): Either[List[String], List[SecurityDirectiveApplication]] = {
      val source = metadata.subgraph.name
      validateAll(metadata.directiveApplications.flatMap { application =>
        application.securityCoordinate.toList.flatMap { case (typeName, fieldName) =>
          application.directives.flatMap { directive =>
            compileSecurityDirective(source, application.coordinate.display, directive, metadata.directiveNames)
              .map(_.map(SecurityDirectiveApplication(source, typeName, fieldName, _)))
          }
        }
      })
    }

    private def unsupportedFederationDiagnostics(metadata: SubgraphMetadata): List[String] =
      for {
        application <- metadata.directiveApplications
        directive   <- application.directives
        diagnostic  <- unsupportedDirectiveDiagnostics(metadata.directiveNames, application, directive)
      } yield s"[${metadata.subgraph.name}] $diagnostic"

    private def progressiveOverrideSourceDiagnostics: List[String] =
      types.groupBy(_.name).toList.flatMap { case (typeName, entries) =>
        def owns(source: String, field: String): Boolean =
          entries.exists(entry => entry.source == source && entry.ownedFields.contains(field))

        entries.flatMap { entry =>
          entry.overrideFields.toList.collect {
            case (field, directive) if directive.progressive.nonEmpty && !owns(directive.from, field) =>
              s"${fieldDiagnosticPrefix(entry.operation, typeName, field)} Progressive @override in subgraph '${entry.source}' requires its 'from' subgraph '${directive.from}' to own the field."
          }
        }
      }

    private def interfaceOverrideDiagnostics: List[String] = {
      val direct = types
        .filter(_.tpe.kind == __TypeKind.INTERFACE)
        .flatMap(entry =>
          entry.overrideFields.map { case (field, directive) =>
            TypeField(entry.name, field) -> SubgraphOverride(directive.from, entry.source, directive.progressive)
          }
        )
        .groupMap(_._1)(_._2)

      def owns(source: String, interfaceName: String, field: String): Boolean =
        types.exists(entry =>
          entry.source == source && entry.name == interfaceName && entry.ownedFields.contains(field)
        )

      val inheritedCollisions = interfaceOverrides.collect {
        case (TypeField(interfaceName, field), overrides)
            if overrides.size > 1 && overrides.exists(_.progressive.nonEmpty) =>
          s"[type $interfaceName.$field] Multiple @override declarations inherited from implementations are not supported when any declaration is progressive."
      }
      val directCollisions    = direct.toList.flatMap { case (key @ TypeField(interfaceName, field), directOverrides) =>
        interfaceOverrides.get(key).toList.collect {
          case inheritedOverrides if (directOverrides ::: inheritedOverrides).exists(_.progressive.nonEmpty) =>
            s"[type $interfaceName.$field] Direct and inherited @override declarations cannot be combined when any declaration is progressive."
        }
      }
      val missingOwners       = interfaceOverrides.toList.flatMap { case (TypeField(interfaceName, field), overrides) =>
        overrides.filter(_.progressive.nonEmpty).flatMap { overrideDirective =>
          val missing =
            List(overrideDirective.from, overrideDirective.by).distinct.filterNot(owns(_, interfaceName, field))
          check(
            missing.isEmpty,
            s"[type $interfaceName.$field] Progressive @override inherited from an implementation requires every participating subgraph to own the interface field; missing ${formatSources(missing)}."
          )
        }
      }
      inheritedCollisions.toList ::: directCollisions ::: missingOwners
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
            val coordinate = entry.operation.fold(s"${entry.name}.${field.name}")(_ => field.name)
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
      val enforcedSecurity    = security.filterNot(_.directive == SecurityDirective.UnsupportedPolicy)
      val fieldSets           = compiledFieldSets.flatMap(_.toOption)
      val requiredFieldSets   = fieldSets.flatMap(_.requiredFieldSets).toMap
      val contexts            = compiledContexts.flatMap(_.toOption)
      val declaredContexts    = contexts.flatMap(_.declaredContexts).toMap
      val contextBindings     = contexts.flatMap(_.contextBindings).toMap
      val dependencies        = OperationSecurity.dependencies(requiredFieldSets, declaredContexts, contextBindings)
      val diagnostics         =
        invalidTransformationDiagnostics(rootType) ::: composedDirectives.schemaDiagnostics(rootType) :::
          hiddenSecurityDiagnostics(enforcedSecurity, rootType) :::
          missingTransitiveSecurityDiagnostics(dependencies, enforcedSecurity, possibleTypesByName, rootType)

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
              sourceFields = types.flatMap { entry =>
                entry.tpe.allFields.map(field => SourceField(entry.source, entry.name, field.name) -> field)
              }.toMap,
              entityLookupsByType = types
                .flatMap(entry => entry.entity.toList.flatMap(_.lookups).map(SourceType(entry.source, entry.name) -> _))
                .groupMap(_._1)(_._2),
              requiredFieldSets = requiredFieldSets,
              providedFieldSets = fieldSets.flatMap(_.providedFieldSets).toMap,
              declaredContexts = declaredContexts,
              contextBindings = contextBindings,
              interfaceObjects =
                types.filter(_.interfaceObject).map(entry => SourceType(entry.source, entry.name)).toSet,
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
        composedTypes.get(typeName).flatMap(_.allFields.find(_.name == field)).map { composedField =>
          val selectFirst = !isCompositeType(composedField._type.innerType)
          RootField(operation, field) -> RootRoute(fieldRouteCandidates(field, entries), selectFirst)
        }
      }

    private def fieldRoutes(definitions: Map[TypeField, List[SubgraphType]]): Map[TypeField, List[FieldRoute]] =
      definitions.flatMap { case (field, entries) =>
        val routes = applyInterfaceOverrides(
          fieldRouteCandidates(field.fieldName, entries),
          interfaceOverrides.getOrElse(field, Nil)
        )
        if (routes.nonEmpty) Some(field -> routes) else None
      }

    private def fieldRouteCandidates(field: String, entries: List[SubgraphType]): List[FieldRoute] = {
      def owns(source: String): Boolean =
        entries.exists(entry => entry.source == source && entry.ownedFields.contains(field))

      val sources           = effectiveFieldSources(field, entries).map(_.source)
      val overrideDirective = entries.collectFirst(
        Function.unlift(entry =>
          entry.overrideFields
            .get(field)
            .map(directive => SubgraphOverride(directive.from, entry.source, directive.progressive))
        )
      )
      val routes            = overrideDirective match {
        case Some(SubgraphOverride(from, by, Some(progressive))) if owns(from) =>
          val overridingRoutes = sources.map { source =>
            FieldRoute(source, if (source == by) Some(overrideCondition(progressive, active = true)) else None)
          }
          overridingRoutes :+ FieldRoute(from, Some(overrideCondition(progressive, active = false)))
        case _                                                                 => sources.map(FieldRoute(_))
      }
      routes.distinct.sortBy(_.source)
    }

    private def applyInterfaceOverrides(routes: List[FieldRoute], overrides: List[SubgraphOverride]): List[FieldRoute] =
      overrides.foldLeft(routes) { (current, overrideDirective) =>
        overrideDirective.progressive match {
          case None              => current.filterNot(_.source == overrideDirective.from)
          case Some(progressive) =>
            current.map { route =>
              if (route.source == overrideDirective.from)
                route.copy(condition = Some(overrideCondition(progressive, active = false)))
              else if (route.source == overrideDirective.by)
                route.copy(condition = Some(overrideCondition(progressive, active = true)))
              else route
            }
        }
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

    private def hiddenSecurityDiagnostics(
      applications: List[SecurityDirectiveApplication],
      rootType: RootType
    ): List[String] = {
      def isVisible(application: SecurityDirectiveApplication): Boolean =
        rootType.types.get(application.typeName).exists { tpe =>
          application.fieldName.forall(name => tpe.allFields.exists(_.name == name))
        }

      applications.filterNot(isVisible).map { application =>
        s"[${application.source}] Federation ${application.directiveName} at '${application.coordinate}' cannot be enforced because the coordinate is not client-visible."
      }
    }

    private def missingTransitiveSecurityDiagnostics(
      dependencies: List[OperationSecurity.Dependency],
      applications: List[SecurityDirectiveApplication],
      possibleTypesByName: Map[String, Set[String]],
      rootType: RootType
    ): List[String] = {
      def applicable(selectedType: String, candidateType: String): Boolean =
        selectedType == candidateType || {
          val selected  = possibleTypesByName.getOrElse(selectedType, Set.empty)
          val candidate = possibleTypesByName.getOrElse(candidateType, Set.empty)
          selected.exists(candidate.contains)
        }

      def typeApplications(typeName: String): List[SecurityDirectiveApplication] =
        applications.filter(application => application.fieldName.isEmpty && applicable(typeName, application.typeName))

      def fieldApplications(typeName: String, fieldName: String): List[SecurityDirectiveApplication] =
        applications.filter(application =>
          application.fieldName.contains(fieldName) && applicable(typeName, application.typeName)
        )

      def requiredProfiles(selections: List[Selection], parentType: String): List[(String, SecurityProfile)] =
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
                  outputType.toList.flatMap(requiredProfiles(field.selectionSet, _))
              }
          case fragment: Selection.InlineFragment =>
            val selectedType = fragment.typeCondition.fold(parentType)(_.name)
            (selectedType -> SecurityProfile(typeApplications(selectedType))) ::
              requiredProfiles(fragment.selectionSet, selectedType)
          case _: Selection.FragmentSpread        => Nil
        }

      dependencies.flatMap { dependency =>
        val available = SecurityProfile(
          typeApplications(dependency.parentType) ::: fieldApplications(dependency.parentType, dependency.fieldName)
        )
        requiredProfiles(dependency.selections, dependency.dependencyType).collect {
          case (coordinate, required) if !available.implies(required) =>
            s"[${dependency.source}] Field '${dependency.parentType}.${dependency.fieldName}' does not specify sufficient Federation security requirements for ${dependency.directive} dependency '$coordinate'."
        }
      }.distinct.sorted
    }
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

  private[composition] def federationTransportTypes(document: Document, federation: Boolean): Set[String] =
    if (federation) federationDirectiveNames(document).hiddenTypes else Set.empty

  private[composition] def parseContextSelection(value: String): Option[(ContextName, List[Selection])] = {
    def indexOrEnd(index: Int): Int = if (index < 0) value.length else index

    @tailrec def skipIgnored(index: Int): Int =
      if (index >= value.length) index
      else
        value.charAt(index) match {
          case character if character.isWhitespace || character == ',' => skipIgnored(index + 1)
          case '#'                                                     =>
            skipIgnored(indexOrEnd(value.indexWhere(character => character == '\n' || character == '\r', index + 1)))
          case _                                                       => index
        }

    val dollar = skipIgnored(0)
    if (dollar >= value.length || value.charAt(dollar) != '$') None
    else {
      val start = skipIgnored(dollar + 1)
      val end   = indexOrEnd(value.indexWhere(character => !(character.isLetterOrDigit || character == '_'), start))
      val name  = value.substring(start, end)
      if (!isContextName(name)) None
      else {
        val rawSelections = value.substring(skipIgnored(end))
        val parsed        =
          if (rawSelections.startsWith("{")) parseSelectionSet(s"query $rawSelections")
          else parseFieldSet(rawSelections)
        parsed.map(ContextName(name) -> _)
      }
    }
  }

  private[composition] final case class FederationDirectiveNames(
    features: List[LinkedFeature],
    federation2: Boolean,
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
    unavailableCost: Map[String, String],
    cost: Set[String],
    listSize: Set[String],
    context: Set[String],
    fromContext: Set[String],
    supportsContexts: Boolean,
    supportsProgressiveOverride: Boolean,
    hidden: Set[String],
    hiddenTypes: Set[String]
  ) {
    val fieldSetDirectives: Set[String] = key ++ requires ++ provides
  }

  private[composition] def federationDirectiveNames(document: Document): FederationDirectiveNames = {
    val links      = linkedFeatures(document)
    val federation = links.filter(_.identity == FederationIdentity)
    val relevant   =
      links.filter(feature => feature.identity == FederationIdentity || LinkedSpecIdentities(feature.identity))
    val prefixes   = (relevant.map(_.namespace).toSet ++ (if (links.nonEmpty) Set("link") else Set.empty)).map(_ + "__")

    def isNamespaced(name: String): Boolean = prefixes.exists(name.startsWith)

    def federationNames(name: String): Set[String] = federation.flatMap(_.directiveNames(name)).toSet

    def federation1Names(name: String): Set[String] = if (federation.isEmpty) Set(name) else federationNames(name)

    def specNames(name: String, identity: String, federationVersion: FeatureVersion): (Set[String], Set[String]) = {
      val (available, unavailable) = (federation ::: links.filter(_.identity == identity)).partition { feature =>
        if (feature.identity == FederationIdentity)
          feature.version.atLeast(federationVersion.major, federationVersion.minor)
        else feature.version == FeatureVersion(0, 1)
      }
      available.flatMap(_.directiveNames(name)).toSet -> unavailable.flatMap(_.directiveNames(name)).toSet
    }

    val keyNames                                    = federation1Names("key")
    val externalNames                               = federation1Names("external")
    val extendsNames                                = federation1Names("extends")
    val shareableNames                              = federationNames("shareable")
    val inaccessibleNames                           = federationNames("inaccessible")
    val overrideNames                               = federationNames("override")
    val requiresNames                               = federation1Names("requires")
    val providesNames                               = federation1Names("provides")
    val interfaceObjectNames                        = federationNames("interfaceObject")
    val (authenticated, unavailableAuthenticated)   =
      specNames("authenticated", AuthenticatedIdentity, FeatureVersion(2, 5))
    val (requiresScopes, unavailableRequiresScopes) =
      specNames("requiresScopes", RequiresScopesIdentity, FeatureVersion(2, 5))
    val (policy, unavailablePolicy)                 = specNames("policy", PolicyIdentity, FeatureVersion(2, 6))
    val (cost, unavailableCostNames)                = specNames("cost", CostIdentity, FeatureVersion(2, 9))
    val (listSize, unavailableListSizeNames)        = specNames("listSize", CostIdentity, FeatureVersion(2, 9))
    val unavailableSecurity                         =
      unavailableAuthenticated.map(_ -> "@authenticated").toMap ++
        unavailableRequiresScopes.map(_ -> "@requiresScopes").toMap ++
        unavailablePolicy.map(_ -> "@policy").toMap
    val unavailableCost                             =
      unavailableCostNames.map(_ -> "@cost").toMap ++ unavailableListSizeNames.map(_ -> "@listSize").toMap
    val context                                     = federationNames("context")
    val fromContext                                 = federationNames("fromContext")
    val hidden                                      =
      Set("link") ++ keyNames ++ externalNames ++ extendsNames ++ shareableNames ++ inaccessibleNames ++
        overrideNames ++ requiresNames ++ providesNames ++ interfaceObjectNames ++ federationNames("tag") ++
        federationNames("composeDirective") ++ authenticated ++ requiresScopes ++ policy ++
        unavailableSecurity.keySet ++ unavailableCost.keySet ++ cost ++ listSize ++ context ++ fromContext ++
        document.directiveDefinitions.iterator.map(_.name).filter(isNamespaced)
    val hiddenTypes                                 =
      document.typeDefinitions.iterator.map(_.name).filter(isNamespaced).toSet ++
        relevant.flatMap(_.imports).collect { case value if !value.isDirective => value.alias } ++
        Set(AnyType, "_Entity", "_FieldSet", ServiceType)

    FederationDirectiveNames(
      features = links,
      federation2 = federation.nonEmpty,
      key = keyNames,
      external = externalNames,
      extendsDirective = extendsNames,
      shareable = shareableNames,
      inaccessible = inaccessibleNames,
      overrideDirective = overrideNames,
      requires = requiresNames,
      provides = providesNames,
      interfaceObject = interfaceObjectNames,
      authenticated = authenticated,
      requiresScopes = requiresScopes,
      policy = policy,
      unavailableSecurity = unavailableSecurity,
      unavailableCost = unavailableCost,
      cost = cost,
      listSize = listSize,
      context = context,
      fromContext = fromContext,
      supportsContexts = federation.exists(_.version.atLeast(2, 8)),
      supportsProgressiveOverride = federation.exists(_.version.atLeast(2, 7)),
      hidden = hidden,
      hiddenTypes = hiddenTypes
    )
  }

  private val ProgressiveOverridePattern      = raw"percent\((\d{1,2}(?:\.\d{1,8})?|100)\)".r
  private val CustomOverrideLabelPattern      = raw"[a-zA-Z][a-zA-Z0-9_\-:./]*".r
  private val ContextNamePattern              = raw"[A-Za-z][A-Za-z0-9]*".r
  private val InterfaceObjectContextSelection = "context selections cannot reference an @interfaceObject type."
  private val LinkedSpecIdentities            = Set(AuthenticatedIdentity, RequiresScopesIdentity, PolicyIdentity, CostIdentity)

  private def isContextName(name: String): Boolean = ContextNamePattern.pattern.matcher(name).matches()

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
      directive.arguments.get("from").collect { case StringValue(from) =>
        val progressive = directive.arguments
          .get("label")
          .collect { case StringValue(label) => label }
          .flatMap(parseProgressiveOverrideLabel(_).toOption)
        FieldOverride(from, progressive)
      }
    }

  private def unsupportedDirectiveDiagnostics(
    names: FederationDirectiveNames,
    application: TypeSystemDirectiveApplication,
    directive: Directive
  ): List[String] = {
    val coordinate = application.coordinate.display
    val security   = securityDirectiveName(directive.name, names).collect {
      case name if application.securityCoordinate.isEmpty => s"Federation $name is not supported at '$coordinate'."
    }
    List(
      security,
      names.unavailableSecurity
        .get(directive.name)
        .map(name => s"Federation $name is not available in the linked feature version at '$coordinate'."),
      names.unavailableCost
        .get(directive.name)
        .map(name => s"Federation $name requires Federation v2.9 or cost spec v0.1 at '$coordinate'."),
      contextDiagnostic(names, application, directive.name),
      overrideDiagnostic(names, application, directive)
    ).flatten
  }

  private def contextDiagnostic(
    names: FederationDirectiveNames,
    application: TypeSystemDirectiveApplication,
    directiveName: String
  ): Option[String] = {
    val coordinate    = application.coordinate.display
    val isContext     = names.context.contains(directiveName)
    val isFromContext = names.fromContext.contains(directiveName)
    if ((isContext || isFromContext) && !names.supportsContexts)
      Some(
        s"Federation ${if (isContext) "@context" else "@fromContext"} is not available in the linked feature version at '$coordinate'."
      )
    else if (isContext && !application.supportsContext)
      Some(s"Federation @context is not supported at '$coordinate'.")
    else if (isFromContext && !application.supportsFromContext)
      Some(s"Federation @fromContext is not supported at '$coordinate'.")
    else None
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

  private def securityDirectiveName(name: String, names: FederationDirectiveNames): Option[String] =
    if (names.authenticated.contains(name)) Some("@authenticated")
    else if (names.requiresScopes.contains(name)) Some("@requiresScopes")
    else if (names.policy.contains(name)) Some("@policy")
    else None

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
        scopeGroups(directive.arguments)
          .map(SecurityDirective.RequiresScopes(_))
          .toRight(s"[$source] Invalid Federation @requiresScopes application at '$coordinate'.")
      )
    else if (names.policy.contains(directive.name)) Some(Right(SecurityDirective.UnsupportedPolicy))
    else None

  private def scopeGroups(arguments: Map[String, InputValue]): Option[List[List[String]]] =
    arguments.get("scopes") match {
      case Some(InputValue.ListValue(groups)) if arguments.size == 1 =>
        traverseOption(groups) {
          case InputValue.ListValue(scopes) => stringValues(scopes)
          case _                            => None
        }
      case _                                                         => None
    }

  private def contextDeclaration(
    subgraph: PreparedSubgraph,
    typeName: String,
    directive: Directive
  ): Either[String, (SourceType, ContextName)] =
    directive.arguments.get("name") match {
      case Some(StringValue(name)) if isContextName(name) =>
        subgraph.rootNames.composed(typeName) match {
          case operation @ ("Mutation" | "Subscription") =>
            Left(s"[${subgraph.name}] Federation @context is not supported on the $operation root type '$typeName'.")
          case composedName                              => Right(SourceType(subgraph.name, composedName) -> ContextName(name))
        }
      case Some(StringValue(name))                        =>
        Left(s"[${subgraph.name}] Invalid Federation @context name '$name' on '$typeName'.")
      case _                                              =>
        Left(s"[${subgraph.name}] Invalid Federation @context application on '$typeName'.")
    }

  private def validateContextSelectionSyntax(selections: List[Selection]): Either[String, Unit] =
    selections.foldLeft[Either[String, Unit]](Right(())) {
      case (result, Selection.Field(alias, _, _, directives, children, _)) =>
        result.flatMap(_ =>
          if (alias.nonEmpty) Left("aliases are not allowed in a context selection.")
          else if (directives.nonEmpty) Left("directives are not allowed in a context selection.")
          else validateContextSelectionSyntax(children)
        )
      case (result, Selection.InlineFragment(_, directives, children))     =>
        result.flatMap(_ =>
          if (directives.nonEmpty) Left("directives are not allowed in a context selection.")
          else validateContextSelectionSyntax(children)
        )
      case (_, _: Selection.FragmentSpread)                                =>
        Left("fragment spreads are not allowed in a context selection.")
    }

  private def validateContextTypeConditions(
    rootType: RootType,
    locations: List[__Type],
    selections: List[Selection]
  ): Either[String, Unit] = {
    val locationTypes = locations.iterator.flatMap(contextPossibleTypes(rootType, _)).flatMap(_.name).toSet

    def matchesNoLocation(condition: String): Boolean =
      rootType.types
        .get(condition)
        .forall(tpe => contextPossibleTypes(rootType, tpe).flatMap(_.name).forall(!locationTypes(_)))

    val unused = selections.collect {
      case Selection.InlineFragment(Some(condition), _, _) if matchesNoLocation(condition.name) => condition.name
    }.distinct.sorted
    Either.cond(
      unused.isEmpty,
      (),
      s"top-level context type conditions do not match a context location: ${unused.mkString(", ")}."
    )
  }

  private def contextPossibleTypes(rootType: RootType, tpe: __Type): List[__Type] =
    if (!isAbstractType(tpe)) tpe :: Nil
    else {
      val direct = tpe.possibleTypes.getOrElse(Nil)
      if (direct.nonEmpty) direct
      else
        rootType.types.valuesIterator.filter { candidate =>
          candidate.kind == __TypeKind.OBJECT && candidate.interfaces().getOrElse(Nil).exists(_.name == tpe.name)
        }.toList
    }

  private def contextValueType(tpe: __Type): __Type =
    tpe.kind match {
      case __TypeKind.NON_NULL => tpe.ofType.fold(tpe)(contextValueType)
      case __TypeKind.LIST     => tpe.copy(ofType = tpe.ofType.map(contextValueType))
      case _                   => tpe
    }

  private def compatibleContextValueType(selected: __Type, argument: __Type): Boolean = {
    val selectedValue = contextValueType(selected)
    val argumentValue = contextValueType(argument)
    (selectedValue.kind, argumentValue.kind) match {
      case (__TypeKind.LIST, __TypeKind.LIST) =>
        (selectedValue.ofType, argumentValue.ofType) match {
          case (Some(selectedItem), Some(argumentItem)) => compatibleContextValueType(selectedItem, argumentItem)
          case _                                        => false
        }
      case _                                  =>
        selectedValue.kind == argumentValue.kind && selectedValue.name == argumentValue.name
    }
  }

  private def validateKey(subgraph: PreparedSubgraph, typeName: String, directive: Directive): Either[String, Unit] = {
    val result = for {
      selections <- directiveFieldSet(directive)
      _          <- keyFields(selections).toRight("only fields without aliases, arguments, or directives can be selected.")
      parent     <- subgraph.rootType.types.get(typeName).toRight("the selected parent type does not exist.")
      _          <- validateFieldSetSelections(subgraph, parent, selections)
    } yield ()
    result.left.map(error => s"[${subgraph.name}] Invalid @${directive.name} field set on '$typeName': $error")
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

  private def directiveFieldSet(directive: Directive): Either[String, List[Selection]] =
    for {
      value      <- directive.arguments
                      .get("fields")
                      .collect { case StringValue(value) => value }
                      .toRight("the 'fields' argument must be a string.")
      selections <- parseFieldSet(value).toRight("the selection could not be parsed.")
    } yield selections

  private def keyFields(selections: List[Selection]): Option[List[KeyField]] =
    traverseOption(selections) {
      case Selection.Field(None, name, arguments, directives, children, _) if arguments.isEmpty && directives.isEmpty =>
        keyFields(children).map(KeyField(name, _))
      case _                                                                                                          => None
    }

  private def keyDirective(directive: Directive, names: FederationDirectiveNames): Option[FederationKey] =
    if (!names.key.contains(directive.name)) None
    else
      directiveFieldSet(directive).toOption.flatMap { selections =>
        keyFields(selections)
          .map(fields => FederationKey(fields, !directive.arguments.get("resolvable").contains(BooleanValue(false))))
      }

  private def federationKeyFields(
    subgraph: PreparedSubgraph,
    names: FederationDirectiveNames,
    schema: SchemaInspection
  ): Set[TypeField] =
    schema.objectLikeTypes.iterator.flatMap { definition =>
      definition.directives.iterator
        .flatMap(keyDirective(_, names))
        .flatMap(key => collectKeyFields(subgraph.rootType, definition.name, key.fields))
    }.toSet

  private def federation1ExtensionKeyFields(
    subgraph: PreparedSubgraph,
    names: FederationDirectiveNames,
    schema: SchemaInspection
  ): Set[TypeField] =
    if (names.federation2) Set.empty
    else
      schema.objectLikeTypes.iterator
        .filter(definition =>
          subgraph.federation1ExtensionTypes(definition.name) ||
            hasDirective(definition.directives, names.extendsDirective)
        )
        .flatMap { definition =>
          definition.directives.iterator
            .flatMap(keyDirective(_, names))
            .flatMap(_.fields.map(field => TypeField(definition.name, field.name)))
        }
        .toSet

  private def collectKeyFields(rootType: RootType, typeName: String, fields: List[KeyField]): List[TypeField] =
    fields.flatMap { field =>
      val children =
        if (field.children.isEmpty) Nil
        else
          rootType.types
            .get(typeName)
            .flatMap(_.allFields.find(_.name == field.name))
            .flatMap(_._type.innerType.name)
            .toList
            .flatMap(collectKeyFields(rootType, _, field.children))
      TypeField(typeName, field.name) :: children
    }

  private def hasEntityLookup(subgraph: PreparedSubgraph, entityType: String): Boolean =
    declaresEntityLookup(subgraph, entityType) ||
      subgraph.federation && !subgraph.rootType.queryType.allFields.exists(_.name == EntitiesField)

  private def declaresEntityLookup(subgraph: PreparedSubgraph, entityType: String): Boolean =
    subgraph.rootType.queryType.allFields.find(_.name == EntitiesField) match {
      case None        => false
      case Some(field) =>
        val acceptsRepresentations = field.allArgs.find(_.name == RepresentationsArgument).exists { argument =>
          argument._type.isList && argument._type.innerType.name.contains(AnyType)
        }
        val returnsEntities        = field._type.isList && field._type.innerType.name.contains("_Entity")
        val includesEntity         = field._type.innerType.possibleTypes.exists(_.exists(_.name.contains(entityType)))
        acceptsRepresentations && returnsEntities && includesEntity
    }

  private def isTransportField(name: String): Boolean =
    name == EntitiesField || name == ServiceField

  private def hasDirective(directives: List[Directive], names: Set[String]): Boolean =
    directives.exists(directive => names.contains(directive.name))

  private def hasDirective(directives: Option[List[Directive]], names: Set[String]): Boolean =
    directives.exists(_.exists(directive => names.contains(directive.name)))

  private final case class SecurityProfile(authenticated: Boolean, scopes: Option[List[Set[String]]]) {
    def implies(required: SecurityProfile): Boolean =
      (authenticated || !required.authenticated) &&
        SecurityProfile.implies(scopes, required.scopes)
  }

  private object SecurityProfile {
    def apply(applications: List[SecurityDirectiveApplication]): SecurityProfile = {
      val scopes = conjunction(applications.flatMap { application =>
        application.directive match {
          case SecurityDirective.RequiresScopes(values) => Some(values)
          case _                                        => None
        }
      })
      SecurityProfile(
        applications.exists(_.directive == SecurityDirective.Authenticated) || scopes.nonEmpty,
        scopes
      )
    }

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
    directiveNames: FederationDirectiveNames,
    keyFields: Set[TypeField],
    federation1ExtensionKeyFields: Set[TypeField],
    hiddenDirectives: Set[String],
    schema: SchemaInspection,
    directiveApplications: List[TypeSystemDirectiveApplication]
  )

  private final case class FederationKey(fields: List[KeyField], resolvable: Boolean)

  private final case class FederationFieldSets(
    requiredFieldSets: List[(SourceField, List[Selection])],
    providedFieldSets: List[(SourceField, List[Selection])]
  )

  private final case class FederationContexts(
    declaredContexts: List[(SourceType, Set[ContextName])],
    contextBindings: List[(SourceField, List[ContextArgument])]
  )

  private final case class TypeSystemDirectiveApplication(coordinate: Coordinate, directives: List[Directive]) {
    def securityCoordinate: Option[(String, Option[String])] =
      coordinate match {
        case TypeCoordinate(typeName, location)
            if location == __DirectiveLocation.SCALAR || location == __DirectiveLocation.OBJECT ||
              location == __DirectiveLocation.INTERFACE || location == __DirectiveLocation.ENUM =>
          Some(typeName -> None)
        case FieldCoordinate(typeName, fieldName) => Some(typeName -> Some(fieldName))
        case _                                    => None
      }

    def supportsOverride: Boolean = coordinate.isInstanceOf[FieldCoordinate]

    def supportsContext: Boolean =
      coordinate match {
        case TypeCoordinate(_, location) =>
          location == __DirectiveLocation.OBJECT || location == __DirectiveLocation.INTERFACE ||
          location == __DirectiveLocation.UNION
        case _                           => false
      }

    def supportsFromContext: Boolean = coordinate.isInstanceOf[ArgumentCoordinate]
  }

  /**
   * Schema syntax inspected once and shared by directive, context and entity-key compilation.
   */
  private final class SchemaInspection(document: Document) {
    val objectLikeTypes                    = document.typeDefinitions.collect { case value: AggregationTypeDefinition => value }
    val fields                             = objectLikeTypes.flatMap(tpe => tpe.fields.map(tpe.name -> _))
    private val unions                     = document.typeDefinitions.collect { case value: UnionTypeDefinition => value }
    val contextTypes: List[TypeDefinition] = objectLikeTypes ::: unions

    def directiveApplications(composedName: String => String): List[TypeSystemDirectiveApplication] = {
      def application(coordinate: Coordinate, directives: List[Directive]) =
        TypeSystemDirectiveApplication(coordinate, directives)

      // Preserve the diagnostic/application order used by composition.
      val types        = document.typeDefinitions.collect { case value: ScalarTypeDefinition => value } :::
        contextTypes :::
        document.typeDefinitions.collect { case value: EnumTypeDefinition => value } :::
        document.typeDefinitions.collect { case value: InputObjectTypeDefinition => value }
      val applications = types.flatMap { tpe =>
        val name                 = composedName(tpe.name)
        val (location, children) = tpe match {
          case value: AggregationTypeDefinition =>
            val location =
              if (value.isInstanceOf[ObjectTypeDefinition]) __DirectiveLocation.OBJECT
              else __DirectiveLocation.INTERFACE
            location -> value.fields.flatMap { field =>
              application(FieldCoordinate(name, field.name), field.directives) :: field.args.map(argument =>
                application(ArgumentCoordinate(name, field.name, argument.name), argument.directives)
              )
            }
          case value: EnumTypeDefinition        =>
            __DirectiveLocation.ENUM -> value.enumValuesDefinition.map(value =>
              application(EnumValueCoordinate(name, value.enumValue), value.directives)
            )
          case value: InputObjectTypeDefinition =>
            __DirectiveLocation.INPUT_OBJECT -> value.fields.map(field =>
              application(InputFieldCoordinate(name, field.name), field.directives)
            )
          case _: ScalarTypeDefinition          => __DirectiveLocation.SCALAR -> Nil
          case _: UnionTypeDefinition           => __DirectiveLocation.UNION  -> Nil
        }
        application(TypeCoordinate(name, location), tpe.directives) :: children
      }
      document.schemaDefinition.toList.map(value => application(SchemaCoordinate, value.directives)) :::
        applications ::: document.directiveDefinitions.flatMap(definition =>
          definition.args.map(argument =>
            application(DirectiveArgumentCoordinate(definition.name, argument.name), argument.directives)
          )
        )
    }
  }
}
