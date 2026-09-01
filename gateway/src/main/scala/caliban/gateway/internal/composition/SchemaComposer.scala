package caliban.gateway.internal.composition

import caliban.gateway.{ Lookup, PreparedSubgraph }
import caliban.gateway.OperationPolicy.SecurityDirective
import caliban.InputValue
import caliban.introspection.adt._
import caliban.parsing.{ Parser, SourceMapper }
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection }
import caliban.parsing.adt.Definition.TypeSystemDefinition.TypeDefinition._
import caliban.parsing.adt.Definition.TypeSystemExtension.SchemaExtension
import caliban.parsing.adt.Definition.TypeSystemExtension.TypeExtension._
import caliban.rendering.DocumentRenderer
import caliban.schema.RootType
import caliban.validation.{ SchemaValidator, Validator }
import caliban.Value.{ BooleanValue, NullValue, StringValue }

/**
 * Composes loaded subgraphs; all intermediate state is scoped to one composition.
 */
private[gateway] final class SchemaComposer private (subgraphs: List[PreparedSubgraph]) {
  import DirectiveComposition._
  import SchemaComposer._
  import TypeComposition._

  private val sortedSubgraphs     = subgraphs.sortBy(_.name)
  private val namesBySource       =
    sortedSubgraphs.map(subgraph => subgraph.name -> federationDirectiveNames(subgraph.document)).toMap
  private val composedDirectives  =
    DirectiveComposition.compile(
      sortedSubgraphs.map(subgraph => Source(subgraph, namesBySource(subgraph.name).hidden))
    )
  private val prepared            = sortedSubgraphs.map { subgraph =>
    val names       = namesBySource(subgraph.name)
    val federation2 = isFederation2(subgraph.document)
    CompositionSubgraph(
      subgraph,
      names,
      federationKeyCoordinates(subgraph, names),
      federation1ExtensionKeyCoordinates(subgraph, names, federation2),
      composedDirectives.hidden(subgraph.name),
      federation2,
      typeSystemDirectiveApplications(subgraph.document, composedTypeName(subgraph, _))
    )
  }
  private val queryEntries        = rootFields(OperationType.Query)
  private val mutationEntries     = rootFields(OperationType.Mutation)
  private val subscriptionEntries = rootFields(OperationType.Subscription)
  private val types               = nonRootTypes
  private val typeComposition     = new TypeComposition(types, enumUsageByName, composedDirectives)
  private val compiledFieldSets   = prepared.map(federationFieldSets)
  private val compiledContexts    = prepared.map(federationContexts)
  private val compiledSecurity    = prepared.map(securityApplications)

  def compose: Either[List[String], ComposedGraph] = {
    val diagnostics =
      (lookupDiagnostics :::
        prepared.flatMap(federationKeyDiagnostics) :::
        composedDirectives.diagnostics :::
        compiledFieldSets.flatMap(_.fold(identity, _ => Nil)) :::
        compiledContexts.flatMap(_.fold(identity, _ => Nil)) :::
        compiledSecurity.flatMap(_.fold(identity, _ => Nil)) :::
        prepared.flatMap(unsupportedFederationDiagnostics) :::
        rootDiagnostics(OperationType.Query, queryEntries) :::
        rootDiagnostics(OperationType.Mutation, mutationEntries) :::
        rootDiagnostics(OperationType.Subscription, subscriptionEntries) :::
        typeComposition.diagnostics :::
        progressiveOverrideSourceDiagnostics :::
        interfaceOverrideDiagnostics :::
        visibilityDiagnostics(queryEntries ::: mutationEntries ::: subscriptionEntries)).distinct.sorted

    if (diagnostics.nonEmpty) Left(diagnostics)
    else {
      val queryFields                                                   = chooseRootFields(queryEntries)
      val mutationFields                                                = chooseRootFields(mutationEntries)
      val subscriptionFields                                            = chooseRootFields(subscriptionEntries)
      val composedTypes                                                 = typeComposition.composed
      def rewrite(tpe: __Type): __Type                                  = rewriteType(tpe, composedTypes)
      val query                                                         = makeRootType("Query", queryFields, rewrite)
      val mutation                                                      =
        if (mutationFields.nonEmpty) Some(makeRootType("Mutation", mutationFields, rewrite))
        else None
      val additional                                                    =
        composedTypes.toList.sortBy(_._1).map(_._2) :::
          composedDirectives.additionalTypes.filterNot(tpe => tpe.name.exists(composedTypes.contains))
      val rootType                                                      = RootType(
        query,
        mutation,
        if (subscriptionFields.nonEmpty) Some(makeRootType("Subscription", subscriptionFields, rewrite)) else None,
        additional,
        composedDirectives.definitions(rewrite)
      )
      val runtimeTypesByName                                            = rootType.types.iterator.map { case (name, tpe) =>
        name -> tpe.possibleTypeNames
      }.toMap
      val transformationDiagnostics                                     = invalidTransformationDiagnostics(rootType)
      val directiveDiagnostics                                          = composedDirectives.finalDiagnostics(rootType)
      val allSecurity                                                   = compiledSecurity.flatMap(_.toOption).flatten
      val enforcedSecurity                                              = allSecurity.filterNot(_.directive == SecurityDirective.UnsupportedPolicy)
      val securityVisibilityDiagnostics                                 = hiddenSecurityDiagnostics(enforcedSecurity, rootType)
      val routes: Map[(OperationType, String), ComposedGraph.RootRoute] =
        rootRoutes(OperationType.Query, queryEntries) ++
          rootRoutes(OperationType.Mutation, mutationEntries) ++ rootRoutes(
            OperationType.Subscription,
            subscriptionEntries
          )
      val fieldDefinitions                                              = types
        .flatMap(entry => entry.tpe.allFields.map(field => (entry.name -> field.name) -> entry))
        .groupBy(_._1)
      val interfaceOverrides                                            = interfaceOverrideTargets(types)
      val fieldRoutes                                                   = fieldDefinitions.flatMap { case (coordinate, definitions) =>
        val declared = definitions.map(_._2)
        val routes   = applyInterfaceOverrides(
          fieldProviderRoutes(coordinate._2, declared),
          interfaceOverrides.getOrElse(coordinate, Nil)
        )
        if (routes.nonEmpty) Some(coordinate -> routes) else None
      }
      val sourceFields                                                  = (types.flatMap { entry =>
        entry.tpe.allFields.map(field => (entry.source, entry.name, field.name) -> field)
      } ::: List(
        "Query"        -> queryEntries,
        "Mutation"     -> mutationEntries,
        "Subscription" -> subscriptionEntries
      ).flatMap { case (owner, entries) =>
        entries.map(entry => (entry.source, owner, entry.field.name) -> entry.field)
      }).toMap
      val lookups                                                       = types
        .flatMap(entry => entry.entity.toList.flatMap(_.lookups).map((entry.source -> entry.name) -> _))
        .groupBy(_._1)
        .map { case (coordinate, values) => coordinate -> values.map(_._2) }
      val fieldSets                                                     = compiledFieldSets.flatMap(_.toOption)
      val requirements                                                  = fieldSets.flatMap(_.requirements).toMap
      val provisions                                                    = fieldSets.flatMap(_.provisions).toMap
      val contextMetadata                                               = compiledContexts.flatMap(_.toOption)
      val contexts                                                      = contextMetadata.flatMap(_.definitions).toMap
      val contextArguments                                              = contextMetadata.flatMap(_.arguments).toMap
      val contextSecurityDependencies                                   = contextMetadata.flatMap { metadata =>
        metadata.arguments.flatMap { case ((source, typeName, fieldName), arguments) =>
          arguments.flatMap { argument =>
            metadata.definitions.collect {
              case ((`source`, contextType), names) if names.contains(argument.context) =>
                (source, typeName, fieldName, contextType, argument.selections)
            }
          }
        }
      }
      val transitiveSecurityDiagnostics                                 = missingTransitiveSecurityDiagnostics(
        requirements,
        contextSecurityDependencies,
        enforcedSecurity,
        runtimeTypesByName,
        rootType
      )
      if (
        transformationDiagnostics.nonEmpty || directiveDiagnostics.nonEmpty ||
        securityVisibilityDiagnostics.nonEmpty ||
        transitiveSecurityDiagnostics.nonEmpty
      )
        Left(
          (transformationDiagnostics ::: directiveDiagnostics ::: securityVisibilityDiagnostics :::
            transitiveSecurityDiagnostics).distinct.sorted
        )
      else
        SchemaValidator
          .validateRootType(rootType)
          .left
          .map(error => List(s"[composition] ${error.getMessage}"))
          .map(_ =>
            new ComposedGraph(
              rootType,
              runtimeTypesByName,
              routes,
              fieldRoutes,
              sourceFields,
              lookups,
              requirements,
              provisions,
              contexts,
              contextArguments,
              types.iterator.filter(_.interfaceObject).map(entry => entry.source -> entry.name).toSet,
              sortedSubgraphs.iterator.flatMap { subgraph =>
                subgraph.rootType.types.iterator.map { case (name, tpe) =>
                  (subgraph.name -> name) -> tpe.possibleTypeNames
                }
              }.toMap,
              sortedSubgraphs.iterator.map(subgraph => subgraph.name -> subgraph.mapping).toMap,
              allSecurity,
              composedDirectives.schemaDirectives
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

  private def missingTransitiveSecurityDiagnostics(
    requirements: Map[(String, String, String), List[Selection]],
    contextDependencies: List[(String, String, String, String, List[Selection])],
    applications: List[ComposedGraph.SecurityApplication],
    runtimeTypesByName: Map[String, Set[String]],
    rootType: RootType
  ): List[String] = {
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

    val required = requirements.toList.map { case ((source, sourceType, fieldName), selections) =>
      (source, sourceType, fieldName, sourceType, selections, "@requires")
    }
    val contexts = contextDependencies.map { case (source, sourceType, fieldName, contextType, selections) =>
      (source, sourceType, fieldName, contextType, selections, "@fromContext")
    }
    (required ::: contexts).flatMap { case (source, sourceType, fieldName, dependencyType, selections, directive) =>
      val available = profile(sourceType, Some(fieldName))
      dependencies(selections, dependencyType).collect {
        case (dependency, required) if !available.implies(required) =>
          s"[$source] Field '$sourceType.$fieldName' does not specify sufficient Federation security requirements for $directive dependency '$dependency'."
      }
    }.distinct.sorted
  }

  private def invalidTransformationDiagnostics(
    rootType: RootType
  ): List[String] = {
    val composedTypes = rootType.types

    def emptyType(name: String, kind: __TypeKind): Boolean =
      composedTypes.get(name).exists { tpe =>
        tpe.kind == kind && (kind match {
          case __TypeKind.OBJECT | __TypeKind.INTERFACE => tpe.allFields.isEmpty
          case __TypeKind.INPUT_OBJECT                  => tpe.allInputFields.isEmpty
          case __TypeKind.ENUM                          => tpe.allEnumValues.isEmpty
          case _                                        => false
        })
      }

    sortedSubgraphs.flatMap { subgraph =>
      val mapping = subgraph.mapping
      val fields  = mapping.hiddenFields.collect {
        case (tpe, _) if emptyType(mapping.composedType(tpe), __TypeKind.OBJECT)    =>
          s"[${subgraph.name}] Transformation leaves object '${mapping.composedType(tpe)}' with no visible fields."
        case (tpe, _) if emptyType(mapping.composedType(tpe), __TypeKind.INTERFACE) =>
          s"[${subgraph.name}] Transformation leaves interface '${mapping.composedType(tpe)}' with no visible fields."
      }
      val inputs  = mapping.hiddenInputFields.collect {
        case (tpe, _) if emptyType(mapping.composedType(tpe), __TypeKind.INPUT_OBJECT) =>
          s"[${subgraph.name}] Transformation leaves input object '${mapping.composedType(tpe)}' with no visible fields."
      }
      fields.toList ::: inputs.toList
    }.distinct.sorted
  }

  private def lookupDiagnostics: List[String] =
    sortedSubgraphs.flatMap { subgraph =>
      val sourceKind =
        if (subgraph.federation && subgraph.lookups.nonEmpty)
          List(s"[${subgraph.name}] Ordinary GraphQL lookups cannot be declared on a Federation subgraph.")
        else Nil
      val duplicates = subgraph.lookups
        .groupBy(_.typeName)
        .collect {
          case (typeName, values) if values.size > 1 =>
            s"[${subgraph.name}] More than one lookup is declared for type '$typeName'."
        }
        .toList
      sourceKind ::: duplicates ::: subgraph.lookups.flatMap(validateLookup(subgraph, _))
    }

  private def validateLookup(subgraph: PreparedSubgraph, lookup: Lookup): List[String] = {
    val prefix      = s"[${subgraph.name}]"
    val targetType  = subgraph.rootType.types.get(lookup.typeName)
    val rootName    = subgraph.rootType.queryType.name.getOrElse("Query")
    val sourceField = subgraph.rootType.queryType.allFields.find(_.name == lookup.field)
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
    val unknown     = lookup.arguments.iterator
      .map(_._1)
      .filterNot(arguments.contains)
      .map(name => s"$prefix Lookup field '$rootName.${lookup.field}' has no argument '$name'.")
      .toList
    val duplicates  = lookup.arguments
      .groupBy(_._1)
      .collect {
        case (name, values) if values.size > 1 =>
          s"$prefix Lookup argument '${lookup.field}.$name' is mapped more than once."
      }
      .toList
    val missing     = field.allArgs.collect {
      case argument
          if !lookup.arguments.exists(
            _._1 == argument.name
          ) && !argument._type.isNullable && argument.defaultValue.isEmpty =>
        s"$prefix Required lookup argument '${lookup.field}.${argument.name}' has no mapping."
    }
    val mappings    = lookup.arguments.flatMap { case (name, mapping) =>
      arguments.get(name).toList.flatMap(argument => validateArgument(prefix, name, mapping, argument._type, keys))
    }
    val batch       = lookup match {
      case _: Lookup.Single if lookup.arguments.exists(value => containsBatch(value._2))                            =>
        List(s"$prefix Single lookup argument mappings cannot contain a batch mapping.")
      case _: Lookup.ListLookup if !lookup.arguments.exists(value => containsBatch(value._2))                       =>
        List(s"$prefix List lookup argument mappings must contain a batch mapping.")
      case _: Lookup.ListLookup if lookup.arguments.exists(value => keyOutsideBatch(value._2, insideBatch = false)) =>
        List(s"$prefix List lookup key mappings must be nested inside a batch mapping.")
      case _                                                                                                        => Nil
    }
    val mappedKeys  = lookup.arguments.iterator.flatMap(value => argumentKeys(value._2)).toSet
    val keyCoverage =
      if (mappedKeys == lookup.keyFields.toSet) Nil
      else List(s"$prefix Lookup argument mappings must use every declared key field.")

    unknown ::: duplicates ::: missing ::: mappings ::: batch ::: keyCoverage
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
  ): List[String] = {
    val fields      = lookup.correlation
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
    operation: OperationType
  ): List[SubgraphRootField] =
    prepared.flatMap { metadata =>
      val subgraph = metadata.subgraph
      val root     = operation match {
        case OperationType.Query        => Some(subgraph.rootType.queryType)
        case OperationType.Mutation     => subgraph.rootType.mutationType
        case OperationType.Subscription => subgraph.rootType.subscriptionType
      }
      val names    = metadata.directives
      root.toList.flatMap { rootType =>
        val typeShareable = subgraph.federation && hasDirective(rootType.directives, names.shareable)
        rootType.allFields
          .filterNot(field => subgraph.federation && isTransportField(field.name))
          .map { field =>
            val contextualArguments = field.allArgs
              .filter(argument => subgraph.federation && hasDirective(argument.directives, names.fromContext))
              .map(_.name)
              .toSet
            SubgraphRootField(
              subgraph.name,
              operation,
              field,
              typeShareable || subgraph.federation && hasDirective(field.directives, names.shareable),
              subgraph.federation && hasDirective(field.directives, names.external),
              subgraph.mapping.hiddenFields.contains(rootType.name.getOrElse("") -> field.name) ||
                subgraph.federation && hasDirective(field.directives, names.inaccessible),
              if (subgraph.federation) fieldOverride(field.directives, names.overrideDirective) else None,
              subgraph.federation,
              metadata.federation2,
              (field.allArgs
                .filter(argument =>
                  subgraph.federation &&
                    hasDirective(argument.directives, names.inaccessible)
                )
                .map(_.name)
                .toSet ++ subgraph.mapping.hiddenArguments.collect {
                case (owner, fieldName, argument) if owner == rootType.name.getOrElse("") && fieldName == field.name =>
                  argument
              } ++ contextualArguments),
              contextualArguments,
              metadata.hiddenDirectives
            )
          }
      }
    }.sortBy(entry => entry.field.name -> entry.source)

  private def rootDiagnostics(
    operation: OperationType,
    fields: List[SubgraphRootField]
  ): List[String] =
    fields
      .groupBy(_.field.name)
      .toList
      .flatMap { case (field, entries) =>
        val providers      = effectiveRootProviders(entries)
        val prefix         = s"[${operation.toString.toLowerCase}.$field]"
        val contextualArgs = entries.iterator.flatMap(_.contextualArguments).toSet
        val compatible     = fieldsCompatible(entries.map(entry => visibleArguments(entry.field, contextualArgs)))
        val contextErrors  = contextualArgumentDiagnostics(
          s"${operation.toString.toLowerCase}.$field",
          entries.map(entry => (entry.source, entry.field, entry.contextualArguments))
        )
        overrideDiagnostics(prefix, entries.map(entry => entry.source -> entry.overrideDirective.map(_.from))) :::
          contextErrors :::
          (if (operation == OperationType.Subscription && (providers.size > 1 || entries.exists(_.shareable)))
             List(s"$prefix Subscription fields require one effective owner and cannot be @shareable.")
           else Nil) :::
          (if (compatible && providers.size > 1 && providers.exists(entry => !entry.federation)) {
             val sources = formatSources(providers.map(_.source))
             List(s"$prefix Field is resolved by multiple ordinary subgraphs: $sources.")
           } else Nil) :::
          (if (
             compatible && providers.size > 1 && providers.forall(_.federation) &&
             providers.exists(entry => entry.federation2 && !entry.shareable)
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

  private def chooseRootFields(fields: List[SubgraphRootField]): List[SubgraphRootField] =
    fields
      .groupBy(_.field.name)
      .toList
      .sortBy(_._1)
      .flatMap { case (_, entries) =>
        if (entries.exists(_.inaccessible)) Nil
        else {
          val providers = effectiveRootProviders(entries).sortBy(_.source)
          providers.headOption.map { selected =>
            val mergedType       = providers.map(_.field._type).reduce(mergeOutputType)
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
    fields: List[SubgraphRootField]
  ): Map[(OperationType, String), ComposedGraph.RootRoute] =
    fields
      .groupBy(_.field.name)
      .flatMap { case (name, entries) =>
        val providers = rootProviderRoutes(entries)
        val composite = entries.headOption.exists(entry =>
          entry.field._type.innerType.kind match {
            case __TypeKind.OBJECT | __TypeKind.INTERFACE | __TypeKind.UNION => true
            case _                                                           => false
          }
        )
        if (entries.exists(_.inaccessible)) None
        else Some((operation -> name) -> ComposedGraph.RootRoute(providers, singleProvider = !composite))
      }

  private def typeSystemDirectiveApplications(
    document: Document,
    composedName: String => String
  ): List[TypeSystemDirectiveApplication] = {
    def unsupported(
      coordinate: String,
      directives: List[Directive],
      supportsFromContext: Boolean = false
    ): TypeSystemDirectiveApplication =
      TypeSystemDirectiveApplication(
        coordinate,
        directives,
        None,
        supportsOverride = false,
        supportsContext = false,
        supportsFromContext
      )

    def typeApplication(
      name: String,
      directives: List[Directive],
      supportsSecurity: Boolean,
      supportsContext: Boolean = false
    ): TypeSystemDirectiveApplication = {
      val typeName = composedName(name)
      TypeSystemDirectiveApplication(
        typeName,
        directives,
        if (supportsSecurity) Some(SecurityCoordinate(typeName, None)) else None,
        supportsOverride = false,
        supportsContext,
        supportsFromContext = false
      )
    }

    def fieldApplications(typeName: String, fields: List[FieldDefinition]): List[TypeSystemDirectiveApplication] = {
      val parent = composedName(typeName)
      fields.flatMap { field =>
        TypeSystemDirectiveApplication(
          s"$parent.${field.name}",
          field.directives,
          Some(SecurityCoordinate(parent, Some(field.name))),
          supportsOverride = true,
          supportsContext = false,
          supportsFromContext = false
        ) :: field.args.map(argument =>
          unsupported(s"$parent.${field.name}(${argument.name}:)", argument.directives, supportsFromContext = true)
        )
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
    val scalarTypes        = document.typeDefinitions.collect { case value: ScalarTypeDefinition =>
      value.name -> value.directives
    } ::: document.typeExtensions.collect { case value: ScalarTypeExtension => value.name -> value.directives }
    val unionTypes         = document.typeDefinitions.collect { case value: UnionTypeDefinition =>
      value.name -> value.directives
    } ::: document.typeExtensions.collect { case value: UnionTypeExtension => value.name -> value.directives }
    val enumTypes          = document.typeDefinitions.collect { case value: EnumTypeDefinition =>
      (value.name, value.directives, value.enumValuesDefinition)
    } ::: document.typeExtensions.collect { case value: EnumTypeExtension =>
      (value.name, value.directives, value.enumValuesDefinition)
    }
    val inputTypes         = document.typeDefinitions.collect { case value: InputObjectTypeDefinition =>
      (value.name, value.directives, value.fields)
    } ::: document.typeExtensions.collect { case value: InputObjectTypeExtension =>
      (value.name, value.directives, value.fields)
    }
    val types              = scalarTypes.map { case (name, directives) =>
      typeApplication(name, directives, supportsSecurity = true)
    } ::: objectLikeEntries(document).flatMap { case (name, directives, fields) =>
      typeApplication(name, directives, supportsSecurity = true, supportsContext = true) :: fieldApplications(
        name,
        fields
      )
    } ::: unionTypes.map { case (name, directives) =>
      typeApplication(name, directives, supportsSecurity = false, supportsContext = true)
    } ::: enumTypes.flatMap { case (name, directives, values) =>
      typeApplication(name, directives, supportsSecurity = true) :: enumApplications(name, values)
    } ::: inputTypes.flatMap { case (name, directives, fields) =>
      typeApplication(name, directives, supportsSecurity = false) :: inputApplications(name, fields)
    }
    val directiveArguments = document.directiveDefinitions.flatMap { definition =>
      definition.args.map(argument => unsupported(s"@${definition.name}(${argument.name}:)", argument.directives))
    }

    schemas ::: types ::: directiveArguments
  }

  private def securityApplications(
    metadata: CompositionSubgraph
  ): Either[List[String], List[ComposedGraph.SecurityApplication]] = {
    val subgraph = metadata.subgraph
    val names    = metadata.directives
    val compiled = metadata.directiveApplications.flatMap { application =>
      application.securityCoordinate.toList.flatMap { securityCoordinate =>
        application.directives.flatMap(directive =>
          compileSecurityDirective(subgraph.name, application.coordinate, directive, names).map(
            _.map { value =>
              ComposedGraph.SecurityApplication(
                subgraph.name,
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
    else if (names.policy.contains(directive.name)) Some(Right(SecurityDirective.UnsupportedPolicy))
    else None

  private def groupedStrings(arguments: Map[String, InputValue], name: String): Option[List[List[String]]] =
    if (arguments.keySet != Set(name)) None
    else
      arguments.get(name).collect { case InputValue.ListValue(groups) => groups }.flatMap { groups =>
        val values = groups.map {
          case InputValue.ListValue(entries) =>
            val strings = entries.collect { case StringValue(value) => value }
            if (strings.size == entries.size) Some(strings) else None
          case _                             => None
        }
        if (values.forall(_.nonEmpty)) Some(values.flatten) else None
      }

  private def composedTypeName(subgraph: PreparedSubgraph, typeName: String): String =
    if (subgraph.rootType.queryType.name.contains(typeName)) "Query"
    else if (subgraph.rootType.mutationType.flatMap(_.name).contains(typeName)) "Mutation"
    else if (subgraph.rootType.subscriptionType.flatMap(_.name).contains(typeName)) "Subscription"
    else typeName

  private def unsupportedFederationDiagnostics(metadata: CompositionSubgraph): List[String] = {
    val subgraph = metadata.subgraph
    val names    = metadata.directives
    metadata.directiveApplications.flatMap { application =>
      application.directives.flatMap { directive =>
        val security          = securityDirectiveName(directive.name, names).collect {
          case name if application.securityCoordinate.isEmpty =>
            s"[${subgraph.name}] Federation $name is not supported at '${application.coordinate}'."
        }
        val unavailable       = names.unavailableSecurity
          .get(directive.name)
          .map(name =>
            s"[${subgraph.name}] Federation $name is not available in the linked feature version at '${application.coordinate}'."
          )
        val context           = {
          val isContext = names.context.contains(directive.name)
          val isFrom    = names.fromContext.contains(directive.name)
          if ((isContext || isFrom) && !names.supportsContexts)
            Some(
              s"[${subgraph.name}] Federation ${if (isContext) "@context"
                else "@fromContext"} is not available in the linked feature version at '${application.coordinate}'."
            )
          else if (isContext && !application.supportsContext)
            Some(s"[${subgraph.name}] Federation @context is not supported at '${application.coordinate}'.")
          else if (isFrom && !application.supportsFromContext)
            Some(s"[${subgraph.name}] Federation @fromContext is not supported at '${application.coordinate}'.")
          else None
        }
        val overrideDirective =
          if (!names.overrideDirective.contains(directive.name)) None
          else if (directive.arguments.get("label").exists(_ != NullValue) && !names.supportsProgressiveOverride)
            Some(
              s"[${subgraph.name}] Federation @override(label:) is not available in the linked feature version at '${application.coordinate}'."
            )
          else
            directive.arguments.get("label") match {
              case Some(StringValue(label)) =>
                parseProgressiveOverrideLabel(label) match {
                  case Left(error)                               => Some(s"[${subgraph.name}] $error At '${application.coordinate}'.")
                  case Right(_) if !application.supportsOverride =>
                    Some(s"[${subgraph.name}] Federation @override is not supported at '${application.coordinate}'.")
                  case Right(_)                                  => None
                }
              case Some(NullValue) | None   =>
                if (!application.supportsOverride)
                  Some(s"[${subgraph.name}] Federation @override is not supported at '${application.coordinate}'.")
                else None
              case Some(_)                  =>
                Some(s"[${subgraph.name}] Invalid Federation @override label at '${application.coordinate}'.")
            }

        security.toList ::: unavailable.toList ::: context.toList ::: overrideDirective.toList
      }
    }
  }

  private def securityDirectiveName(name: String, names: FederationDirectiveNames): Option[String] =
    if (names.authenticated.contains(name)) Some("@authenticated")
    else if (names.requiresScopes.contains(name)) Some("@requiresScopes")
    else if (names.policy.contains(name)) Some("@policy")
    else None

  private def federationFieldSets(
    metadata: CompositionSubgraph
  ): Either[List[String], FederationFieldSets] = {
    val subgraph = metadata.subgraph
    if (!subgraph.federation) Right(FederationFieldSets(Nil, Nil))
    else {
      val names        = metadata.directives
      val fields       = objectLikeEntries(subgraph.document).flatMap { case (name, _, fields) =>
        fields.map(name -> _)
      }
      val requirements = fields.flatMap { case (typeName, field) =>
        val parent = subgraph.rootType.types.get(typeName)
        compileFieldSet(subgraph, typeName, field.name, field.directives, names.requires, parent)
      }
      val provisions   = fields.flatMap { case (typeName, field) =>
        val parent   = subgraph.rootType.types.get(typeName)
        val provided = parent.flatMap(tpe => Option(tpe.getFieldOrNull(field.name))).map(_._type.innerType)
        compileFieldSet(subgraph, typeName, field.name, field.directives, names.provides, provided)
      }
      val errors       = (requirements ::: provisions).collect { case Left(error) => error }

      if (errors.nonEmpty) Left(errors)
      else {
        val compiledRequirements = requirements.collect { case Right(selections) =>
          (subgraph.name, composedTypeName(subgraph, selections._1), selections._2) -> selections._3
        }
        val compiledProvisions   = provisions.collect { case Right(selections) =>
          (subgraph.name, composedTypeName(subgraph, selections._1), selections._2) -> selections._3
        }
        Right(FederationFieldSets(compiledRequirements, compiledProvisions))
      }
    }
  }

  private def federationContexts(
    metadata: CompositionSubgraph
  ): Either[List[String], FederationContexts] = {
    val subgraph = metadata.subgraph
    val names    = metadata.directives
    if (!subgraph.federation || !names.supportsContexts) Right(FederationContexts(Nil, Nil))
    else {
      val typeEntries                                             = objectLikeEntries(subgraph.document).map { case (name, directives, _) =>
        name -> directives
      } :::
        subgraph.document.typeDefinitions.collect { case definition: UnionTypeDefinition =>
          definition.name -> definition.directives
        } ::: subgraph.document.typeExtensions.collect { case extension: UnionTypeExtension =>
          extension.name -> extension.directives
        }
      def nonQueryOperationRoot(typeName: String): Option[String] =
        if (subgraph.rootType.mutationType.flatMap(_.name).contains(typeName)) Some("Mutation")
        else if (subgraph.rootType.subscriptionType.flatMap(_.name).contains(typeName)) Some("Subscription")
        else None
      val declarations                                            = typeEntries.flatMap { case (typeName, directives) =>
        directives.filter(directive => names.context.contains(directive.name)).map { directive =>
          directive.arguments.get("name") match {
            case Some(StringValue(name)) if ContextNamePattern.pattern.matcher(name).matches() =>
              nonQueryOperationRoot(typeName) match {
                case Some(operation) =>
                  Left(
                    s"[${subgraph.name}] Federation @context is not supported on the $operation root type '$typeName'."
                  )
                case None            =>
                  Right(
                    (subgraph.name -> composedTypeName(subgraph, typeName)) -> ComposedGraph.ContextName(name)
                  )
              }
            case Some(StringValue(name))                                                       =>
              Left(s"[${subgraph.name}] Invalid Federation @context name '$name' on '$typeName'.")
            case _                                                                             =>
              Left(s"[${subgraph.name}] Invalid Federation @context application on '$typeName'.")
          }
        }
      }
      val declarationErrors                                       = declarations.collect { case Left(error) => error }
      val declared                                                = declarations.collect { case Right(value) => value }
        .groupBy(_._1)
        .map { case (coordinate, values) => coordinate -> values.map(_._2).toSet }
      val byName                                                  = typeEntries.flatMap { case (typeName, directives) =>
        directives
          .filter(directive => names.context.contains(directive.name))
          .flatMap(
            _.arguments.get("name").collect { case StringValue(name) =>
              (subgraph.name -> ComposedGraph.ContextName(name)) -> typeName
            }
          )
      }.groupBy(_._1).map { case (key, values) => key -> values.map(_._2) }
      val arguments                                               = objectLikeEntries(subgraph.document).flatMap { case (typeName, _, fields) =>
        fields.flatMap { field =>
          field.args.flatMap { argument =>
            argument.directives
              .filter(directive => names.fromContext.contains(directive.name))
              .map { directive =>
                val coordinate = s"${composedTypeName(subgraph, typeName)}.${field.name}(${argument.name}:)"
                val prefix     = s"[${subgraph.name}] Invalid Federation @fromContext application at '$coordinate'"
                for {
                  value             <- directive.arguments
                                         .get("field")
                                         .collect { case StringValue(value) => value }
                                         .toRight(s"$prefix: the 'field' argument must be a string.")
                  parsed            <- parseContextSelection(value).toRight(s"$prefix: the context selection could not be parsed.")
                  (name, selections) = parsed
                  contextTypes       = byName.getOrElse(subgraph.name -> name, Nil)
                  argumentType      <- subgraph.rootType.types
                                         .get(typeName)
                                         .flatMap(tpe => Option(tpe.getFieldOrNull(field.name)))
                                         .flatMap(_.allArgs.find(_.name == argument.name))
                                         .map(_._type)
                                         .toRight(s"$prefix: the context argument does not exist in the source schema.")
                  _                 <- Either.cond(
                                         contextTypes.nonEmpty,
                                         (),
                                         s"$prefix: context '${name.value}' is not declared by this subgraph."
                                       )
                  _                 <- Either.cond(
                                         argument.ofType.nullable,
                                         (),
                                         s"$prefix: context arguments must be nullable."
                                       )
                  _                 <- Either.cond(
                                         argument.defaultValue.isEmpty,
                                         (),
                                         s"$prefix: context arguments must not define a default value."
                                       )
                  _                 <- validateContextReceiver(
                                         subgraph.name,
                                         composedTypeName(subgraph, typeName),
                                         field.name,
                                         prefix
                                       )
                  _                 <- validateContextSelectionSyntax(selections).left.map(error => s"$prefix: $error")
                  contextParents    <-
                    contextTypes.foldLeft[Either[String, List[(String, __Type)]]](Right(Nil)) { (result, contextType) =>
                      for {
                        parents <- result
                        _       <- Either.cond(
                                     !types.exists(entry =>
                                       entry.source == subgraph.name &&
                                         entry.name == composedTypeName(subgraph, contextType) &&
                                         entry.interfaceObject
                                     ),
                                     (),
                                     s"$prefix: context type '$contextType' cannot be an @interfaceObject."
                                   )
                        parent  <- subgraph.rootType.types
                                     .get(contextType)
                                     .toRight(s"$prefix: context type '$contextType' does not exist.")
                      } yield (contextType -> parent) :: parents
                    }
                  _                 <- validateContextTypeConditions(
                                         subgraph.rootType,
                                         contextParents.map(_._2),
                                         selections
                                       ).left.map(error => s"$prefix: $error")
                  _                 <- contextParents.foldLeft[Either[String, Unit]](Right(())) { case (result, (_, parent)) =>
                                         result.flatMap(_ =>
                                           for {
                                             values <- contextSelectionTypes(subgraph.name, subgraph.rootType, parent, selections).left
                                                         .map(error => s"$prefix: $error")
                                             _      <- Either.cond(
                                                         values.forall(compatibleContextValueType(_, argumentType)),
                                                         (),
                                                         s"$prefix: the selected value is incompatible with argument type '${DocumentRenderer
                                                             .renderTypeName(argumentType)}'."
                                                       )
                                           } yield ()
                                         )
                                       }
                } yield (
                  (subgraph.name, composedTypeName(subgraph, typeName), field.name) ->
                    ComposedGraph.ContextArgument(argument.name, name, selections)
                )
              }
          }
        }
      }
      val argumentErrors                                          = arguments.collect { case Left(error) => error }
      if (declarationErrors.nonEmpty || argumentErrors.nonEmpty) Left(declarationErrors ::: argumentErrors)
      else
        Right(
          FederationContexts(
            declared.toList,
            arguments.collect { case Right(value) => value }
              .groupBy(_._1)
              .toList
              .map { case (coordinate, values) => coordinate -> values.map(_._2) }
          )
        )
    }
  }

  private def validateContextReceiver(
    source: String,
    typeName: String,
    fieldName: String,
    prefix: String
  ): Either[String, Unit] = {
    val entries     = types.filter(entry => entry.source == source && entry.name == typeName)
    val receiver    = entries.headOption
    val parent      = receiver.map(_.tpe)
    val implemented = parent.toList.flatMap(_.interfaces().getOrElse(Nil)).exists { interface =>
      Option(interface.getFieldOrNull(fieldName)).nonEmpty
    }
    Either.cond(
      receiver.exists(_.entity.exists(_.lookups.nonEmpty)) && parent.exists(
        _.kind == __TypeKind.OBJECT
      ) && !implemented,
      (),
      if (implemented) s"$prefix: context arguments cannot be used on fields that implement interface fields."
      else s"$prefix: the containing object must define a resolvable entity lookup."
    )
  }

  private def validateContextSelectionSyntax(selections: List[Selection]): Either[String, Unit] = {
    def loop(values: List[Selection]): Either[String, Unit] =
      values.foldLeft[Either[String, Unit]](Right(())) {
        case (result, Selection.Field(alias, _, _, directives, children, _)) =>
          result.flatMap(_ =>
            if (alias.nonEmpty) Left("aliases are not allowed in a context selection.")
            else if (directives.nonEmpty) Left("directives are not allowed in a context selection.")
            else loop(children)
          )
        case (result, Selection.InlineFragment(_, directives, children))     =>
          result.flatMap(_ =>
            if (directives.nonEmpty) Left("directives are not allowed in a context selection.") else loop(children)
          )
        case (_, _: Selection.FragmentSpread)                                =>
          Left("fragment spreads are not allowed in a context selection.")
      }
    loop(selections)
  }

  private def contextRuntimeTypes(rootType: RootType, tpe: __Type): List[__Type] =
    tpe.kind match {
      case __TypeKind.OBJECT                       => tpe :: Nil
      case __TypeKind.INTERFACE | __TypeKind.UNION =>
        val direct = tpe.possibleTypes.getOrElse(Nil)
        if (direct.nonEmpty) direct
        else
          rootType.types.valuesIterator
            .filter(candidate =>
              candidate.kind == __TypeKind.OBJECT && candidate
                .interfaces()
                .getOrElse(Nil)
                .exists(_.name == tpe.name)
            )
            .toList
      case _                                       => tpe :: Nil
    }

  private def validateContextTypeConditions(
    rootType: RootType,
    locations: List[__Type],
    selections: List[Selection]
  ): Either[String, Unit] = {
    val locationTypes = locations.iterator.flatMap(contextRuntimeTypes(rootType, _)).flatMap(_.name).toSet
    val unused        = selections.collect {
      case Selection.InlineFragment(Some(condition), _, _)
          if rootType.types
            .get(condition.name)
            .forall(tpe =>
              contextRuntimeTypes(rootType, tpe).flatMap(_.name).forall(name => !locationTypes.contains(name))
            ) =>
        condition.name
    }.distinct.sorted
    Either.cond(
      unused.isEmpty,
      (),
      s"top-level context type conditions do not match a context location: ${unused.mkString(", ")}."
    )
  }

  private def contextSelectionTypes(
    source: String,
    rootType: RootType,
    parent: __Type,
    selections: List[Selection]
  ): Either[String, List[__Type]] = {
    def isInterfaceObject(tpe: __Type): Boolean =
      tpe.name.exists(name =>
        types.exists(entry => entry.source == source && entry.name == name && entry.interfaceObject)
      )

    def validateFragments(values: List[Selection], topLevel: Boolean): Either[String, Unit] =
      values.foldLeft[Either[String, Unit]](Right(())) {
        case (result, field: Selection.Field)             =>
          result.flatMap(_ => validateFragments(field.selectionSet, topLevel = false))
        case (result, fragment: Selection.InlineFragment) =>
          result.flatMap { _ =>
            val condition = fragment.typeCondition.flatMap(value => rootType.types.get(value.name))
            if (!topLevel) Left("inline fragments are only allowed at the top level of a context selection.")
            else if (!condition.exists(_.kind == __TypeKind.OBJECT))
              Left("top-level context type conditions must name concrete object types.")
            else if (condition.exists(isInterfaceObject))
              Left("context selections cannot reference an @interfaceObject type.")
            else validateFragments(fragment.selectionSet, topLevel = false)
          }
        case (result, _: Selection.FragmentSpread)        => result
      }

    def projectType(container: __Type, selected: __Type): __Type =
      contextValueType(container) match {
        case list if list.kind == __TypeKind.LIST => list.copy(ofType = list.ofType.map(projectType(_, selected)))
        case _                                    => selected
      }

    def applies(fragment: Selection.InlineFragment, runtime: __Type): Boolean =
      fragment.typeCondition.forall(condition =>
        rootType.types.get(condition.name).exists(contextRuntimeTypes(rootType, _).exists(_.name == runtime.name))
      )

    def resolve(staticType: __Type, runtime: __Type, values: List[Selection]): Either[String, List[__Type]] = {
      val selected = values.collect {
        case field: Selection.Field                                           => Left(field)
        case fragment: Selection.InlineFragment if applies(fragment, runtime) => Right(fragment)
      }
      if (selected.size != 1)
        Left(
          if (selected.isEmpty) "the context selection does not match this context type."
          else "the context selection resolves to multiple fields."
        )
      else
        selected.head match {
          case Left(field)     =>
            Option(staticType.getFieldOrNull(field.name))
              .toRight(s"field '${field.name}' does not exist on context type '${staticType.name.getOrElse("")}'.")
              .flatMap { definition =>
                if (field.selectionSet.isEmpty) Right(contextValueType(definition._type) :: Nil)
                else {
                  val output = definition._type.innerType
                  if (isInterfaceObject(output)) Left("context selections cannot reference an @interfaceObject type.")
                  else
                    contextRuntimeTypes(rootType, output).foldLeft[Either[String, List[__Type]]](Right(Nil)) {
                      (result, childRuntime) =>
                        for {
                          accumulated <- result
                          child       <- resolve(output, childRuntime, field.selectionSet)
                        } yield accumulated ::: child.map(projectType(definition._type, _))
                    }
                }
              }
          case Right(fragment) =>
            val narrowed =
              fragment.typeCondition.flatMap(condition => rootType.types.get(condition.name)).getOrElse(staticType)
            resolve(narrowed, runtime, fragment.selectionSet)
        }
    }

    for {
      _      <- Either.cond(!isInterfaceObject(parent), (), "context selections cannot reference an @interfaceObject type.")
      _      <- validateFragments(selections, topLevel = true)
      values <-
        contextRuntimeTypes(rootType, parent).foldLeft[Either[String, List[__Type]]](Right(Nil)) { (result, runtime) =>
          for {
            accumulated <- result
            selected    <- resolve(parent, runtime, selections)
          } yield accumulated ::: selected
        }
    } yield values
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
          case (Some(a), Some(b)) => compatibleContextValueType(a, b)
          case _                  => false
        }
      case _                                  =>
        selectedValue.kind == argumentValue.kind && selectedValue.name == argumentValue.name
    }
  }

  private def compileFieldSet(
    subgraph: PreparedSubgraph,
    typeName: String,
    fieldName: String,
    directives: List[Directive],
    names: Set[String],
    startType: Option[__Type]
  ): Option[Either[String, (String, String, List[Selection])]] =
    directives.find(directive => names.contains(directive.name)).map { directive =>
      val prefix = s"[${subgraph.name}] Invalid @${directive.name} field set on '$typeName.$fieldName'"
      for {
        selections <- directiveFieldSet(directive).left.map(error => s"$prefix: $error")
        parent     <- startType.toRight(s"$prefix: the selected parent type does not exist.")
        _          <- validateFieldSetSelections(subgraph, parent, selections).left.map(error => s"$prefix: $error")
      } yield (typeName, fieldName, selections)
    }

  private def federationKeyDiagnostics(metadata: CompositionSubgraph): List[String] = {
    val subgraph = metadata.subgraph
    if (!subgraph.federation) Nil
    else {
      val definitions = objectLikeEntries(subgraph.document).map { case (name, directives, _) => name -> directives }

      definitions.flatMap { case (typeName, directives) =>
        directives.filter(directive => metadata.directives.key.contains(directive.name)).flatMap { directive =>
          val prefix = s"[${subgraph.name}] Invalid @${directive.name} field set on '$typeName'"
          val result = for {
            selections <- directiveFieldSet(directive).left.map(error => s"$prefix: $error")
            _          <- keyFields(selections)
                            .toRight(
                              s"$prefix: only fields without aliases, arguments, or directives can be selected."
                            )
            parent     <- subgraph.rootType.types
                            .get(typeName)
                            .toRight(s"$prefix: the selected parent type does not exist.")
            _          <- validateFieldSetSelections(subgraph, parent, selections).left.map(error => s"$prefix: $error")
          } yield ()

          result.fold(_ :: Nil, _ => Nil)
        }
      }
    }
  }

  private def validateFieldSetSelections(
    subgraph: PreparedSubgraph,
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

    Validator.validateAll(document, subgraph.rootType.copy(queryType = parent)).left.map(_.msg)
  }

  private def nonRootTypes: List[SubgraphType] = {
    val allTypes        = prepared.flatMap(metadata => metadata.subgraph.rootType.types.valuesIterator.toList)
    val byName          = allTypes.flatMap(tpe => tpe.name.map(_ -> tpe)).groupBy(_._1).map { case (name, values) =>
      name -> values.map(_._2)
    }
    val implementations = allTypes
      .filter(_.kind == __TypeKind.OBJECT)
      .flatMap(tpe =>
        tpe.interfaces().getOrElse(Nil).flatMap(_.name).flatMap(interface => tpe.name.map(interface -> _))
      )
      .groupBy(_._1)
      .map { case (interface, values) => interface -> values.map(_._2).distinct }
    val reachable       = scala.collection.mutable.Set.empty[String]
    val pending         = scala.collection.mutable.Queue.empty[String]

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

    prepared.foreach { metadata =>
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

    prepared.flatMap { metadata =>
      val subgraph  = metadata.subgraph
      val rootNames =
        subgraph.rootType.queryType.name.toSet ++
          subgraph.rootType.mutationType.flatMap(_.name).toSet ++
          subgraph.rootType.subscriptionType.flatMap(_.name).toSet

      subgraph.rootType.types.valuesIterator
        .filterNot(tpe =>
          tpe.name.forall(name =>
            !reachable.contains(name) || rootNames.contains(name) ||
              subgraph.federation && metadata.directives.hiddenTypes.contains(name)
          )
        )
        .flatMap(tpe => tpe.name.map(name => subgraphType(metadata, name, tpe)))
        .toList
    }
  }

  private def subgraphType(metadata: CompositionSubgraph, name: String, tpe: __Type): SubgraphType = {
    val subgraph            = metadata.subgraph
    val definitions         = subgraph.document.typeDefinitions.filter(_.name == name)
    val extensionDirectives = subgraph.document.typeExtensions.collect {
      case extension: ObjectTypeExtension if extension.name == name      =>
        extension.directives
      case extension: InterfaceTypeExtension if extension.name == name   =>
        extension.directives
      case extension: ScalarTypeExtension if extension.name == name      =>
        extension.directives
      case extension: UnionTypeExtension if extension.name == name       =>
        extension.directives
      case extension: EnumTypeExtension if extension.name == name        =>
        extension.directives
      case extension: InputObjectTypeExtension if extension.name == name =>
        extension.directives
    }
    val extensionFields     = subgraph.document.typeExtensions.collect {
      case extension: ObjectTypeExtension if extension.name == name    => extension.fields
      case extension: InterfaceTypeExtension if extension.name == name => extension.fields
    }
    val directives          = definitions.flatMap(_.directives) ::: extensionDirectives.flatten
    val fields              = definitions.flatMap {
      case definition: ObjectTypeDefinition    => definition.fields
      case definition: InterfaceTypeDefinition => definition.fields
      case _                                   => Nil
    } ::: extensionFields.flatten
    val names               = metadata.directives
    val interfaceObject     = subgraph.federation && hasDirective(directives, names.interfaceObject)
    val composedType        =
      if (interfaceObject && tpe.kind == __TypeKind.OBJECT) tpe.copy(kind = __TypeKind.INTERFACE)
      else tpe
    val entity              =
      if (subgraph.federation) {
        val keys = directives.flatMap(keyDirective(_, names))
        if (keys.nonEmpty) {
          val lookups =
            if (hasEntityLookup(subgraph, name))
              keys.collect {
                case key if key.resolvable =>
                  ComposedGraph.EntityLookup(
                    key.fields,
                    ComposedGraph.LookupOperation.FederationEntities(
                      if (declaresEntityLookup(subgraph, name)) Some(key.fields) else None
                    ),
                    if (interfaceObject) Some(name) else None
                  )
              }
            else Nil
          Some(EntityDefinition(keys.flatMap(_.fields.map(_.name)).toSet, lookups))
        } else None
      } else
        subgraph.lookups
          .find(_.typeName == name)
          .map { lookup =>
            val key = lookup.keyFields.map(ComposedGraph.KeyField(_, Nil))
            EntityDefinition(
              lookup.keyFields.toSet,
              compileLookup(subgraph, lookup).toList.map(ComposedGraph.EntityLookup(key, _))
            )
          }
    val typeExternal        = subgraph.federation && hasDirective(directives, names.external)
    val fed1Owned           = metadata.federation1ExtensionKeyCoordinates.collect { case (`name`, field) => field }
    val external            = (fields.collect {
      case field if subgraph.federation && hasDirective(field.directives, names.external) =>
        field.name
    }.toSet ++ (if (typeExternal) fields.map(_.name) else Nil)) -- fed1Owned
    val typeShareable       = subgraph.federation && hasDirective(directives, names.shareable)
    val keyFields           = entity.fold(Set.empty[String])(_.keyFields) ++ metadata.keyCoordinates.collect {
      case (`name`, field) => field
    }
    val shareable           = fields.collect {
      case field if subgraph.federation && hasDirective(field.directives, names.shareable) =>
        field.name
    }.toSet ++ keyFields ++ (if (typeShareable) fields.map(_.name) else Nil)
    val inaccessible        = subgraph.mapping.hiddenTypes.contains(name) ||
      subgraph.federation && hasDirective(directives, names.inaccessible)
    val hiddenFields        = fields.collect {
      case field if subgraph.federation && hasDirective(field.directives, names.inaccessible) => field.name
    }.toSet ++ subgraph.mapping.hiddenFields.collect { case (`name`, field) => field }
    val contextualArgs      = fields.iterator
      .flatMap(field =>
        field.args.iterator.collect {
          case argument if hasDirective(argument.directives, names.fromContext) => field.name -> argument.name
        }
      )
      .toSet
    val hiddenArgs          = subgraph.mapping.hiddenArguments.collect { case (`name`, field, argument) =>
      field -> argument
    } ++ fields.iterator
      .flatMap(field =>
        field.args.iterator.collect {
          case argument if hasDirective(argument.directives, names.inaccessible) =>
            field.name -> argument.name
        }
      )
      .toSet ++ contextualArgs
    val hiddenInputs        = subgraph.mapping.hiddenInputFields.collect { case (`name`, field) => field } ++
      tpe.allInputFields.iterator.collect {
        case field if hasDirective(field.directives, names.inaccessible) => field.name
      }
    val hiddenEnums         =
      tpe.allEnumValues.iterator.collect {
        case value if hasDirective(value.directives, names.inaccessible) => value.name
      }.toSet
    val overrides           = fields.flatMap { field =>
      fieldOverride(field.directives, names.overrideDirective).map(field.name -> _)
    }.toMap
    SubgraphType(
      subgraph.name,
      name,
      composedType,
      interfaceObject,
      entity,
      tpe.allFields.map(_.name).toSet -- external,
      shareable,
      inaccessible,
      hiddenFields,
      hiddenArgs,
      contextualArgs,
      hiddenInputs,
      hiddenEnums,
      overrides,
      metadata.federation2,
      metadata.hiddenDirectives
    )
  }

  private def compileLookup(
    subgraph: PreparedSubgraph,
    lookup: Lookup
  ): Option[ComposedGraph.LookupOperation.GraphQLQuery] =
    subgraph.rootType.queryType.allFields.find(_.name == lookup.field).flatMap { field =>
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
          ComposedGraph.LookupResult.ByKey(value.correlation)
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

  private def effectiveRootProviders(entries: List[SubgraphRootField]): List[SubgraphRootField] = {
    val overridden = entries.flatMap(_.overrideDirective.map(_.from)).toSet
    entries.filterNot(entry => entry.external || overridden.contains(entry.source))
  }

  private def rootProviderRoutes(entries: List[SubgraphRootField]): List[ComposedGraph.FieldRoute] = {
    val overrideDirective = entries.collectFirst(
      Function.unlift(entry =>
        entry.overrideDirective.map(directive => ProviderOverride(directive.from, entry.source, directive.progressive))
      )
    )
    providerRoutes(
      effectiveRootProviders(entries).map(_.source),
      overrideDirective,
      source => entries.exists(entry => entry.source == source && !entry.external)
    )
  }

  private def fieldProviderRoutes(
    field: String,
    entries: List[SubgraphType]
  ): List[ComposedGraph.FieldRoute] = {
    val overrideDirective = entries.collectFirst(
      Function.unlift(entry =>
        entry.overrideFields
          .get(field)
          .map(directive => ProviderOverride(directive.from, entry.source, directive.progressive))
      )
    )
    providerRoutes(
      effectiveFieldProviders(field, entries).map(_.source),
      overrideDirective,
      source => entries.exists(entry => entry.source == source && entry.ownedFields.contains(field))
    )
  }

  private def providerRoutes(
    effectiveSources: List[String],
    overrideDirective: Option[ProviderOverride],
    isOverriddenSourceAvailable: String => Boolean
  ): List[ComposedGraph.FieldRoute] = {
    val routes = overrideDirective.flatMap(directive => directive.progressive.map(directive -> _)) match {
      case None                                                                 => effectiveSources.map(ComposedGraph.FieldRoute(_))
      case Some((directive, _)) if !isOverriddenSourceAvailable(directive.from) =>
        effectiveSources.map(ComposedGraph.FieldRoute(_))
      case Some((directive, value))                                             =>
        val overridingRoutes = effectiveSources.map { source =>
          val condition =
            if (source == directive.by)
              Some(ComposedGraph.OverrideCondition(value.label, value.percentage, active = true))
            else None
          ComposedGraph.FieldRoute(source, condition)
        }
        val overriddenRoute  =
          if (isOverriddenSourceAvailable(directive.from))
            List(
              ComposedGraph.FieldRoute(
                directive.from,
                Some(ComposedGraph.OverrideCondition(value.label, value.percentage, active = false))
              )
            )
          else Nil
        overridingRoutes ::: overriddenRoute
    }
    routes.distinct.sortBy(_.source)
  }

  private def progressiveOverrideSourceDiagnostics: List[String] = {
    def diagnostic(
      prefix: String,
      overridingSource: String,
      overrideDirective: FieldOverride,
      overriddenSourceAvailable: Boolean
    ): List[String] =
      if (overrideDirective.progressive.nonEmpty && !overriddenSourceAvailable)
        List(
          s"$prefix Progressive @override in subgraph '$overridingSource' requires its 'from' subgraph '${overrideDirective.from}' to own the field."
        )
      else Nil

    val rootDiagnostics  = List(
      OperationType.Query        -> queryEntries,
      OperationType.Mutation     -> mutationEntries,
      OperationType.Subscription -> subscriptionEntries
    ).flatMap { case (operation, fields) =>
      fields.groupBy(_.field.name).toList.flatMap { case (field, entries) =>
        entries.flatMap(entry =>
          entry.overrideDirective.toList.flatMap(directive =>
            diagnostic(
              s"[${operation.toString.toLowerCase}.$field]",
              entry.source,
              directive,
              entries.exists(candidate => candidate.source == directive.from && !candidate.external)
            )
          )
        )
      }
    }
    val fieldDiagnostics = types.groupBy(_.name).toList.flatMap { case (typeName, entries) =>
      entries.flatMap(entry =>
        entry.overrideFields.toList.flatMap { case (field, directive) =>
          diagnostic(
            s"[type $typeName.$field]",
            entry.source,
            directive,
            entries.exists(candidate => candidate.source == directive.from && candidate.ownedFields.contains(field))
          )
        }
      )
    }
    rootDiagnostics ::: fieldDiagnostics
  }

  private def applyInterfaceOverrides(
    routes: List[ComposedGraph.FieldRoute],
    overrides: List[ProviderOverride]
  ): List[ComposedGraph.FieldRoute] =
    overrides.foldLeft(routes) { (current, overrideDirective) =>
      overrideDirective.progressive match {
        case None        => current.filterNot(_.source == overrideDirective.from)
        case Some(value) =>
          current.map { route =>
            if (route.source == overrideDirective.from)
              route
                .copy(condition = Some(ComposedGraph.OverrideCondition(value.label, value.percentage, active = false)))
            else if (route.source == overrideDirective.by)
              route
                .copy(condition = Some(ComposedGraph.OverrideCondition(value.label, value.percentage, active = true)))
            else route
          }
      }
    }

  private def interfaceOverrideDiagnostics: List[String] = {
    val inherited = interfaceOverrideTargets(types)
    val direct    = types.iterator
      .filter(_.tpe.kind == __TypeKind.INTERFACE)
      .flatMap(entry =>
        entry.overrideFields.iterator.map { case (field, directive) =>
          (entry.name -> field) -> ProviderOverride(directive.from, entry.source, directive.progressive)
        }
      )
      .toList
      .groupBy(_._1)
      .map { case (coordinate, values) => coordinate -> values.map(_._2) }

    val inheritedCollisions = inherited.collect {
      case ((interfaceName, field), overrides) if overrides.size > 1 && overrides.exists(_.progressive.nonEmpty) =>
        s"[type $interfaceName.$field] Multiple @override declarations inherited from implementations are not supported when any declaration is progressive."
    }
    val directCollisions    = direct.toList.flatMap { case ((interfaceName, field), directOverrides) =>
      inherited.get(interfaceName -> field).toList.collect {
        case inheritedOverrides if (directOverrides ::: inheritedOverrides).exists(_.progressive.nonEmpty) =>
          s"[type $interfaceName.$field] Direct and inherited @override declarations cannot be combined when any declaration is progressive."
      }
    }
    val missingProviders    = inherited.toList.flatMap { case ((interfaceName, field), overrides) =>
      val interfaceEntries = types.filter(_.name == interfaceName)
      overrides.flatMap { overrideDirective =>
        if (overrideDirective.progressive.isEmpty) Nil
        else {
          val missing = List(overrideDirective.from, overrideDirective.by).distinct.filterNot(source =>
            interfaceEntries.exists(entry => entry.source == source && entry.ownedFields.contains(field))
          )
          if (missing.nonEmpty)
            List(
              s"[type $interfaceName.$field] Progressive @override inherited from an implementation requires every participating subgraph to own the interface field; missing ${formatSources(missing)}."
            )
          else Nil
        }
      }
    }
    inheritedCollisions.toList ::: directCollisions ::: missingProviders
  }

  private def visibilityDiagnostics(
    roots: List[SubgraphRootField]
  ): List[String] = {
    val inaccessibleTypes      = types.filter(_.inaccessible).map(_.name).toSet
    val inaccessibleFields     = types.iterator.flatMap(entry => entry.inaccessibleFields.map(entry.name -> _)).toSet
    val inaccessibleInputs     =
      types.iterator.flatMap(entry => entry.inaccessibleInputFields.map(entry.name -> _)).toSet
    val inaccessibleRoots      = roots.iterator
      .filter(_.inaccessible)
      .map(entry => entry.operation -> entry.field.name)
      .toSet
    val rootHiddenArguments    = roots
      .groupBy(entry => entry.operation -> entry.field.name)
      .map { case (coordinate, entries) => coordinate -> entries.iterator.flatMap(_.inaccessibleArguments).toSet }
    val hiddenArguments        = types.iterator
      .flatMap(entry =>
        entry.inaccessibleArguments.map { case (field, argument) =>
          (entry.name, field, argument)
        }
      )
      .toSet
    val rootOutputErrors       = roots.collect {
      case entry
          if !inaccessibleRoots.contains(entry.operation -> entry.field.name) &&
            entry.field._type.innerType.name.exists(inaccessibleTypes.contains) =>
        s"[${entry.source}] Field '${entry.field.name}' must be @inaccessible because its return type is inaccessible."
    }
    val rootArgumentErrors     = roots.flatMap { entry =>
      entry.field.allArgs.collect {
        case argument
            if !inaccessibleRoots.contains(entry.operation -> entry.field.name) &&
              !rootHiddenArguments.getOrElse(entry.operation -> entry.field.name, Set.empty).contains(argument.name) &&
              argument._type.innerType.name.exists(inaccessibleTypes.contains) =>
          s"[${entry.source}] Argument '${entry.field.name}.${argument.name}' must be @inaccessible because its input type is inaccessible."
      }
    }
    val requiredRootArguments  = roots.flatMap { entry =>
      entry.field.allArgs.collect {
        case argument
            if !inaccessibleRoots.contains(entry.operation -> entry.field.name) &&
              rootHiddenArguments.getOrElse(entry.operation -> entry.field.name, Set.empty).contains(argument.name) &&
              !argument._type.isNullable && argument.defaultValue.isEmpty =>
          s"[${entry.source}] Required @inaccessible argument '${entry.field.name}.${argument.name}' must define a default value."
      }
    }
    val accessibleTypes        = types.filterNot(entry => inaccessibleTypes.contains(entry.name))
    val fieldOutputErrors      = accessibleTypes.flatMap { entry =>
      entry.tpe.allFields.collect {
        case field
            if !inaccessibleFields.contains(entry.name -> field.name) &&
              field._type.innerType.name.exists(inaccessibleTypes.contains) =>
          s"[${entry.source}] Field '${entry.name}.${field.name}' must be @inaccessible because its return type is inaccessible."
      }
    }
    val fieldArgumentErrors    = accessibleTypes.flatMap { entry =>
      entry.tpe.allFields.filterNot(field => inaccessibleFields.contains(entry.name -> field.name)).flatMap { field =>
        field.allArgs.collect {
          case argument
              if !hiddenArguments.contains((entry.name, field.name, argument.name)) &&
                argument._type.innerType.name.exists(inaccessibleTypes.contains) =>
            s"[${entry.source}] Argument '${entry.name}.${field.name}.${argument.name}' must be @inaccessible because its input type is inaccessible."
        }
      }
    }
    val requiredFieldArguments = accessibleTypes.flatMap { entry =>
      entry.tpe.allFields.filterNot(field => inaccessibleFields.contains(entry.name -> field.name)).flatMap { field =>
        field.allArgs.collect {
          case argument
              if hiddenArguments.contains((entry.name, field.name, argument.name)) &&
                !argument._type.isNullable && argument.defaultValue.isEmpty =>
            s"[${entry.source}] Required @inaccessible argument '${entry.name}.${field.name}.${argument.name}' must define a default value."
        }
      }
    }
    val inputFieldErrors       = accessibleTypes.flatMap { entry =>
      entry.tpe.allInputFields.collect {
        case field
            if !inaccessibleInputs.contains(entry.name -> field.name) &&
              field._type.innerType.name.exists(inaccessibleTypes.contains) =>
          s"[${entry.source}] Input field '${entry.name}.${field.name}' must be @inaccessible because its input type is inaccessible."
      }
    }
    val requiredInputFields    = accessibleTypes.flatMap { entry =>
      entry.tpe.allInputFields.collect {
        case field
            if inaccessibleInputs.contains(entry.name -> field.name) &&
              !field._type.isNullable && field.defaultValue.isEmpty =>
          s"[${entry.source}] Required @inaccessible input field '${entry.name}.${field.name}' must define a default value."
      }
    }
    rootOutputErrors :::
      rootArgumentErrors :::
      requiredRootArguments :::
      fieldOutputErrors :::
      fieldArgumentErrors :::
      requiredFieldArguments :::
      inputFieldErrors :::
      requiredInputFields
  }

  private def enumUsageByName: Map[String, EnumUsage] = {
    val allTypes = sortedSubgraphs.flatMap(_.rootType.types.values)
    val inputs   = allTypes.iterator.flatMap { tpe =>
      tpe.allInputFields.iterator.flatMap(_._type.innerType.name) ++
        tpe.allFields.iterator.flatMap(_.allArgs.iterator.flatMap(_._type.innerType.name))
    }.toSet
    val outputs  = allTypes.iterator.flatMap(_.allFields.iterator.flatMap(_._type.innerType.name)).toSet

    (inputs ++ outputs).iterator.map(name => name -> EnumUsage(inputs.contains(name), outputs.contains(name))).toMap
  }

  private def directiveString(
    directives: Option[List[Directive]],
    names: Set[String],
    argument: String
  ): Option[String] =
    directives.iterator.flatten
      .find(directive => names.contains(directive.name))
      .flatMap(_.arguments.get(argument))
      .collect { case StringValue(value) => value }

  private def fieldOverride(
    directives: Option[List[Directive]],
    names: Set[String]
  ): Option[FieldOverride] =
    directives.iterator.flatten.find(directive => names.contains(directive.name)).flatMap { directive =>
      directive.arguments.get("from").collect { case StringValue(from) =>
        val progressive = directive.arguments
          .get("label")
          .collect { case StringValue(label) =>
            parseProgressiveOverrideLabel(label).toOption
          }
          .flatten
        FieldOverride(from, progressive)
      }
    }

  private def fieldOverride(directives: List[Directive], names: Set[String]): Option[FieldOverride] =
    fieldOverride(Some(directives), names)

  private def makeRootType(
    name: String,
    fields: List[SubgraphRootField],
    rewrite: __Type => __Type
  ): __Type = {
    val sorted = fields
      .map(entry =>
        composedDirectives.attachField(
          name,
          sanitizeField(entry.field, rewrite, entry.hiddenDirectives, entry.inaccessibleArguments)
        )
      )
      .sortBy(_.name)
    composedDirectives.attachType(
      __Type(
        kind = __TypeKind.OBJECT,
        name = Some(name),
        fields = args => Some(if (args.includeDeprecated.getOrElse(false)) sorted else sorted.filterNot(_.isDeprecated))
      ),
      name
    )
  }

}

private[gateway] object SchemaComposer {
  import TypeComposition._

  private val ProgressiveOverridePattern = raw"percent\((\d{1,2}(?:\.\d{1,8})?|100)\)".r
  private val CustomOverrideLabelPattern = raw"[a-zA-Z][a-zA-Z0-9_\-:./]*".r
  private val ContextNamePattern         = raw"[A-Za-z][A-Za-z0-9]*".r

  private def parseProgressiveOverrideLabel(
    label: String
  ): Either[String, ComposedGraph.ProgressiveOverride] =
    label match {
      case ProgressiveOverridePattern(value) =>
        Right(ComposedGraph.ProgressiveOverride(ComposedGraph.OverrideLabel(label), Some(BigDecimal(value))))
      case CustomOverrideLabelPattern()      =>
        Right(ComposedGraph.ProgressiveOverride(ComposedGraph.OverrideLabel(label), None))
      case _                                 =>
        Left(s"Invalid Federation @override label '$label'.")
    }

  private final case class SecurityProfile(
    authenticated: Boolean,
    scopes: Option[List[Set[String]]]
  ) {
    def implies(required: SecurityProfile): Boolean =
      (authenticated || !required.authenticated) &&
        SecurityProfile.implies(scopes, required.scopes)
  }

  private object SecurityProfile {
    def apply(applications: List[ComposedGraph.SecurityApplication]): SecurityProfile = {
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

  private final case class SubgraphRootField(
    source: String,
    operation: OperationType,
    field: __Field,
    shareable: Boolean,
    external: Boolean,
    inaccessible: Boolean,
    overrideDirective: Option[FieldOverride],
    federation: Boolean,
    federation2: Boolean,
    inaccessibleArguments: Set[String],
    contextualArguments: Set[String],
    hiddenDirectives: Set[String]
  )

  private final case class CompositionSubgraph(
    subgraph: PreparedSubgraph,
    directives: FederationDirectiveNames,
    keyCoordinates: Set[(String, String)],
    federation1ExtensionKeyCoordinates: Set[(String, String)],
    hiddenDirectives: Set[String],
    federation2: Boolean,
    directiveApplications: List[TypeSystemDirectiveApplication]
  )

  private final case class FederationFieldSets(
    requirements: List[((String, String, String), List[Selection])],
    provisions: List[((String, String, String), List[Selection])]
  )

  private final case class FederationContexts(
    definitions: List[((String, String), Set[ComposedGraph.ContextName])],
    arguments: List[((String, String, String), List[ComposedGraph.ContextArgument])]
  )

  private final case class SecurityCoordinate(typeName: String, fieldName: Option[String])

  private final case class TypeSystemDirectiveApplication(
    coordinate: String,
    directives: List[Directive],
    securityCoordinate: Option[SecurityCoordinate],
    supportsOverride: Boolean,
    supportsContext: Boolean,
    supportsFromContext: Boolean
  )

  private def hasDirective(directives: List[Directive], names: Set[String]): Boolean =
    directives.exists(directive => names.contains(directive.name))

  private def hasDirective(directives: Option[List[Directive]], names: Set[String]): Boolean =
    directives.exists(_.exists(directive => names.contains(directive.name)))

  private def objectLikeEntries(document: Document): List[(String, List[Directive], List[FieldDefinition])] =
    document.typeDefinitions.collect {
      case definition: ObjectTypeDefinition    => (definition.name, definition.directives, definition.fields)
      case definition: InterfaceTypeDefinition => (definition.name, definition.directives, definition.fields)
    } ::: document.typeExtensions.collect {
      case extension: ObjectTypeExtension    => (extension.name, extension.directives, extension.fields)
      case extension: InterfaceTypeExtension => (extension.name, extension.directives, extension.fields)
    }

  private final case class FederationKey(fields: List[ComposedGraph.KeyField], resolvable: Boolean)

  def compose(subgraphs: List[PreparedSubgraph]): Either[List[String], ComposedGraph] =
    new SchemaComposer(subgraphs).compose

  import DirectiveComposition._

  def isFederation(document: Document): Boolean =
    isFederation2(document) || {
      val typeNames = document.typeDefinitions.iterator.map(_.name).toSet
      typeNames.contains("_Any") && typeNames.contains("_Entity") &&
      document.objectTypeDefinitions.exists(_.fields.exists(_.name == "_entities"))
    }

  def fieldSetDirectiveNames(document: Document): (Set[String], Set[String]) = {
    val names = federationDirectiveNames(document)
    (names.key ++ names.requires ++ names.provides) -> names.provides
  }

  def federationTransportTypes(document: Document, federation: Boolean): Set[String] =
    if (federation) federationDirectiveNames(document).hiddenTypes else Set.empty

  private[internal] def formatSources(sources: Iterable[String]): String =
    sources.toList.distinct.sorted.map(source => s"'$source'").mkString(", ")

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
    supportsContexts: Boolean,
    supportsProgressiveOverride: Boolean,
    hidden: Set[String],
    hiddenTypes: Set[String]
  )

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
    val tagNames            = federationNames("tag")
    val composeNames        = federationNames("composeDirective")
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
    val supportsContexts    = federation.exists(_.version.atLeast(2, 8))
    val progressiveOverride = federation.exists(_.version.atLeast(2, 7))
    val hiddenDirectives    = Set("link") ++ keyNames ++ externalNames ++ extendsNames ++ shareableNames ++
      inaccessibleNames ++ overrideNames ++ requiresNames ++ providesNames ++ interfaceObjects ++
      tagNames ++ composeNames ++ authenticated ++ requiresScopes ++ policy ++ unavailableSecurity.keySet ++ context ++
      fromContext ++
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
      supportsContexts,
      progressiveOverride,
      hiddenDirectives,
      hiddenTypes
    )
  }

  private val AuthenticatedIdentity                      = "https://specs.apollo.dev/authenticated"
  private val RequiresScopesIdentity                     = "https://specs.apollo.dev/requiresScopes"
  private val PolicyIdentity                             = "https://specs.apollo.dev/policy"
  private val SecurityFeatureIdentities                  = Set(AuthenticatedIdentity, RequiresScopesIdentity, PolicyIdentity)
  private def isFederation2(document: Document): Boolean =
    linkedFeatures(document).exists(_.identity == FederationIdentity)

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
    subgraph: PreparedSubgraph,
    names: FederationDirectiveNames
  ): Set[(String, String)] = {
    val definitions = objectLikeEntries(subgraph.document).map { case (name, directives, _) => name -> directives }

    definitions.iterator.flatMap { case (typeName, directives) =>
      directives.iterator
        .flatMap(keyDirective(_, names))
        .flatMap(key => keyCoordinates(subgraph.rootType, typeName, key.fields))
    }.toSet
  }

  private def federation1ExtensionKeyCoordinates(
    subgraph: PreparedSubgraph,
    names: FederationDirectiveNames,
    federation2: Boolean
  ): Set[(String, String)] =
    if (federation2) Set.empty
    else {
      val extensions          = subgraph.document.typeExtensions.collect {
        case extension: ObjectTypeExtension    => extension.name -> extension.directives
        case extension: InterfaceTypeExtension => extension.name -> extension.directives
      }
      val extendedDefinitions = subgraph.document.typeDefinitions.collect {
        case definition: ObjectTypeDefinition if hasDirective(definition.directives, names.extendsDirective)    =>
          definition.name -> definition.directives
        case definition: InterfaceTypeDefinition if hasDirective(definition.directives, names.extendsDirective) =>
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

  private def parseContextSelection(value: String): Option[(ComposedGraph.ContextName, List[Selection])] = {
    def skipIgnored(from: Int): Int = {
      var index = from
      var done  = false
      while (index < value.length && !done)
        value.charAt(index) match {
          case character if character.isWhitespace || character == ',' => index += 1
          case '#'                                                     =>
            index += 1
            while (index < value.length && value.charAt(index) != '\n' && value.charAt(index) != '\r') index += 1
          case _                                                       => done = true
        }
      index
    }

    val dollar = skipIgnored(0)
    if (dollar >= value.length || value.charAt(dollar) != '$') None
    else {
      val start = skipIgnored(dollar + 1)
      var end   = start
      while (end < value.length && (value.charAt(end).isLetterOrDigit || value.charAt(end) == '_')) end += 1
      val name  = value.substring(start, end)
      if (!ContextNamePattern.pattern.matcher(name).matches()) None
      else {
        val selectionStart = skipIgnored(end)
        val rawSelections  = value.substring(selectionStart)
        val parsed         =
          if (rawSelections.startsWith("{"))
            Parser.parseQuery(s"query $rawSelections") match {
              case Right(document) => document.operationDefinition(None).map(_.selectionSet)
              case Left(_)         => None
            }
          else parseFieldSet(rawSelections)
        parsed.map(ComposedGraph.ContextName(name) -> _)
      }
    }
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

  private def hasEntityLookup(subgraph: PreparedSubgraph, entityType: String): Boolean =
    declaresEntityLookup(subgraph, entityType) ||
      subgraph.federation && !subgraph.rootType.queryType.allFields.exists(_.name == "_entities")

  private def declaresEntityLookup(subgraph: PreparedSubgraph, entityType: String): Boolean =
    subgraph.rootType.queryType.allFields.find(_.name == "_entities") match {
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
