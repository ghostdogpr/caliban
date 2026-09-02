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
import caliban.schema.{ RootType, Types }
import caliban.validation.{ SchemaValidator, Validator }
import caliban.Value.{ BooleanValue, IntValue, NullValue, StringValue }

import scala.collection.compat._

/**
 * Composes loaded subgraphs; all intermediate state is scoped to one composition.
 */
private[gateway] final class SchemaComposer private (subgraphs: List[PreparedSubgraph]) {
  import DirectiveComposition._
  import SchemaComposer._
  import TypeComposition._

  private val sortedSubgraphs    = subgraphs.sortBy(_.name)
  private val namesBySource      =
    sortedSubgraphs.map(subgraph => subgraph.name -> federationDirectiveNames(subgraph.document)).toMap
  private val composedDirectives =
    DirectiveComposition.compile(
      sortedSubgraphs.map { subgraph =>
        val names = namesBySource(subgraph.name)
        Source(subgraph, names.hidden, names.features, names.interfaceObject)
      }
    )
  private val prepared           = sortedSubgraphs.map { subgraph =>
    val names       = namesBySource(subgraph.name)
    val federation2 = names.federation2
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
  private val types              = rootTypes ::: nonRootTypes
  private val typeComposition    = new TypeComposition(types, enumUsageByName, composedDirectives)
  private val compiledFieldSets  = prepared.map(federationFieldSets)
  private val compiledContexts   = prepared.map(federationContexts)
  private val compiledCosts      = prepared.map(federationCosts)
  private val compiledSecurity   = prepared.map(securityApplications)

  def compose: Either[List[String], ComposedGraph] = {
    val diagnostics =
      (lookupDiagnostics :::
        prepared.flatMap(federationKeyDiagnostics) :::
        composedDirectives.diagnostics :::
        compiledFieldSets.flatMap(_.fold(identity, _ => Nil)) :::
        compiledContexts.flatMap(_.fold(identity, _ => Nil)) :::
        compiledCosts.flatMap(_.fold(identity, _ => Nil)) :::
        compiledSecurity.flatMap(_.fold(identity, _ => Nil)) :::
        prepared.flatMap(unsupportedFederationDiagnostics) :::
        typeComposition.diagnostics :::
        progressiveOverrideSourceDiagnostics :::
        interfaceOverrideDiagnostics :::
        visibilityDiagnostics).distinct.sorted

    if (diagnostics.nonEmpty) Left(diagnostics)
    else {
      val composedTypes                                                 = typeComposition.composed
      def rewrite(tpe: __Type): __Type                                  = rewriteType(tpe, composedTypes)
      val query                                                         = composedTypes("Query")
      val mutation                                                      = composedTypes.get("Mutation").filter(_.allFields.nonEmpty)
      val subscription                                                  = composedTypes.get("Subscription").filter(_.allFields.nonEmpty)
      val operationTypeNames                                            = Set("Query", "Mutation", "Subscription")
      val additional                                                    =
        composedTypes.toList.sortBy(_._1).collect {
          case (name, tpe) if !operationTypeNames.contains(name) => tpe
        } :::
          composedDirectives.additionalTypes.filterNot(tpe => tpe.name.exists(composedTypes.contains))
      val rootType                                                      = RootType(
        query,
        mutation,
        subscription,
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
      val fieldDefinitions                                              = types
        .flatMap(entry => entry.tpe.allFields.map(field => (entry.name -> field.name) -> entry))
        .groupMap(_._1)(_._2)
      val rootDefinitions                                               = fieldDefinitions.filter { case (_, definitions) =>
        definitions.exists(_.operation.nonEmpty)
      }
      val routes: Map[(OperationType, String), ComposedGraph.RootRoute] = rootDefinitions.flatMap {
        case ((typeName, field), definitions) =>
          val operation = definitions.flatMap(_.operation).head
          composedTypes.get(typeName).flatMap(_.allFields.find(_.name == field)).map { composedField =>
            val providers = fieldProviderRoutes(field, definitions)
            val composite = composedField._type.innerType.kind match {
              case __TypeKind.OBJECT | __TypeKind.INTERFACE | __TypeKind.UNION => true
              case _                                                           => false
            }
            (operation -> field) -> ComposedGraph.RootRoute(providers, singleProvider = !composite)
          }
      }
      val interfaceOverrides                                            = interfaceOverrideTargets(types)
      val fieldRoutes                                                   =
        fieldDefinitions.filterNot(_._2.exists(_.operation.nonEmpty)).flatMap { case (coordinate, definitions) =>
          val routes = applyInterfaceOverrides(
            fieldProviderRoutes(coordinate._2, definitions),
            interfaceOverrides.getOrElse(coordinate, Nil)
          )
          if (routes.nonEmpty) Some(coordinate -> routes) else None
        }
      val sourceFields                                                  = types.flatMap { entry =>
        entry.tpe.allFields.map(field => (entry.source, entry.name, field.name) -> field)
      }.toMap
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
      val costs                                                         = mergeCosts(compiledCosts.flatMap(_.toOption))
      val transitiveSecurityDiagnostics                                 = missingTransitiveSecurityDiagnostics(
        ComposedGraph.securityDependencies(requirements, contexts, contextArguments),
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
              costs,
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
    dependenciesByField: List[ComposedGraph.SecurityDependency],
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

    dependenciesByField.flatMap { dependency =>
      val available = profile(dependency.sourceType, Some(dependency.fieldName))
      dependencies(dependency.selections, dependency.dependencyType).collect {
        case (coordinate, required) if !available.implies(required) =>
          s"[${dependency.source}] Field '${dependency.sourceType}.${dependency.fieldName}' does not specify sufficient Federation security requirements for ${dependency.directive} dependency '$coordinate'."
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

  private lazy val validatedLookups = sortedSubgraphs.map { subgraph =>
    subgraph -> subgraph.lookups.map(lookup => lookup -> validateLookup(subgraph, lookup))
  }

  private lazy val compiledLookups = validatedLookups.iterator.flatMap { case (subgraph, lookups) =>
    lookups.iterator.flatMap { case (lookup, result) =>
      result.value.map((subgraph.name -> lookup.typeName) -> _)
    }
  }.toMap

  private def lookupDiagnostics: List[String] =
    validatedLookups.flatMap { case (subgraph, lookups) =>
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
      sourceKind ::: duplicates ::: lookups.flatMap(_._2.diagnostics)
    }

  private def validateLookup(
    subgraph: PreparedSubgraph,
    lookup: Lookup
  ): ValidationResult[ComposedGraph.LookupOperation.GraphQLQuery] = {
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
    val argumentValidation     = sourceField.map(validateLookupArguments(prefix, rootName, lookup, _, keys))
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
        shapeErrors ::: argumentValidation.toList.flatMap(_.diagnostics)
    }
    val correlationDiagnostics = (lookup, targetType, sourceField) match {
      case (list: Lookup.ListLookup, Some(target), Some(field)) =>
        validateCorrelation(prefix, rootName, list, field, target, keys)
      case _                                                    => Nil
    }

    val diagnostics = targetDiagnostics ::: keyDiagnostics ::: fieldDiagnostics ::: correlationDiagnostics
    val result      = lookup match {
      case _: Lookup.Single         => ComposedGraph.LookupResult.Single
      case value: Lookup.ListLookup => ComposedGraph.LookupResult.ByKey(value.correlation)
    }
    val compiled    = argumentValidation
      .flatMap(_.value)
      .map(arguments => ComposedGraph.LookupOperation.GraphQLQuery(lookup.field, arguments, result))
    ValidationResult(diagnostics, if (diagnostics.isEmpty) compiled else None)
  }

  private def validateLookupArguments(
    prefix: String,
    rootName: String,
    lookup: Lookup,
    field: __Field,
    keys: Map[String, __Field]
  ): ValidationResult[Map[String, ComposedGraph.LookupArgument]] = {
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
      arguments.get(name).toList.map(argument => name -> validateArgument(prefix, name, mapping, argument._type, keys))
    }
    val batch       = lookup match {
      case _: Lookup.Single if lookup.arguments.exists(value => containsBatch(value._2))       =>
        List(s"$prefix Single lookup argument mappings cannot contain a batch mapping.")
      case _: Lookup.ListLookup if !lookup.arguments.exists(value => containsBatch(value._2))  =>
        List(s"$prefix List lookup argument mappings must contain a batch mapping.")
      case _: Lookup.ListLookup if lookup.arguments.exists(value => keyOutsideBatch(value._2)) =>
        List(s"$prefix List lookup key mappings must be nested inside a batch mapping.")
      case _                                                                                   => Nil
    }
    val mappedKeys  = lookup.arguments.iterator.flatMap(value => argumentKeys(value._2)).toSet
    val keyCoverage =
      if (mappedKeys == lookup.keyFields.toSet) Nil
      else List(s"$prefix Lookup argument mappings must use every declared key field.")

    val diagnostics =
      unknown ::: duplicates ::: missing ::: mappings.flatMap(_._2.diagnostics) ::: batch ::: keyCoverage
    val compiled    = mappings.foldLeft(Option(Map.empty[String, ComposedGraph.LookupArgument])) {
      case (values, (name, result)) =>
        for {
          map   <- values
          value <- result.value
        } yield map.updated(name, value)
    }
    ValidationResult(diagnostics, if (diagnostics.isEmpty) compiled else None)
  }

  private def validateArgument(
    prefix: String,
    path: String,
    mapping: Lookup.Argument,
    expected: __Type,
    keys: Map[String, __Field]
  ): ValidationResult[ComposedGraph.LookupArgument] = {
    val valueType = nullableType(expected)
    mapping match {
      case Lookup.Argument.Key(field)            =>
        val diagnostics = keys.get(field) match {
          case None           => List(s"$prefix Lookup argument '$path' references undeclared key field '$field'.")
          case Some(keyField) =>
            if (compatibleValueType(keyField._type, valueType)) Nil
            else
              List(
                s"$prefix Lookup argument '$path' is incompatible with key field '${keyField.name}'."
              )
        }
        ValidationResult(
          diagnostics,
          if (diagnostics.isEmpty) Some(ComposedGraph.LookupArgument.Key(field, valueType)) else None
        )
      case Lookup.Argument.ObjectMapping(fields) =>
        if (valueType.kind != __TypeKind.INPUT_OBJECT)
          ValidationResult(
            List(s"$prefix Lookup argument '$path' maps an object into a non-input-object value."),
            None
          )
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
          val mappings    = fields.flatMap { case (name, value) =>
            inputFields
              .get(name)
              .toList
              .map(input => name -> validateArgument(prefix, s"$path.$name", value, input._type, keys))
          }
          val diagnostics = duplicates ::: unknown ::: missing ::: mappings.flatMap(_._2.diagnostics)
          val compiled    = mappings.foldLeft(Option(List.empty[(String, ComposedGraph.LookupArgument)])) {
            case (values, (name, result)) =>
              for {
                list  <- values
                value <- result.value
              } yield (name -> value) :: list
          }
          ValidationResult(
            diagnostics,
            if (diagnostics.isEmpty) compiled.map(values => ComposedGraph.LookupArgument.ObjectMapping(values.reverse))
            else None
          )
        }
      case Lookup.Argument.Batch(value)          =>
        if (containsBatch(value))
          ValidationResult(List(s"$prefix Lookup argument '$path' cannot nest a batch mapping."), None)
        else if (!valueType.isList)
          ValidationResult(List(s"$prefix Lookup argument '$path' maps a batch into a non-list value."), None)
        else {
          val nested =
            validateArgument(prefix, path, value, valueType.ofType.map(nullableType).getOrElse(valueType), keys)
          ValidationResult(nested.diagnostics, nested.value.map(ComposedGraph.LookupArgument.Batch.apply))
        }
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

  private def keyOutsideBatch(argument: Lookup.Argument): Boolean =
    argument match {
      case _: Lookup.Argument.Key                => true
      case Lookup.Argument.ObjectMapping(fields) => fields.exists(value => keyOutsideBatch(value._2))
      case _: Lookup.Argument.Batch              => false
    }

  private def argumentKeys(argument: Lookup.Argument): List[String] =
    argument match {
      case Lookup.Argument.Key(field)            => field :: Nil
      case Lookup.Argument.ObjectMapping(fields) => fields.flatMap(value => argumentKeys(value._2))
      case Lookup.Argument.Batch(value)          => argumentKeys(value)
    }

  private def nullableType(tpe: __Type): __Type =
    if (tpe.kind == __TypeKind.NON_NULL) tpe.ofType.map(nullableType).getOrElse(tpe) else tpe

  private def fieldFlags(
    metadata: CompositionSubgraph,
    owner: String,
    field: __Field,
    inheritedShareable: Boolean = false,
    inheritedExternal: Boolean = false
  ): FieldFlags = {
    val subgraph   = metadata.subgraph
    val federation = subgraph.federation
    val contextual = field.allArgs.iterator.collect {
      case argument if federation && hasDirective(argument.directives, metadata.directives.fromContext) =>
        argument.name
    }.toSet
    val hiddenArgs = field.allArgs.iterator.collect {
      case argument if federation && hasDirective(argument.directives, metadata.directives.inaccessible) =>
        argument.name
    }.toSet ++ subgraph.mapping.hiddenArguments.collect {
      case (`owner`, fieldName, argument) if fieldName == field.name => argument
    } ++ contextual

    FieldFlags(
      inheritedShareable || federation && hasDirective(field.directives, metadata.directives.shareable),
      inheritedExternal || federation && hasDirective(field.directives, metadata.directives.external),
      subgraph.mapping.hiddenFields.contains(owner -> field.name) ||
        federation && hasDirective(field.directives, metadata.directives.inaccessible),
      if (federation) fieldOverride(field.directives, metadata.directives.overrideDirective) else None,
      hiddenArgs,
      contextual
    )
  }

  private def compatibleValueType(left: __Type, right: __Type): Boolean = {
    val a = nullableType(left)
    val b = nullableType(right)
    a.kind == b.kind && a.name == b.name
  }

  private def rootTypes: List[SubgraphType] =
    List(OperationType.Query, OperationType.Mutation, OperationType.Subscription).flatMap { operation =>
      prepared.flatMap { metadata =>
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
          subgraphType(metadata, operation.toString, tpe, Some(operation), rootType.name.getOrElse(""))
        }
      }
    }

  private def typeSystemDirectiveApplications(
    document: Document,
    composedName: String => String
  ): List[TypeSystemDirectiveApplication] = {
    def unsupported(
      coordinate: Coordinate,
      directives: List[Directive]
    ): TypeSystemDirectiveApplication =
      TypeSystemDirectiveApplication(coordinate, directives)

    def typeApplication(
      name: String,
      directives: List[Directive],
      location: __DirectiveLocation
    ): TypeSystemDirectiveApplication = {
      val typeName = composedName(name)
      TypeSystemDirectiveApplication(TypeCoordinate(typeName, location), directives)
    }

    def fieldApplications(typeName: String, fields: List[FieldDefinition]): List[TypeSystemDirectiveApplication] = {
      val parent = composedName(typeName)
      fields.flatMap { field =>
        TypeSystemDirectiveApplication(
          FieldCoordinate(parent, field.name),
          field.directives
        ) :: field.args.map(argument =>
          unsupported(ArgumentCoordinate(parent, field.name, argument.name), argument.directives)
        )
      }
    }

    def inputApplications(
      typeName: String,
      fields: List[InputValueDefinition]
    ): List[TypeSystemDirectiveApplication] = {
      val parent = composedName(typeName)
      fields.map(field => unsupported(InputFieldCoordinate(parent, field.name), field.directives))
    }

    def enumApplications(
      typeName: String,
      values: List[EnumValueDefinition]
    ): List[TypeSystemDirectiveApplication] = {
      val parent = composedName(typeName)
      values.map(value => unsupported(EnumValueCoordinate(parent, value.enumValue), value.directives))
    }

    val schemas            =
      document.schemaDefinition.toList.map(definition => unsupported(SchemaCoordinate, definition.directives))
    val scalarTypes        = document.typeDefinitions.collect { case value: ScalarTypeDefinition =>
      value.name -> value.directives
    }
    val unionTypes         = document.typeDefinitions.collect { case value: UnionTypeDefinition =>
      value.name -> value.directives
    }
    val enumTypes          = document.typeDefinitions.collect { case value: EnumTypeDefinition =>
      (value.name, value.directives, value.enumValuesDefinition)
    }
    val inputTypes         = document.typeDefinitions.collect { case value: InputObjectTypeDefinition =>
      (value.name, value.directives, value.fields)
    }
    val objectLikeTypes    = document.typeDefinitions.collect {
      case value: ObjectTypeDefinition    =>
        (value.name, value.directives, value.fields, __DirectiveLocation.OBJECT)
      case value: InterfaceTypeDefinition =>
        (value.name, value.directives, value.fields, __DirectiveLocation.INTERFACE)
    }
    val types              = scalarTypes.map { case (name, directives) =>
      typeApplication(name, directives, __DirectiveLocation.SCALAR)
    } ::: objectLikeTypes.flatMap { case (name, directives, fields, location) =>
      typeApplication(name, directives, location) :: fieldApplications(name, fields)
    } ::: unionTypes.map { case (name, directives) =>
      typeApplication(name, directives, __DirectiveLocation.UNION)
    } ::: enumTypes.flatMap { case (name, directives, values) =>
      typeApplication(name, directives, __DirectiveLocation.ENUM) :: enumApplications(name, values)
    } ::: inputTypes.flatMap { case (name, directives, fields) =>
      typeApplication(name, directives, __DirectiveLocation.INPUT_OBJECT) :: inputApplications(name, fields)
    }
    val directiveArguments = document.directiveDefinitions.flatMap { definition =>
      definition.args.map(argument =>
        unsupported(DirectiveArgumentCoordinate(definition.name, argument.name), argument.directives)
      )
    }

    schemas ::: types ::: directiveArguments
  }

  private def securityApplications(
    metadata: CompositionSubgraph
  ): Either[List[String], List[ComposedGraph.SecurityApplication]] = {
    val subgraph = metadata.subgraph
    val names    = metadata.directives
    val compiled = metadata.directiveApplications.flatMap { application =>
      application.securityCoordinate.toList.flatMap { case (typeName, fieldName) =>
        application.directives.flatMap(directive =>
          compileSecurityDirective(subgraph.name, application.coordinate.display, directive, names).map(
            _.map { value =>
              ComposedGraph.SecurityApplication(
                subgraph.name,
                typeName,
                fieldName,
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
    subgraph.rootNames.composed(typeName)

  private def unsupportedFederationDiagnostics(metadata: CompositionSubgraph): List[String] = {
    val subgraph = metadata.subgraph
    val names    = metadata.directives
    metadata.directiveApplications.flatMap { application =>
      application.directives.flatMap { directive =>
        val security          = securityDirectiveName(directive.name, names).collect {
          case name if application.securityCoordinate.isEmpty =>
            s"[${subgraph.name}] Federation $name is not supported at '${application.coordinate.display}'."
        }
        val unavailable       = names.unavailableSecurity
          .get(directive.name)
          .map(name =>
            s"[${subgraph.name}] Federation $name is not available in the linked feature version at '${application.coordinate.display}'."
          )
        val unavailableCost   = names.unavailableCost
          .get(directive.name)
          .map(name =>
            s"[${subgraph.name}] Federation $name requires Federation v2.9 or cost spec v0.1 at '${application.coordinate.display}'."
          )
        val context           = {
          val isContext = names.context.contains(directive.name)
          val isFrom    = names.fromContext.contains(directive.name)
          if ((isContext || isFrom) && !names.supportsContexts)
            Some(
              s"[${subgraph.name}] Federation ${if (isContext) "@context"
                else "@fromContext"} is not available in the linked feature version at '${application.coordinate.display}'."
            )
          else if (isContext && !application.supportsContext)
            Some(s"[${subgraph.name}] Federation @context is not supported at '${application.coordinate.display}'.")
          else if (isFrom && !application.supportsFromContext)
            Some(s"[${subgraph.name}] Federation @fromContext is not supported at '${application.coordinate.display}'.")
          else None
        }
        val overrideDirective =
          if (!names.overrideDirective.contains(directive.name)) None
          else if (directive.arguments.get("label").exists(_ != NullValue) && !names.supportsProgressiveOverride)
            Some(
              s"[${subgraph.name}] Federation @override(label:) is not available in the linked feature version at '${application.coordinate.display}'."
            )
          else
            directive.arguments.get("label") match {
              case Some(StringValue(label)) =>
                parseProgressiveOverrideLabel(label) match {
                  case Left(error)                               => Some(s"[${subgraph.name}] $error At '${application.coordinate.display}'.")
                  case Right(_) if !application.supportsOverride =>
                    Some(
                      s"[${subgraph.name}] Federation @override is not supported at '${application.coordinate.display}'."
                    )
                  case Right(_)                                  => None
                }
              case Some(NullValue) | None   =>
                if (!application.supportsOverride)
                  Some(
                    s"[${subgraph.name}] Federation @override is not supported at '${application.coordinate.display}'."
                  )
                else None
              case Some(_)                  =>
                Some(s"[${subgraph.name}] Invalid Federation @override label at '${application.coordinate.display}'.")
            }

        security.toList ::: unavailable.toList ::: unavailableCost.toList ::: context.toList ::: overrideDirective.toList
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
        }
      def nonQueryOperationRoot(typeName: String): Option[String] =
        subgraph.rootNames.composed(typeName) match {
          case "Mutation"     => Some("Mutation")
          case "Subscription" => Some("Subscription")
          case _              => None
        }
      val contextDirectives                                       = typeEntries.flatMap { case (typeName, directives) =>
        directives.collect { case directive if names.context.contains(directive.name) => typeName -> directive }
      }
      val declarations                                            = contextDirectives.map { case (typeName, directive) =>
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
      val declarationErrors                                       = declarations.collect { case Left(error) => error }
      val declared                                                = declarations.collect { case Right(value) => value }
        .groupMap(_._1)(_._2)
        .map { case (coordinate, values) => coordinate -> values.toSet }
      val byName                                                  = contextDirectives.flatMap { case (typeName, directive) =>
        directive.arguments.get("name").collect { case StringValue(name) =>
          (subgraph.name -> ComposedGraph.ContextName(name)) -> typeName
        }
      }.groupMap(_._1)(_._2)
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
                    contextTypes.foldLeft[Either[String, List[__Type]]](Right(Nil)) { (result, contextType) =>
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
                      } yield parent :: parents
                    }
                  _                 <- validateContextTypeConditions(
                                         subgraph.rootType,
                                         contextParents,
                                         selections
                                       ).left.map(error => s"$prefix: $error")
                  _                 <- contextParents.foldLeft[Either[String, Unit]](Right(())) { case (result, parent) =>
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
              .groupMap(_._1)(_._2)
              .toList
          )
        )
    }
  }

  private def federationCosts(
    metadata: CompositionSubgraph
  ): Either[List[String], ComposedGraph.CostMetadata] = {
    val subgraph = metadata.subgraph
    val names    = metadata.directives

    def isList(tpe: __Type): Boolean = nullableType(tpe).kind == __TypeKind.LIST

    def resolvedType(tpe: __Type): __Type = {
      val inner = tpe.innerType
      inner.name.flatMap(subgraph.rootType.types.get).getOrElse(inner)
    }

    def strings(arguments: Map[String, InputValue], name: String): Either[String, List[String]] =
      arguments.get(name) match {
        case None                               => Right(Nil)
        case Some(StringValue(value))           => Right(value :: Nil)
        case Some(InputValue.ListValue(values)) =>
          val result = values.collect { case StringValue(value) => value }
          Either.cond(result.size == values.size, result, s"the '$name' argument must be a list of strings.")
        case _                                  => Left(s"the '$name' argument must be a list of strings.")
      }

    def selectionPaths(
      selections: List[Selection],
      prefix: Vector[String] = Vector.empty
    ): Option[List[Vector[String]]] =
      selections.foldLeft(Option(List.empty[Vector[String]])) { case (result, selection) =>
        for {
          paths <- result
          next  <- selection match {
                     case Selection.Field(None, name, arguments, directives, children, _)
                         if arguments.isEmpty && directives.isEmpty =>
                       if (children.isEmpty) Some(List(prefix :+ name))
                       else selectionPaths(children, prefix :+ name)
                     case _ => None
                   }
        } yield paths ::: next
      }

    def hasNoSiblingLeaves(selections: List[Selection]): Boolean =
      selections.count {
        case Selection.Field(_, _, _, _, children, _) => children.isEmpty
        case _                                        => false
      } <= 1 && selections.forall {
        case Selection.Field(_, _, _, _, children, _) => children.isEmpty || hasNoSiblingLeaves(children)
        case _                                        => true
      }

    def inputPathType(field: __Field, path: Vector[String]): Option[__Type] =
      path.headOption.flatMap(name => field.allArgs.find(_.name == name)).flatMap { argument =>
        path.tail.foldLeft(Option(argument._type)) { (current, name) =>
          current.flatMap { tpe =>
            val value = nullableType(tpe)
            if (value.kind == __TypeKind.LIST) None
            else resolvedType(value).allInputFields.find(_.name == name).map(_._type)
          }
        }
      }

    def sizedPathType(field: __Field, path: Vector[String]): Option[__Type] =
      path.foldLeft(Option(field._type)) { (current, name) =>
        current.flatMap { tpe =>
          Option(resolvedType(tpe).getFieldOrNull(name)).map(_._type)
        }
      }

    def listSizeApplication(
      directive: Directive,
      typeName: String,
      field: __Field
    ): Either[String, ((String, String, String), ComposedGraph.ListSize)] = {
      val coordinate = s"$typeName.${field.name}"
      val prefix     = s"[${subgraph.name}] Invalid Federation @listSize application at '$coordinate'"
      val arguments  = directive.arguments
      val allowed    = Set("assumedSize", "slicingArguments", "sizedFields", "requireOneSlicingArgument")

      if (!arguments.keySet.subsetOf(allowed)) Left(s"$prefix: unsupported argument.")
      else {
        val assumed = arguments.get("assumedSize") match {
          case None                                         => Right(None)
          case Some(value: IntValue) if value.toBigInt >= 0 => Right(Some(value.toBigInt.longValue))
          case Some(_: IntValue)                            => Left("the 'assumedSize' argument must not be negative.")
          case _                                            => Left("the 'assumedSize' argument must be an integer.")
        }
        val require = arguments.get("requireOneSlicingArgument") match {
          case None                      => Right(true)
          case Some(BooleanValue(value)) => Right(value)
          case _                         => Left("the 'requireOneSlicingArgument' argument must be a boolean.")
        }

        for {
          assumedSize  <- assumed.left.map(error => s"$prefix: $error")
          slicing      <- strings(arguments, "slicingArguments").left.map(error => s"$prefix: $error")
          sized        <- strings(arguments, "sizedFields").left.map(error => s"$prefix: $error")
          requireOne   <- require.left.map(error => s"$prefix: $error")
          slicingPaths <- slicing.foldLeft[Either[String, List[Vector[String]]]](Right(Nil)) { (result, value) =>
                            val path = value.split("\\.", -1).toVector
                            for {
                              paths <- result
                              _     <- Either.cond(
                                         path.forall(_.nonEmpty),
                                         (),
                                         s"$prefix: slicing argument '$value' is not a valid path."
                                       )
                            } yield paths :+ path
                          }
          _            <- slicingPaths.foldLeft[Either[String, Unit]](Right(())) { (result, path) =>
                            result.flatMap { _ =>
                              val valid = inputPathType(field, path).exists { tpe =>
                                val value = nullableType(tpe)
                                value.kind == __TypeKind.LIST ||
                                (value.kind == __TypeKind.SCALAR && value.name.contains("Int"))
                              }
                              Either.cond(
                                valid,
                                (),
                                s"$prefix: slicing argument '${path.mkString(".")}' must resolve to an Int or list argument."
                              )
                            }
                          }
          sizedPaths   <- sized.foldLeft[Either[String, List[Vector[String]]]](Right(Nil)) { (result, value) =>
                            for {
                              paths      <- result
                              parsed     <- parseFieldSet(value)
                                              .toRight(s"$prefix: sized field '$value' is not a valid field path.")
                              selections <- selectionPaths(parsed)
                                              .toRight(s"$prefix: sized field '$value' is not a valid field path.")
                              _          <- Either.cond(
                                              hasNoSiblingLeaves(parsed),
                                              (),
                                              s"$prefix: sized field '$value' must not select sibling leaf fields."
                                            )
                            } yield paths ::: selections
                          }
          _            <- Either.cond(
                            isList(field._type) || sizedPaths.nonEmpty,
                            (),
                            s"$prefix: the field must return a list or define 'sizedFields'."
                          )
          _            <- sizedPaths.foldLeft[Either[String, Unit]](Right(())) { (result, path) =>
                            result.flatMap(_ =>
                              Either.cond(
                                sizedPathType(field, path).exists(isList),
                                (),
                                s"$prefix: sized field '${path.mkString(".")}' must exist and return a list."
                              )
                            )
                          }
        } yield (subgraph.name, typeName, field.name) -> ComposedGraph.ListSize(
          assumedSize,
          slicingPaths.map { path =>
            val default = path.headOption.flatMap(name => field.allArgs.find(_.name == name)).flatMap { argument =>
              argument.parsedDefaultValue
            }
            ComposedGraph.SlicingArgument(path, default, inputPathType(field, path).exists(isList))
          },
          sizedPaths,
          requireOne
        )
      }
    }

    def applications(
      directives: Option[List[Directive]],
      coordinate: String,
      entry: Long => CostEntry,
      locationError: Option[String] = None
    ): List[Either[String, CostEntry]] =
      directives.getOrElse(Nil).filter(directive => names.cost.contains(directive.name)).map { directive =>
        val prefix = s"[${subgraph.name}] Invalid Federation @cost application at '$coordinate'"
        if (locationError.nonEmpty)
          Left(s"$prefix: ${locationError.get}")
        else
          directive.arguments match {
            case arguments if arguments.keySet == Set("weight") =>
              arguments("weight") match {
                case value: IntValue => Right(entry(value.toBigInt.longValue))
                case _               => Left(s"$prefix: the 'weight' argument must be an integer.")
              }
            case _                                              =>
              Left(s"$prefix: exactly one 'weight' argument is required.")
          }
      }

    val sortedTypes        = subgraph.rootType.types.toList.sortBy(_._1)
    val applicationsByType = sortedTypes.map { case (sourceName, tpe) =>
      val typeName         = composedTypeName(subgraph, sourceName)
      val supportedType    = tpe.kind == __TypeKind.OBJECT || tpe.kind == __TypeKind.SCALAR || tpe.kind == __TypeKind.ENUM
      val typeEntries      = applications(
        tpe.directives,
        typeName,
        weight => TypeCost(typeName, weight),
        if (supportedType) None else Some("@cost is not supported at this type location.")
      )
      val fields           = tpe.allFields.flatMap { field =>
        applications(
          field.directives,
          s"$typeName.${field.name}",
          weight => FieldCost(typeName, field.name, weight),
          if (tpe.kind == __TypeKind.INTERFACE) Some("@cost cannot be applied to an interface field.") else None
        ) ::: field.allArgs.flatMap(argument =>
          applications(
            argument.directives,
            s"$typeName.${field.name}(${argument.name}:)",
            weight => ArgumentCost(typeName, field.name, argument.name, weight)
          )
        )
      }
      val inputFields      = tpe.allInputFields.flatMap(field =>
        applications(
          field.directives,
          s"$typeName.${field.name}",
          weight => InputFieldCost(typeName, field.name, weight)
        )
      )
      val listSizes        = tpe.allFields.flatMap { field =>
        field.directives.getOrElse(Nil).filter(directive => names.listSize.contains(directive.name)).map { directive =>
          listSizeApplication(directive, typeName, field)
        }
      }
      val invalidListSizes =
        tpe.directives
          .getOrElse(Nil)
          .filter(directive => names.listSize.contains(directive.name))
          .map(_ => typeName) :::
          tpe.allFields.flatMap { field =>
            field.allArgs.flatMap(argument =>
              argument.directives
                .getOrElse(Nil)
                .filter(directive => names.listSize.contains(directive.name))
                .map(_ => s"$typeName.${field.name}(${argument.name}:)")
            )
          } :::
          tpe.allInputFields.flatMap { field =>
            field.directives
              .getOrElse(Nil)
              .filter(directive => names.listSize.contains(directive.name))
              .map(_ => s"$typeName.${field.name}")
          }
      (typeEntries ::: fields ::: inputFields) ->
        (listSizes ::: invalidListSizes.map(coordinate =>
          Left(
            s"[${subgraph.name}] Invalid Federation @listSize application at '$coordinate': @listSize is only supported on fields."
          )
        ))
    }
    val entries            = applicationsByType.flatMap(_._1)
    val listSizes          = applicationsByType.flatMap(_._2)
    val errors             = entries.collect { case Left(error) => error } ::: listSizes.collect { case Left(error) =>
      error
    }

    if (errors.nonEmpty) Left(errors)
    else {
      val values = entries.collect { case Right(value) => value }
      Right(
        ComposedGraph.CostMetadata(
          maximum(values.collect { case TypeCost(name, weight) => name -> weight }),
          maximum(values.collect { case FieldCost(parent, name, weight) => (parent -> name) -> weight }),
          maximum(values.collect { case ArgumentCost(parent, field, name, weight) => (parent, field, name) -> weight }),
          maximum(values.collect { case InputFieldCost(parent, name, weight) => (parent -> name) -> weight }),
          listSizes.collect { case Right(value) => value }.toMap
        )
      )
    }
  }

  private def mergeCosts(values: List[ComposedGraph.CostMetadata]): ComposedGraph.CostMetadata =
    ComposedGraph.CostMetadata(
      maximum(values.flatMap(_.types)),
      maximum(values.flatMap(_.fields)),
      maximum(values.flatMap(_.arguments)),
      maximum(values.flatMap(_.inputFields)),
      values.flatMap(_.listSizes).toMap
    )

  private def maximum[K](values: Iterable[(K, Long)]): Map[K, Long] =
    values.groupBy(_._1).map { case (key, entries) => key -> entries.iterator.map(_._2).max }

  private def validateContextReceiver(
    source: String,
    typeName: String,
    fieldName: String,
    prefix: String
  ): Either[String, Unit] = {
    val receiver    = types.find(entry => entry.source == source && entry.name == typeName)
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
            if (field.name == "__typename" && field.selectionSet.isEmpty) Right(Types.string :: Nil)
            else
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
      val rootNames = subgraph.rootNames.sourceNames

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

  private def subgraphType(
    metadata: CompositionSubgraph,
    name: String,
    tpe: __Type,
    operation: Option[OperationType] = None,
    owner: String = ""
  ): SubgraphType = {
    val subgraph        = metadata.subgraph
    val directives      = tpe.directives.getOrElse(Nil)
    val fields          = tpe.allFields
    val names           = metadata.directives
    val sourceName      = if (owner.isEmpty) name else owner
    val interfaceObject = operation.isEmpty && subgraph.federation && hasDirective(directives, names.interfaceObject)
    val composedType    =
      if (operation.nonEmpty)
        tpe.copy(
          description = None,
          directives = None,
          interfaces = () => None,
          possibleTypes = None
        )
      else if (interfaceObject && tpe.kind == __TypeKind.OBJECT) tpe.copy(kind = __TypeKind.INTERFACE)
      else tpe
    val entity          =
      if (operation.nonEmpty) None
      else if (subgraph.federation) {
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
              compiledLookups
                .get(subgraph.name -> name)
                .toList
                .map(ComposedGraph.EntityLookup(key, _))
            )
          }
    val typeExternal    = operation.isEmpty && subgraph.federation && hasDirective(directives, names.external)
    val typeShareable   = subgraph.federation && hasDirective(directives, names.shareable)
    val flags           = fields
      .map(field => field.name -> fieldFlags(metadata, sourceName, field, typeShareable, typeExternal))
      .toMap
    val fed1Owned       =
      if (operation.nonEmpty) Set.empty[String]
      else metadata.federation1ExtensionKeyCoordinates.collect { case (`name`, field) => field }
    val external        = flags.collect { case (field, value) if value.external => field }.toSet -- fed1Owned
    val keyFields       = entity.fold(Set.empty[String])(_.keyFields) ++
      (if (operation.nonEmpty) Set.empty
       else metadata.keyCoordinates.collect { case (`name`, field) => field })
    val shareable       = flags.collect { case (field, value) if value.shareable => field }.toSet ++ keyFields
    val inaccessible    = operation.isEmpty && (subgraph.mapping.hiddenTypes.contains(name) ||
      subgraph.federation && hasDirective(directives, names.inaccessible))
    val hiddenFields    = flags.collect { case (field, value) if value.inaccessible => field }.toSet
    val contextualArgs  = flags.iterator.flatMap { case (field, value) =>
      value.contextualArguments.iterator.map(field -> _)
    }.toSet
    val hiddenArgs      = flags.iterator.flatMap { case (field, value) =>
      value.inaccessibleArguments.iterator.map(field -> _)
    }.toSet
    val hiddenInputs    = subgraph.mapping.hiddenInputFields.collect { case (`name`, field) => field } ++
      tpe.allInputFields.iterator.collect {
        case field if hasDirective(field.directives, names.inaccessible) => field.name
      }
    val hiddenEnums     =
      tpe.allEnumValues.iterator.collect {
        case value if hasDirective(value.directives, names.inaccessible) => value.name
      }.toSet
    val overrides       = flags.flatMap { case (field, value) => value.overrideDirective.map(field -> _) }
    SubgraphType(
      subgraph.name,
      name,
      composedType,
      operation,
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
      if (!subgraph.federation) SubgraphMode.Ordinary
      else if (metadata.federation2) SubgraphMode.Federation2
      else SubgraphMode.Federation1,
      metadata.hiddenDirectives
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
        val overriddenRoute  = List(
          ComposedGraph.FieldRoute(
            directive.from,
            Some(ComposedGraph.OverrideCondition(value.label, value.percentage, active = false))
          )
        )
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

    types.groupBy(_.name).toList.flatMap { case (typeName, entries) =>
      entries.flatMap(entry =>
        entry.overrideFields.toList.flatMap { case (field, directive) =>
          val prefix =
            entry.operation.fold(s"[type $typeName.$field]")(operation => s"[${operation.toString.toLowerCase}.$field]")
          diagnostic(
            prefix,
            entry.source,
            directive,
            entries.exists(candidate => candidate.source == directive.from && candidate.ownedFields.contains(field))
          )
        }
      )
    }
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

  private def visibilityDiagnostics: List[String] = {
    val inaccessibleTypes  = types.filter(_.inaccessible).map(_.name).toSet
    val inaccessibleFields = types.iterator.flatMap(entry => entry.inaccessibleFields.map(entry.name -> _)).toSet
    val inaccessibleInputs =
      types.iterator.flatMap(entry => entry.inaccessibleInputFields.map(entry.name -> _)).toSet
    val hiddenArguments    = types.iterator
      .flatMap(entry =>
        entry.inaccessibleArguments.map { case (field, argument) =>
          (entry.name, field, argument)
        }
      )
      .toSet
    def argumentDiagnostics(
      source: String,
      coordinate: String,
      arguments: List[__InputValue],
      hidden: String => Boolean
    ): List[String] =
      arguments.collect {
        case argument if !hidden(argument.name) && argument._type.innerType.name.exists(inaccessibleTypes.contains) =>
          s"[$source] Argument '$coordinate.${argument.name}' must be @inaccessible because its input type is inaccessible."
        case argument if hidden(argument.name) && !argument._type.isNullable && argument.defaultValue.isEmpty       =>
          s"[$source] Required @inaccessible argument '$coordinate.${argument.name}' must define a default value."
      }
    def fieldDiagnostics(
      source: String,
      coordinate: String,
      field: __Field,
      inaccessible: Boolean,
      inaccessibleArguments: Set[String]
    ): (List[String], List[String]) =
      if (inaccessible) (Nil, Nil)
      else {
        val output = field._type.innerType.name.toList.collect {
          case name if inaccessibleTypes.contains(name) =>
            s"[$source] Field '$coordinate' must be @inaccessible because its return type is inaccessible."
        }
        output -> argumentDiagnostics(source, coordinate, field.allArgs, inaccessibleArguments)
      }
    val accessibleTypes    = types.filterNot(entry => inaccessibleTypes.contains(entry.name))
    val typeFieldErrors    = accessibleTypes.flatMap { entry =>
      entry.tpe.allFields.map { field =>
        val entryName = entry.name
        val fieldName = field.name
        fieldDiagnostics(
          entry.source,
          entry.operation.fold(s"$entryName.$fieldName")(_ => fieldName),
          field,
          inaccessibleFields.contains(entryName -> fieldName),
          hiddenArguments.collect { case (`entryName`, `fieldName`, argument) => argument }
        )
      }
    }
    val inputFieldErrors   = accessibleTypes.flatMap { entry =>
      entry.tpe.allInputFields.collect {
        case field
            if !inaccessibleInputs.contains(entry.name -> field.name) &&
              field._type.innerType.name.exists(inaccessibleTypes.contains) =>
          s"[${entry.source}] Input field '${entry.name}.${field.name}' must be @inaccessible because its input type is inaccessible."
        case field
            if inaccessibleInputs.contains(entry.name -> field.name) &&
              !field._type.isNullable && field.defaultValue.isEmpty =>
          s"[${entry.source}] Required @inaccessible input field '${entry.name}.${field.name}' must define a default value."
      }
    }
    typeFieldErrors.flatMap(_._1) :::
      typeFieldErrors.flatMap(_._2) :::
      inputFieldErrors
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

}

private[gateway] object SchemaComposer {
  import DirectiveComposition._

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

  private final case class FieldFlags(
    shareable: Boolean,
    external: Boolean,
    inaccessible: Boolean,
    overrideDirective: Option[FieldOverride],
    inaccessibleArguments: Set[String],
    contextualArguments: Set[String]
  )

  private final case class ValidationResult[A](diagnostics: List[String], value: Option[A])

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

  private sealed trait CostEntry
  private final case class TypeCost(name: String, weight: Long)                                    extends CostEntry
  private final case class FieldCost(parent: String, name: String, weight: Long)                   extends CostEntry
  private final case class ArgumentCost(parent: String, field: String, name: String, weight: Long) extends CostEntry
  private final case class InputFieldCost(parent: String, name: String, weight: Long)              extends CostEntry

  private final case class TypeSystemDirectiveApplication(
    coordinate: Coordinate,
    directives: List[Directive]
  ) {
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

  private def hasDirective(directives: List[Directive], names: Set[String]): Boolean =
    directives.exists(directive => names.contains(directive.name))

  private def hasDirective(directives: Option[List[Directive]], names: Set[String]): Boolean =
    directives.exists(_.exists(directive => names.contains(directive.name)))

  private def objectLikeEntries(document: Document): List[(String, List[Directive], List[FieldDefinition])] =
    document.typeDefinitions.collect {
      case definition: ObjectTypeDefinition    => (definition.name, definition.directives, definition.fields)
      case definition: InterfaceTypeDefinition => (definition.name, definition.directives, definition.fields)
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
  )

  private def federationDirectiveNames(document: Document): FederationDirectiveNames = {
    val links                                                                    = linkedFeatures(document)
    val federation                                                               = links.filter(_.identity == FederationIdentity)
    val security                                                                 = links.filter(feature => SecurityFeatureIdentities.contains(feature.identity))
    val costFeature                                                              = links.filter(_.identity == CostIdentity)
    val relevant                                                                 = federation ::: security ::: costFeature
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
    def partitionNames(
      name: String,
      features: List[LinkedFeature]
    )(available: LinkedFeature => Boolean): (Set[String], Set[String]) = {
      val (supported, unsupported) = features.partition(available)
      supported.iterator.flatMap(_.directiveNames(name)).toSet ->
        unsupported.iterator.flatMap(_.directiveNames(name)).toSet
    }
    def securityNames(
      name: String,
      federationMajor: Int,
      federationMinor: Int,
      identity: String
    ): (Set[String], Set[String]) =
      partitionNames(name, federation ::: security.filter(_.identity == identity))(feature =>
        if (feature.identity == FederationIdentity) feature.version.atLeast(federationMajor, federationMinor)
        else feature.version == FeatureVersion(0, 1)
      )
    def costNames(name: String): (Set[String], Set[String])                      =
      partitionNames(name, federation ::: costFeature)(feature =>
        if (feature.identity == FederationIdentity) feature.version.atLeast(2, 9)
        else feature.version == FeatureVersion(0, 1)
      )

    val keyNames                                    = federationNames("key", federation1 = true)
    val externalNames                               = federationNames("external", federation1 = true)
    val extendsNames                                = federationNames("extends", federation1 = true)
    val shareableNames                              = federationNames("shareable")
    val inaccessibleNames                           = federationNames("inaccessible")
    val overrideNames                               = federationNames("override")
    val requiresNames                               = federationNames("requires", federation1 = true)
    val providesNames                               = federationNames("provides", federation1 = true)
    val interfaceObjects                            = federationNames("interfaceObject")
    val tagNames                                    = federationNames("tag")
    val composeNames                                = federationNames("composeDirective")
    val (authenticated, unavailableAuthenticated)   = securityNames("authenticated", 2, 5, AuthenticatedIdentity)
    val (requiresScopes, unavailableRequiresScopes) = securityNames("requiresScopes", 2, 5, RequiresScopesIdentity)
    val (policy, unavailablePolicy)                 = securityNames("policy", 2, 6, PolicyIdentity)
    val unavailableSecurity                         =
      unavailableAuthenticated.map(_ -> "@authenticated").toMap ++
        unavailableRequiresScopes.map(_ -> "@requiresScopes").toMap ++
        unavailablePolicy.map(_ -> "@policy").toMap
    val (cost, unavailableCostNames)                = costNames("cost")
    val (listSize, unavailableListSizeNames)        = costNames("listSize")
    val unavailableCost                             = unavailableCostNames.map(_ -> "@cost").toMap ++
      unavailableListSizeNames.map(_ -> "@listSize").toMap
    val context                                     = federationNames("context")
    val fromContext                                 = federationNames("fromContext")
    val supportsContexts                            = federation.exists(_.version.atLeast(2, 8))
    val progressiveOverride                         = federation.exists(_.version.atLeast(2, 7))
    val hiddenDirectives                            = Set("link") ++ keyNames ++ externalNames ++ extendsNames ++ shareableNames ++
      inaccessibleNames ++ overrideNames ++ requiresNames ++ providesNames ++ interfaceObjects ++
      tagNames ++ composeNames ++ authenticated ++ requiresScopes ++ policy ++ unavailableSecurity.keySet ++
      unavailableCost.keySet ++ cost ++ listSize ++ context ++ fromContext ++
      document.directiveDefinitions.iterator.map(_.name).filter(name => namespacePrefix.exists(name.startsWith))

    FederationDirectiveNames(
      links,
      federation.nonEmpty,
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
      unavailableCost,
      cost,
      listSize,
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
  private val CostIdentity                               = "https://specs.apollo.dev/cost"
  private val SecurityFeatureIdentities                  = Set(AuthenticatedIdentity, RequiresScopesIdentity, PolicyIdentity)
  private def isFederation2(document: Document): Boolean =
    (linkedFeatures(document) ::: linkedFeatures(document.typeExtensions.collect { case extension: SchemaExtension =>
      extension
    }.flatMap(_.directives))).exists(_.identity == FederationIdentity)

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
      val extendedDefinitions = subgraph.document.typeDefinitions.collect {
        case definition: ObjectTypeDefinition if subgraph.federation1ExtensionTypes(definition.name)            =>
          definition.name -> definition.directives
        case definition: InterfaceTypeDefinition if subgraph.federation1ExtensionTypes(definition.name)         =>
          definition.name -> definition.directives
        case definition: ObjectTypeDefinition if hasDirective(definition.directives, names.extendsDirective)    =>
          definition.name -> definition.directives
        case definition: InterfaceTypeDefinition if hasDirective(definition.directives, names.extendsDirective) =>
          definition.name -> definition.directives
      }

      extendedDefinitions.iterator.flatMap { case (typeName, directives) =>
        directives.iterator
          .flatMap(keyDirective(_, names))
          .flatMap(_.fields.map(field => typeName -> field.name))
      }.toSet
    }

  private[gateway] def federation1ExtensionTypes(document: Document): Set[String] =
    document.typeExtensions.iterator.collect {
      case extension: ObjectTypeExtension    => extension.name
      case extension: InterfaceTypeExtension => extension.name
    }.toSet

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
