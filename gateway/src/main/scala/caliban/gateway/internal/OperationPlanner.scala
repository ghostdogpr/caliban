package caliban.gateway.internal

import caliban.InputValue
import caliban.Value.NullValue
import caliban.execution.{ isIntrospectionField, isMetaField, ExecutionRequest, Field, Fragment }
import caliban.gateway.internal.OperationPlanner._
import caliban.introspection.adt.{ __Field, __Type, __TypeKind }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.{ Definition, Directive, Document, OperationType, Selection }
import caliban.rendering.DocumentRenderer
import caliban.schema.Types
import zio.Duration

import scala.collection.immutable.ListMap
import scala.collection.mutable

private[gateway] final class OperationPlanner(
  graph: ComposedGraph,
  sourceCount: Int,
  limits: OperationPlanner.Limits
) {

  def plan(document: Document, execution: ExecutionRequest): Either[PlanningFailure, OperationPlan] = {
    implicit val search: PlanningSearch = new PlanningSearch(limits)
    val rootName                        = operationRootName(execution.operationType)
    val fields                          = execution.field.collectFields(rootName)
    val localFields                     = fields.filter(isMetaField)

    for {
      _          <- search.check
      candidate  <- planRoots(fields.filterNot(isMetaField), execution.operationType)
      runtime     = runtimeTypeSelections(candidate.roots, candidate.entities)
      passthrough = passthroughSource(candidate.planned.routes, candidate.entities, runtime, localFields)
      _          <- Either.cond(
                      passthrough.nonEmpty || !document.hasDirective(execution.operationName)(isCustomDirective),
                      (),
                      PlanningFailure("Custom executable directives are not supported by this gateway.")
                    )
      _          <- search.check
    } yield OperationPlan(
      execution.operationType,
      rootName,
      fields,
      localFields,
      candidate.planned.routes,
      candidate.entities,
      runtime,
      passthrough
    )
  }

  private def planRoots(
    fields: List[Field],
    operationType: OperationType
  )(implicit search: PlanningSearch): Either[PlanningFailure, PlannedCandidate] =
    fields
      .foldLeft[Either[PlanningFailure, List[List[PlannedRoot]]]](Right(List(Nil))) { case (result, field) =>
        for {
          accumulated <- result
          options     <- planRootOptions(field, operationType)
          combined    <-
            if (accumulated == List(Nil)) Right(options)
            else search.combine(accumulated, options)(_ ::: _)
        } yield combined
      }
      .flatMap(options =>
        search
          .evaluate(options) { roots =>
            val planned  = rootRoutes(roots, operationType)
            val entities = enrichmentDependencies(entityRoutes(planned.assignments, planned.routes.size))
            validateDependencies(entities).map(_ => PlannedCandidate(roots, planned, entities))
          }
          .flatMap {
            case candidate :: Nil  => Right(candidate)
            case candidate :: rest => Right(best(candidate, rest)(routeCost))
            case Nil               => Left(PlanningFailure("No complete route candidate was found."))
          }
      )

  private def planRootOptions(
    field: Field,
    operationType: OperationType
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[List[PlannedRoot]]] = {
    val sources = graph.sources(operationType, field.name)
    for {
      _       <- Either.cond(sources.nonEmpty, (), PlanningFailure(s"No subgraph owns root field '${field.name}'."))
      options <-
        if (sources.size == 1)
          planRootsAtSource(field, field, sources.head, sources, true).flatMap {
            case roots if roots.nonEmpty => Right(roots.map(List(_)))
            case _                       => Left(PlanningFailure(s"No subgraph can execute root field '${field.name}'."))
          }
        else {
          val strategies =
            if (operationType == OperationType.Mutation) sources.map(RootStrategy.Single.apply)
            else sources.map(RootStrategy.Single.apply) ::: RootStrategy.Split :: Nil
          search
            .evaluate(strategies) {
              case RootStrategy.Single(source) =>
                planRootsAtSource(field, field, source, sources, true).flatMap {
                  case roots if roots.nonEmpty => Right(roots.map(List(_)))
                  case _                       =>
                    Left(PlanningFailure(s"Source '$source' has no executable work for '${field.name}'."))
                }
              case RootStrategy.Split          =>
                sources
                  .foldLeft[Either[PlanningFailure, List[List[PlannedRoot]]]](Right(List(Nil))) {
                    case (result, source) =>
                      result.flatMap { roots =>
                        val selected = rootFieldForSource(field, source, sources)
                        search
                          .evaluate(roots) { current =>
                            planRootsAtSource(field, selected, source, sources, false)
                              .map(planned => if (planned.isEmpty) List(current) else planned.map(current :+ _))
                          }
                          .map(_.flatten)
                      }
                  }
                  .flatMap { roots =>
                    val complete = roots.filter(_.nonEmpty)
                    Either.cond(
                      complete.nonEmpty,
                      complete,
                      PlanningFailure(s"No subgraph can execute '${field.name}'.")
                    )
                  }
            }
            .map(_.flatten)
        }
    } yield options
  }

  private def best[A](candidate: A, alternatives: List[A])(cost: A => StructuralCost): A =
    alternatives
      .foldLeft(candidate -> cost(candidate)) { case ((selected, selectedCost), alternative) =>
        val alternativeCost = cost(alternative)
        if (alternativeCost.betterThan(selectedCost)) alternative -> alternativeCost else selected -> selectedCost
      }
      ._1

  private def rootRoutes(roots: List[PlannedRoot], operationType: OperationType): PlannedRoutes =
    if (operationType == OperationType.Mutation) {
      val routes = roots.zipWithIndex.map { case (root, index) =>
        RootRoute(RouteId(index), root.source, root.client :: Nil, root.downstream :: Nil)
      }
      PlannedRoutes(routes, roots.zip(routes).map { case (root, route) => root -> route.id })
    } else {
      val grouped  = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[PlannedRoot]]
      roots.foreach(root => grouped.getOrElseUpdate(root.source, mutable.ListBuffer.empty) += root)
      val routes   = grouped.iterator.zipWithIndex.map { case ((source, planned), index) =>
        val selected = planned.toList
        RootRoute(RouteId(index), source, selected.map(_.client), selected.map(_.downstream))
      }.toList
      val bySource = routes.iterator.map(route => route.source -> route.id).toMap
      PlannedRoutes(routes, roots.flatMap(root => bySource.get(root.source).map(root -> _)))
    }

  private def entityRoutes(assignments: List[(PlannedRoot, RouteId)], firstId: Int): List[EntityRoute] =
    flattenEntityGroups(assignments.map { case (planned, root) => planned.entities -> root }, firstId)

  private def flattenEntityGroups(
    groups: List[(List[PlannedEntity], RouteId)],
    firstId: Int
  ): List[EntityRoute] = {
    var nextRouteId = firstId

    def flatten(values: List[PlannedEntity], root: RouteId, dependencies: Set[RouteId]): List[EntityRoute] =
      values.flatMap { entity =>
        val id       = RouteId(nextRouteId)
        nextRouteId += 1
        val current  = EntityRoute(
          id,
          root,
          entity.source,
          dependencies,
          entity.dependencySource,
          entity.mergePath,
          entity.entityType,
          entity.keys,
          entity.requirements,
          entity.typename,
          entity.lookup,
          entity.fields,
          entity.requiresKeyEnrichment
        )
        val children = flatten(entity.entities, root, Set(id))
        current :: children
      }

    groups.flatMap { case (values, root) => flatten(values, root, Set(root)) }
  }

  private def enrichmentDependencies(routes: List[EntityRoute]): List[EntityRoute] = {
    val byId = routes.iterator.map(route => route.id -> route).toMap

    def dependsOn(route: EntityRoute, dependency: RouteId, seen: Set[RouteId]): Boolean =
      route.dependencies.contains(dependency) || route.dependencies.exists { id =>
        !seen.contains(id) && byId.get(id).exists(dependsOn(_, dependency, seen + id))
      }
    def selectionPaths(selection: RequiredSelection): List[Vector[String]]              =
      if (selection.children.isEmpty) Vector(selection.responseName) :: Nil
      else selection.children.flatMap(selectionPaths).map(Vector(selection.responseName) ++ _)
    def fieldPaths(field: Field): List[Vector[String]]                                  =
      if (field.fields.isEmpty) Vector(field.aliasedName) :: Nil
      else field.fields.flatMap(fieldPaths).map(Vector(field.aliasedName) ++ _)

    routes.map { route =>
      if (!route.requiresKeyEnrichment) route
      else {
        val required     = (route.keys ::: route.requirements).flatMap(selectionPaths).map(route.mergePath ++ _).toSet
        val dependencies = routes.iterator
          .filter(_.root == route.root)
          .filterNot(candidate => dependsOn(candidate, route.id, Set.empty))
          .filter(candidate =>
            candidate.fields.flatMap(fieldPaths).exists(path => required.contains(candidate.mergePath ++ path))
          )
          .map(_.id)
          .toSet
        route.copy(dependencies = route.dependencies ++ dependencies)
      }
    }
  }

  private def runtimeTypeSelections(
    roots: List[PlannedRoot],
    entities: List[EntityRoute]
  ): List[RuntimeTypeSelection] =
    (roots.flatMap(_.runtimeTypes) ::: entities.flatMap(route =>
      route.typename
        .filter(_ => graph.isObjectType(route.entityType))
        .map(selection => RuntimeTypeSelection(route.mergePath, selection.responseName))
    )).distinct

  private def passthroughSource(
    routes: List[RootRoute],
    entities: List[EntityRoute],
    runtimeTypes: List[RuntimeTypeSelection],
    localFields: List[Field]
  ): Option[String] =
    if (
      sourceCount == 1 && routes.size == 1 && entities.isEmpty && runtimeTypes.isEmpty && localFields.isEmpty &&
      routes.headOption.forall(route => graph.mapping(route.source).forall(!_.nonEmpty))
    ) routes.headOption.map(_.source)
    else None

  private def planRootsAtSource(
    client: Field,
    selected: Field,
    source: String,
    sources: List[String],
    addRuntimeTypeFallback: Boolean
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[PlannedRoot]] =
    planFieldCandidates(
      selected,
      source,
      Vector(client.aliasedName),
      Set.empty,
      sources.toSet,
      availableKeys(source, selected.fieldType.innerType),
      Nil,
      Set.empty
    ).flatMap { candidates =>
      val complete = candidates.filter(_.pending.isEmpty)
      if (complete.isEmpty)
        candidates.headOption
          .fold[Either[PlanningFailure, List[PlannedRoot]]](Right(Nil))(planned =>
            Left(PlanningFailure(unsatisfiedMessage(planned.pending)))
          )
      else
        Right(complete.flatMap { planned =>
          if (hasRootWork(planned))
            List(PlannedRoot(source, client, planned.downstream, planned.entities, planned.runtimeTypes))
          else if (
            addRuntimeTypeFallback && Set[__TypeKind](__TypeKind.INTERFACE, __TypeKind.UNION)
              .contains(selected.fieldType.innerType.kind)
          ) {
            val (alias, typename) = privateTypename(
              "_caliban_gateway_runtime_typename",
              selected.fieldType.innerType,
              selected.fields.iterator.map(_.aliasedName).toSet
            )
            val downstream        = planned.downstream.copy(fields = typename :: Nil)
            List(
              PlannedRoot(
                source,
                client,
                downstream,
                planned.entities,
                RuntimeTypeSelection(Vector(client.aliasedName), alias) :: planned.runtimeTypes
              )
            )
          } else Nil
        })
    }

  private def rootFieldForSource(field: Field, source: String, rootSources: List[String]): Field = {
    def filter(
      parent: __Type,
      fields: List[Field],
      candidates: List[String],
      provided: List[Field]
    ): List[Field] = {
      val typeName = parent.name.getOrElse("")
      fields.flatMap { child =>
        val childParent = child.parentType.flatMap(_.name).getOrElse(typeName)
        val owners      = candidates.filter(graph.owns(_, childParent, child.name))
        val next        = if (owners.nonEmpty) owners else candidates
        val supplied    = provided.find(providedFieldCovers(_, child))
        val children    = filter(child.fieldType.innerType, child.fields, next, supplied.toList.flatMap(_.fields))
        val include     =
          child.name == "__typename" && candidates.contains(source) ||
            (if (child.fields.nonEmpty) children.nonEmpty
             else
               supplied.nonEmpty || owners.contains(source) || owners.isEmpty && candidates.headOption.contains(source))

        if (include) child.copy(fields = children) :: Nil else Nil
      }
    }

    val rootType = field.parentType.flatMap(_.name).getOrElse("")
    val provided = fieldSetFields(graph.provided(source, rootType, field.name), field.fieldType)
    field.copy(fields = filter(field.fieldType.innerType, field.fields, rootSources, provided))
  }

  private def hasRootWork(plan: PlannedField): Boolean =
    plan.downstream.fieldType.innerType.kind match {
      case __TypeKind.OBJECT | __TypeKind.INTERFACE | __TypeKind.UNION =>
        plan.downstream.fields.nonEmpty || plan.entities.nonEmpty
      case _                                                           => true
    }

  private def validateDependencies(routes: List[EntityRoute]): Either[PlanningFailure, Unit] = {
    val routeById = routes.iterator.map(route => route.id -> route).toMap

    def visit(
      id: RouteId,
      visiting: Set[RouteId],
      visited: Set[RouteId]
    ): Either[PlanningFailure, Set[RouteId]] =
      if (visited.contains(id)) Right(visited)
      else if (visiting.contains(id)) Left(PlanningFailure("Entity routing dependency cycle detected."))
      else
        routeById.get(id) match {
          case None        => Right(visited)
          case Some(route) =>
            route.dependencies.toList
              .sortBy(_.value)
              .foldLeft[Either[PlanningFailure, Set[RouteId]]](Right(visited)) { case (result, dependency) =>
                result.flatMap(visit(dependency, visiting + id, _))
              }
              .map(_ + id)
        }

    routes
      .sortBy(_.id.value)
      .foldLeft[Either[PlanningFailure, Set[RouteId]]](Right(Set.empty)) { case (result, route) =>
        result.flatMap(visit(route.id, Set.empty, _))
      }
      .map(_ => ())
  }

  private def planFieldCandidates(
    field: Field,
    source: String,
    path: Vector[String],
    trail: Set[TransitionKey],
    runtimeSources: Set[String],
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field],
    satisfiedRequirements: Set[(String, String)]
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[PlannedField]] = {
    val parentType     = field.fieldType.innerType
    val typeName       = parentType.name.getOrElse("")
    val sourceTypeName = field.parentType
      .flatMap(_.name)
      .flatMap(graph.field(source, _, field.name))
      .flatMap(_._type.innerType.name)
      .getOrElse(typeName)
    val possibleTypes  = field.parentType
      .flatMap(_.name)
      .map(graph.runtimeTypesForField(runtimeSources, source, _, field.name, sourceTypeName))
      .getOrElse(graph.runtimeTypes(source, sourceTypeName).toSet)
    val scoped         = mergeFields(
      provided ::: fieldSetFields(
        graph.provided(source, field.parentType.flatMap(_.name).getOrElse(""), field.name),
        field.fieldType
      )
    )
    val selections     = selectedFields(field, parentType, typeName)
      .filter(graph.appliesOnSource(source, sourceTypeName, _))
      .filter(child =>
        graph.isInterfaceObject(source, sourceTypeName) ||
          child._condition.forall(condition => possibleTypes.isEmpty || condition.exists(possibleTypes))
      )
    val runtimeType    =
      if (
        graph.isInterfaceObject(source, typeName) && selections.exists(_.targets.nonEmpty) &&
        !selections.exists(_.name == "__typename")
      )
        Some(
          privateTypename(
            "_caliban_gateway_runtime_typename",
            parentType,
            field.fields.iterator.map(_.aliasedName).toSet
          )
        )
      else None
    val routed         = selections ::: runtimeType.toList.map(_._2)

    val routedSelections = routed.flatMap { child =>
      val childParent                                   = child.parentType.flatMap(_.name).getOrElse(typeName)
      val supplied                                      = scoped.find(candidate => providedFieldCovers(candidate, child))
      def providers(owner: String): List[FieldProvider] = {
        val candidates =
          if (child.name == "__typename" && graph.isInterfaceObject(source, typeName))
            graph.runtimeTypeSource(typeName, source).toList.map(FieldProvider(_, typeName))
          else if (child.name == "__typename") List(FieldProvider(source, typeName))
          else
            supplied
              .map(_ => List(FieldProvider(source, owner)))
              .getOrElse {
                List(
                  graph.fieldSources(owner, child.name, source).map(FieldProvider(_, owner)),
                  if (owner == childParent) Nil
                  else graph.fieldSources(childParent, child.name, source).map(FieldProvider(_, childParent)),
                  if (childParent == typeName) Nil
                  else graph.fieldSources(typeName, child.name, source).map(FieldProvider(_, typeName))
                ).find(_.nonEmpty).getOrElse(Nil)
              }
        candidates.find { provider =>
          val requirements = graph.required(provider.source, provider.typeName, child.name)
          canResolveLocally(provider, source, child.name, requirements, satisfiedRequirements)
        }
          .fold(candidates)(List(_))
      }

      val directProviders = providers(childParent)
      val conditions      =
        if (
          child.name != "__typename" && !graph.isInterfaceObject(source, typeName) &&
          (childParent == typeName || directProviders.isEmpty) &&
          Set[__TypeKind](__TypeKind.INTERFACE, __TypeKind.UNION).contains(parentType.kind)
        )
          child._condition
            .fold(possibleTypes)(condition =>
              if (possibleTypes.isEmpty) condition else condition intersect possibleTypes
            )
            .filter(condition => providers(condition).nonEmpty)
            .toList
            .sorted
        else Nil

      if (conditions.isEmpty) RoutedSelection(child, supplied, directProviders) :: Nil
      else
        conditions.map { condition =>
          RoutedSelection(
            child.copy(_condition = Some(Set(condition)), targets = Some(Set(condition))),
            supplied,
            providers(condition)
          )
        }
    }

    for {
      _           <- routedSelections.find(_.providers.isEmpty) match {
                       case Some(value) =>
                         Left(PlanningFailure(s"No subgraph owns field '$typeName.${value.field.name}'."))
                       case None        => Right(())
                     }
      assignments <- providerAssignments(routedSelections)
      planned     <- search.evaluate(assignments) { assignment =>
                       val local  = mutable.ListBuffer.empty[(Field, List[Field])]
                       val remote = mutable.LinkedHashMap.empty[(String, List[Selection]), mutable.ListBuffer[Field]]
                       assignment.foreach { case (selection, provider) =>
                         val requirements = graph.required(provider.source, provider.typeName, selection.field.name)
                         if (
                           canResolveLocally(
                             provider,
                             source,
                             selection.field.name,
                             requirements,
                             satisfiedRequirements
                           )
                         )
                           local += selection.field -> selection.supplied.toList.flatMap(_.fields)
                         else
                           remote.getOrElseUpdate(
                             provider.source -> requirements,
                             mutable.ListBuffer.empty
                           ) += selection.field
                       }
                       for {
                         localPlans <- planLocalFieldCandidates(source, path, trail, runtimeSources, local.toList)
                         values     <- search.evaluate(localPlans) { localPlan =>
                                         val candidates = groupPending(
                                           remote.iterator.map { case ((target, requirements), fields) =>
                                             PendingSelection(target, fields.toList, requirements)
                                           }.toList ::: localPlan.pending
                                         )
                                         planTransitionFields(
                                           field,
                                           source,
                                           path,
                                           parentType,
                                           typeName,
                                           trail,
                                           availableExternal,
                                           scoped,
                                           localPlan.downstream,
                                           localPlan.entities,
                                           localPlan.runtimeTypes,
                                           candidates
                                         )
                                       }
                       } yield values.flatten.map { value =>
                         val withRuntimeType = addRuntimeType(field, source, path, parentType, value)
                         runtimeType.fold(withRuntimeType) { case (alias, _) =>
                           withRuntimeType.copy(
                             runtimeTypes = RuntimeTypeSelection(path, alias) :: withRuntimeType.runtimeTypes
                           )
                         }
                       }
                     }
    } yield planned.flatten
  }

  private def providerAssignments(
    selections: List[RoutedSelection]
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[List[(RoutedSelection, FieldProvider)]]] =
    selections.foldLeft[Either[PlanningFailure, List[List[(RoutedSelection, FieldProvider)]]]](Right(List(Nil))) {
      case (result, selection) =>
        for {
          current  <- result
          combined <- search.combine(current, selection.providers) { case (values, provider) =>
                        values ::: List(selection -> provider)
                      }
        } yield combined
    }

  private def planLocalFieldCandidates(
    source: String,
    path: Vector[String],
    trail: Set[TransitionKey],
    runtimeSources: Set[String],
    local: List[(Field, List[Field])]
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[PlannedSelections]] =
    local
      .foldLeft[Either[PlanningFailure, List[PlannedSelections]]](
        Right(List(PlannedSelections(Nil, Nil, Nil, Nil)))
      ) { case (result, (child, provided)) =>
        result.flatMap(values =>
          search
            .evaluate(values) { current =>
              planFieldCandidates(
                child,
                source,
                path :+ child.aliasedName,
                trail,
                graph.runtimeSources(
                  runtimeSources,
                  child.parentType.flatMap(_.name).getOrElse(""),
                  child.name
                ),
                availableKeys(source, child.fieldType.innerType),
                provided,
                Set.empty
              ).map(_.map { planned =>
                PlannedSelections(
                  planned.downstream :: current.downstream,
                  current.entities ::: planned.entities,
                  current.pending ::: wrapPending(child, planned.pending),
                  current.runtimeTypes ::: planned.runtimeTypes
                )
              })
            }
            .map(_.flatten)
        )
      }
      .map(_.map(value => value.copy(downstream = value.downstream.reverse)))

  private def planTransitionFields(
    field: Field,
    source: String,
    path: Vector[String],
    parentType: __Type,
    typeName: String,
    trail: Set[TransitionKey],
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field],
    selected: List[Field],
    nestedEntities: List[PlannedEntity],
    runtimeTypes: List[RuntimeTypeSelection],
    pending: List[PendingSelection]
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[PlannedField]] = {
    val context = TransitionContext(
      field,
      source,
      path,
      parentType,
      typeName,
      trail,
      availableExternal,
      provided
    )
    completeTransitions(
      context,
      TransitionState(selected.toVector, nestedEntities, Nil, runtimeTypes),
      pending
    ).map { states =>
      states.map { state =>
        PlannedField(
          field.copy(fields = mergeFields(state.downstream.toList)),
          state.entities,
          state.pending,
          state.runtimeTypes
        )
      }
    }
  }

  private def completeTransitions(
    context: TransitionContext,
    initial: TransitionState,
    pending: List[PendingSelection]
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[TransitionState]] =
    pending.foldLeft[Either[PlanningFailure, List[TransitionState]]](Right(List(initial))) { case (result, candidate) =>
      result.flatMap(states => search.evaluate(states)(planTransitionCandidates(context, _, candidate)).map(_.flatten))
    }

  private def planTransitionCandidates(
    context: TransitionContext,
    state: TransitionState,
    candidate: PendingSelection
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[TransitionState]] = {
    val resolution = resolveCandidate(context, candidate)
    val concrete   = resolution.lookups.filter(value => graph.isObjectType(value.entityType))
    val types      = concrete.map(_.entityType).distinct
    if (
      resolution.lookups.headOption.forall(_.entityType != context.typeName) &&
      types.size > 1
    )
      types.foldLeft[Either[PlanningFailure, List[TransitionState]]](Right(List(state))) { case (result, entityType) =>
        result.flatMap(states =>
          search
            .evaluate(states) { current =>
              val selected = candidate.copy(
                fields = candidate.fields.filter(_._condition.forall(_.contains(entityType))),
                requirements =
                  candidate.fields.flatMap(child => graph.required(candidate.source, entityType, child.name))
              )
              applyTransitionCandidates(context, current, selected, concrete.filter(_.entityType == entityType))
            }
            .map(_.flatten)
        )
      }
    else {
      val entityType = resolution.lookups.headOption.map(_.entityType).getOrElse(context.typeName)
      val transition = TransitionKey(context.source, candidate.source, entityType, flatten(candidate.fields))
      if (context.trail.contains(transition))
        Left(PlanningFailure(s"Entity routing cycle detected: ${transition.render}."))
      else if (resolution.lookups.nonEmpty) applyTransitionCandidates(context, state, candidate, resolution.lookups)
      else planBridgeCandidates(context, state, candidate, resolution.lookupTypes, transition)
    }
  }

  private def applyTransitionCandidates(
    context: TransitionContext,
    state: TransitionState,
    candidate: PendingSelection,
    lookups: List[ResolvedLookup]
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[TransitionState]] =
    search
      .evaluate(lookups.sortBy(lookupSignature)) { lookup =>
        val transition = TransitionKey(context.source, candidate.source, lookup.entityType, flatten(candidate.fields))
        if (context.trail.contains(transition))
          Left(PlanningFailure(s"Entity routing cycle detected: ${transition.render}."))
        else applyTransition(context, state, candidate, lookup, transition)
      }
      .map(_.flatten)

  private def resolveCandidate(
    context: TransitionContext,
    candidate: PendingSelection
  ): CandidateResolution = {
    val sourceType   = context.field.parentType
      .flatMap(_.name)
      .flatMap(graph.field(context.source, _, context.field.name))
      .flatMap(_._type.innerType.name)
      .getOrElse(context.typeName)
    val sourceTypes  = graph.runtimeTypes(context.source, sourceType)
    val knownTypes   =
      if (sourceTypes.nonEmpty) sourceTypes else graph.runtimeTypes(candidate.source, context.typeName)
    val conditions   = candidate.fields.iterator
      .flatMap(_._condition)
      .flatMap(_.iterator)
      .filter(name => sourceTypes.isEmpty || sourceTypes.contains(name))
    val runtimeTypes = (conditions ++ knownTypes).filter(graph.isObjectType).toList.distinct.sorted
    val lookupTypes  = ((context.typeName, context.parentType) :: runtimeTypes
      .flatMap(name => graph.rootType.types.get(name).map(name -> _))).distinct
    val lookups      = lookupTypes.flatMap { case (entityType, entityParent) =>
      val selected = selectLookups(
        entityParent,
        entityType,
        context.source,
        candidate.source,
        context.availableExternal
      ).map { case (value, fields) => LookupSelection.Static(value, fields) }
      val lookups  =
        if (selected.nonEmpty) selected else clientLookups(context.field, entityParent, entityType, candidate.source)
      lookups.map(ResolvedLookup(entityType, entityParent, _))
    }
    CandidateResolution(lookupTypes, lookups)
  }

  private def applyTransition(
    context: TransitionContext,
    state: TransitionState,
    candidate: PendingSelection,
    resolved: ResolvedLookup,
    transition: TransitionKey
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[TransitionState]] = {
    val entityField                    = context.field.copy(fieldType = resolved.parentType)
    val requirementData                =
      injectRequirementFields(
        entityField,
        state.downstream,
        fieldSetFields(candidate.requirements, resolved.parentType)
      )
    val (requiredFields, requirements) = requirementData
    for {
      requirementPlans <- planRequirementCandidates(
                            entityField,
                            context.source,
                            context.path,
                            context.trail + transition,
                            context.availableExternal,
                            context.provided,
                            requiredFields
                          )
      completed        <- search.evaluate(requirementPlans) { requirementPlan =>
                            for {
                              _       <- Either.cond(
                                           requirementPlan.pending.isEmpty,
                                           (),
                                           PlanningFailure(unsatisfiedMessage(requirementPlan.pending))
                                         )
                              planned <- planFieldCandidates(
                                           entityField.copy(fields = candidate.fields),
                                           candidate.source,
                                           context.path,
                                           context.trail + transition,
                                           Set(candidate.source),
                                           resolved.selection.lookup.key,
                                           Nil,
                                           candidate.fields.iterator
                                             .flatMap(child =>
                                               (child.parentType.flatMap(_.name).toList :::
                                                 resolved.parentType.name.toList :::
                                                 context.parentType.name.toList).map(_ -> child.name)
                                             )
                                             .toSet
                                         )
                              values  <- search.evaluate(planned) { value =>
                                           val enrichedDownstream           = mergeFields(
                                             state.downstream.toList ::: requirementPlan.downstream.fields
                                           ).toVector
                                           val keySelection                 = resolved.selection match {
                                             case LookupSelection.Static(_, fields) => fields                       -> List.empty[RequiredSelection]
                                             case LookupSelection.Client(_, fields) => List.empty[RequiredKeyField] -> fields
                                           }
                                           val keyData                      = injectKeyFields(
                                             entityField,
                                             resolved.parentType,
                                             enrichedDownstream,
                                             keySelection._1,
                                             keySelection._2,
                                             resolved.selection.lookup,
                                             transitionTarget(context.parentType, resolved.entityType)
                                           )
                                           val (downstream, keys, typename) = keyData
                                           val entity                       = PlannedEntity(
                                             candidate.source,
                                             context.source,
                                             context.path,
                                             resolved.entityType,
                                             keys,
                                             requirements,
                                             typename,
                                             resolved.selection.lookup,
                                             value.downstream.fields,
                                             value.entities,
                                             resolved.selection.requiresKeyEnrichment ||
                                               requirementPlan.entities.nonEmpty
                                           )
                                           val next                         = TransitionState(
                                             downstream,
                                             state.entities ::: requirementPlan.entities ::: (entity :: Nil),
                                             state.pending,
                                             state.runtimeTypes ::: requirementPlan.runtimeTypes :::
                                               value.runtimeTypes
                                           )
                                           completeTransitions(
                                             context.copy(
                                               field = entityField,
                                               parentType = resolved.parentType,
                                               typeName = resolved.entityType,
                                               trail = context.trail + transition
                                             ),
                                             next,
                                             value.pending
                                           )
                                         }
                            } yield values.flatten
                          }
    } yield completed.flatten
  }

  private def planBridgeCandidates(
    context: TransitionContext,
    state: TransitionState,
    candidate: PendingSelection,
    lookupTypes: List[(String, __Type)],
    transition: TransitionKey
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[TransitionState]] = {
    val candidates = lookupTypes.flatMap { case (candidateTypeName, _) =>
      bridgeSources(candidateTypeName, context.source, candidate.source)
    }.distinct
    if (candidates.isEmpty) Right(List(state.copy(pending = groupPending(state.pending ::: (candidate :: Nil)))))
    else
      search.evaluate(candidates)(next =>
        planTransitionCandidates(
          context.copy(trail = context.trail + transition),
          state,
          PendingSelection(next, candidate.fields, Nil)
        ).flatMap { values =>
          val completed = values.filter(_.pending == state.pending)
          Either.cond(
            completed.nonEmpty,
            completed,
            PlanningFailure(s"Bridge source '$next' did not complete the entity transition.")
          )
        }
      ) match {
        case Right(values)                      => Right(values.flatten)
        case Left(failure) if failure.exhausted => Left(failure)
        case Left(_)                            =>
          Right(List(state.copy(pending = groupPending(state.pending ::: (candidate :: Nil)))))
      }
  }

  private def clientLookups(
    field: Field,
    parentType: __Type,
    typeName: String,
    target: String
  ): List[LookupSelection.Client] = {
    val fields = field.collectFields(typeName)
    graph
      .lookups(target, typeName)
      .flatMap(lookup =>
        clientKeySelections(fields, parentType, lookup.key)
          .map(selected => LookupSelection.Client(lookup, selected))
      )
  }

  private def clientKeySelections(
    fields: List[Field],
    parentType: __Type,
    keys: List[ComposedGraph.KeyField]
  ): Option[List[RequiredSelection]] =
    traverse(keys)(clientKeySelection(fields, parentType, _))

  private def clientKeySelection(
    fields: List[Field],
    parentType: __Type,
    key: ComposedGraph.KeyField
  ): Option[RequiredSelection] =
    fields.find(_.name == key.name).flatMap { selected =>
      val nestedType = Option(parentType.getFieldOrNull(key.name)).map(_._type.innerType)
      nestedType.flatMap(value =>
        traverse(key.children)(clientKeySelection(selected.collectFields(value.name.getOrElse("")), value, _))
          .map(children => RequiredSelection(key.name, selected.aliasedName, children))
      )
    }

  private def traverse[A, B](values: List[A])(f: A => Option[B]): Option[List[B]] =
    values.foldRight(Option(List.empty[B])) { case (value, result) =>
      for {
        next <- f(value)
        rest <- result
      } yield next :: rest
    }

  private def fieldSetFields(selections: List[Selection], parentType: __Type): List[Field] =
    if (selections.isEmpty) Nil
    else
      Field(
        selectionSet = selections,
        fragments = Map.empty,
        variableValues = Map.empty,
        variableDefinitions = Nil,
        fieldType = parentType,
        sourceMapper = SourceMapper.empty,
        directives = Nil,
        rootType = graph.rootType
      ).fields

  private def canResolveLocally(
    provider: FieldProvider,
    currentSource: String,
    fieldName: String,
    requirements: List[Selection],
    satisfiedRequirements: Set[(String, String)]
  ): Boolean =
    provider.source == currentSource &&
      (requirements.isEmpty || satisfiedRequirements.contains(provider.typeName -> fieldName))

  private def providedFieldCovers(provided: Field, requested: Field): Boolean =
    provided.name == requested.name && provided.arguments == requested.arguments &&
      provided._condition.forall(condition => requested._condition.exists(_.subsetOf(condition)))

  private def selectedFields(field: Field, parentType: __Type, typeName: String): List[Field] =
    parentType.kind match {
      case __TypeKind.INTERFACE | __TypeKind.UNION => field.fields
      case _                                       => field.collectFields(typeName)
    }

  private def injectRequirementFields(
    field: Field,
    selected: Vector[Field],
    requirements: List[Field]
  ): (List[Field], List[RequiredSelection]) = {
    val usedNames = field.fields.iterator.map(_.aliasedName).toSet ++ selected.iterator.map(_.aliasedName)
    requirements
      .foldLeft((List.empty[Field], List.empty[RequiredSelection], usedNames)) {
        case ((fields, selections, names), requirement) =>
          val alias                = privateAlias(requirementAliasBase(requirement), names)
          val (aliased, selection) = requirementSelection(requirement.copy(alias = Some(alias)))
          (
            aliased :: fields,
            selection :: selections,
            names + alias
          )
      } match {
      case (fields, selections, _) => fields.reverse -> selections.reverse
    }
  }

  private def planRequirementCandidates(
    field: Field,
    source: String,
    path: Vector[String],
    trail: Set[TransitionKey],
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field],
    requirements: List[Field]
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[PlannedField]] =
    if (requirements.isEmpty) Right(List(PlannedField(field.copy(fields = Nil), Nil, Nil, Nil)))
    else
      planFieldCandidates(
        field.copy(fields = requirements),
        source,
        path,
        trail,
        Set(source),
        availableExternal,
        provided,
        Set.empty
      )

  private def addRuntimeType(
    field: Field,
    source: String,
    path: Vector[String],
    parentType: __Type,
    planned: PlannedField
  ): PlannedField =
    parentType.kind match {
      case __TypeKind.INTERFACE | __TypeKind.UNION
          if !parentType.name.exists(graph.isInterfaceObject(source, _)) &&
            (planned.downstream.fields.isEmpty || planned.downstream.fields.exists(_.targets.nonEmpty)) =>
        val used              =
          field.fields.iterator.map(_.aliasedName).toSet ++ planned.downstream.fields.iterator.map(_.aliasedName)
        val (alias, typename) = privateTypename("_caliban_gateway_runtime_typename", parentType, used)
        planned.copy(
          downstream = planned.downstream.copy(
            fields = planned.downstream.fields ::: typename :: Nil
          ),
          runtimeTypes = RuntimeTypeSelection(path, alias) :: planned.runtimeTypes
        )
      case _ => planned
    }

  private def requirementSelection(field: Field): (Field, RequiredSelection) = {
    val prepared         = field.fields.map(requirementSelection)
    val children         = prepared.map(_._1)
    val selections       = prepared.map(_._2)
    val needsRuntimeType = selections.exists(_.conditions.nonEmpty)
    val runtimeType      =
      if (needsRuntimeType) {
        Some(
          privateTypename(
            "_caliban_gateway_requirement_typename",
            field.fieldType.innerType,
            children.iterator.map(_.aliasedName).toSet
          )
        )
      } else None
    val downstream       = field.copy(fields = children ::: runtimeType.toList.map(_._2))
    downstream -> RequiredSelection(
      field.name,
      field.aliasedName,
      selections,
      field._condition.orElse(field.targets),
      runtimeType.map(_._1)
    )
  }

  private def requirementAliasBase(field: Field): String = {
    def parts(value: Field): List[String] =
      value.name ::
        value.arguments.toList.sortBy(_._1).flatMap { case (name, argument) =>
          name :: argument.toInputString :: Nil
        } :::
        value.targets.toList.flatMap(_.toList.sorted) :::
        value.fields.flatMap(parts)

    val suffix = parts(field)
      .mkString("_")
      .map(character =>
        if (
          character >= 'a' && character <= 'z' ||
          character >= 'A' && character <= 'Z' ||
          character >= '0' && character <= '9' ||
          character == '_'
        ) character
        else '_'
      )
      .mkString
    s"_caliban_gateway_requirement_$suffix"
  }

  private def wrapPending(field: Field, pending: List[PendingSelection]): List[PendingSelection] =
    pending.map(value => value.copy(fields = field.copy(fields = value.fields) :: Nil))

  private def injectKeyFields(
    field: Field,
    parentType: __Type,
    selected: Vector[Field],
    keyFields: List[RequiredKeyField],
    keys: List[RequiredSelection],
    lookup: ComposedGraph.EntityLookup,
    targets: Option[Set[String]]
  ): (Vector[Field], List[RequiredSelection], Option[RequiredSelection]) = {
    val usedNames    = field.fields.iterator.map(_.aliasedName).toSet ++ selected.iterator.map(_.aliasedName)
    val keyData      = keyFields.foldLeft(
      (List.empty[RequiredSelection], Vector.empty[Field], usedNames)
    ) { case ((selections, fields, names), keyField) =>
      val alias = privateAlias("_caliban_gateway_key", names)
      (
        requiredSelection(keyField, alias) :: selections,
        fields :+ requiredField(keyField, parentType, alias).copy(targets = targets),
        names + alias
      )
    }
    val injectedKeys = keyData._1.reverse
    val selections   = if (injectedKeys.isEmpty) keys else keys ::: injectedKeys
    val runtimeType  =
      if (lookup.operation.requiresTypename || targets.nonEmpty)
        Some(privateTypename("_caliban_gateway_typename", parentType, keyData._3))
      else None
    val typename     = runtimeType.map { case (alias, _) => RequiredSelection("__typename", alias) }
    (selected ++ keyData._2 ++ runtimeType.map(_._2), selections, typename)
  }

  private def privateTypename(base: String, parentType: __Type, used: Set[String]): (String, Field) = {
    val alias = privateAlias(base, used)
    alias -> Field("__typename", Types.string, Some(parentType), alias = Some(alias))
  }

  private def transitionTarget(parentType: __Type, entityType: String): Option[Set[String]] =
    parentType.kind match {
      case __TypeKind.INTERFACE | __TypeKind.UNION if graph.isObjectType(entityType) => Some(Set(entityType))
      case _                                                                         => None
    }

  private def selectLookups(
    parentType: __Type,
    typeName: String,
    source: String,
    target: String,
    availableExternal: List[ComposedGraph.KeyField]
  ): List[(ComposedGraph.EntityLookup, List[RequiredKeyField])] =
    graph
      .lookups(target, typeName)
      .flatMap(lookup => requiredKeyFields(parentType, source, lookup.key, availableExternal).toOption.map(lookup -> _))
      .sortBy { case (lookup, fields) => (if (fields.forall(_.fullyOwned)) 0 else 1, lookupSignature(lookup)) }

  private def lookupSignature(lookup: ResolvedLookup): String =
    s"${lookup.entityType}:${lookupSignature(lookup.selection.lookup)}"

  private def lookupSignature(lookup: ComposedGraph.EntityLookup): String = {
    def keys(values: List[ComposedGraph.KeyField]): String =
      values.map(value => s"${value.name}{${keys(value.children)}}").mkString(",")

    val operation = lookup.operation match {
      case _: ComposedGraph.LookupOperation.FederationEntities => "_entities"
      case value: ComposedGraph.LookupOperation.GraphQLQuery   => value.field
    }
    s"$operation:${keys(lookup.key)}"
  }

  private def requiredKeyFields(
    parentType: __Type,
    source: String,
    keys: List[ComposedGraph.KeyField],
    availableExternal: List[ComposedGraph.KeyField]
  ): Either[PlanningFailure, List[RequiredKeyField]] =
    keys
      .foldLeft[Either[PlanningFailure, List[RequiredKeyField]]](Right(Nil)) { case (result, key) =>
        for {
          fields <- result
          field  <- requiredKeyField(parentType, source, key, availableExternal)
        } yield field :: fields
      }
      .map(_.reverse)

  private def requiredKeyField(
    parentType: __Type,
    source: String,
    key: ComposedGraph.KeyField,
    availableExternal: List[ComposedGraph.KeyField]
  ): Either[PlanningFailure, RequiredKeyField] =
    for {
      typeName <- parentType.name.toRight(PlanningFailure("Entity key parent type has no name."))
      field    <- graph
                    .field(source, typeName, key.name)
                    .toRight(
                      PlanningFailure(s"Source '$source' does not provide key field '$typeName.${key.name}'.")
                    )
      carried   = availableExternal.find(_.name == key.name)
      owned     = graph.owns(source, typeName, key.name)
      _        <- Either.cond(
                    owned || carried.nonEmpty,
                    (),
                    PlanningFailure(s"Source '$source' does not provide key field '$typeName.${key.name}'.")
                  )
      children <- requiredKeyFields(field._type.innerType, source, key.children, carried.toList.flatMap(_.children))
    } yield RequiredKeyField(key.name, field, children, owned)

  private def availableKeys(source: String, tpe: __Type): List[ComposedGraph.KeyField] =
    tpe.name.toList
      .flatMap(typeName => graph.lookups(source, typeName).flatMap(_.key).filter(declaredKey(source, tpe, _)))
      .distinct

  private def declaredKey(source: String, parentType: __Type, key: ComposedGraph.KeyField): Boolean =
    parentType.name.exists { typeName =>
      graph.declares(source, typeName, key.name) && graph.field(source, typeName, key.name).exists { field =>
        key.children.forall(declaredKey(source, field._type.innerType, _))
      }
    }

  private def requiredSelection(field: RequiredKeyField, responseName: String): RequiredSelection =
    RequiredSelection(
      field.name,
      responseName,
      field.children.map(child => requiredSelection(child, child.name))
    )

  private def requiredField(field: RequiredKeyField, parentType: __Type, responseName: String): Field =
    Field(
      field.name,
      field.field._type,
      Some(parentType),
      fields = field.children.map(child => requiredField(child, field.field._type.innerType, child.name)),
      alias = Some(responseName)
    )

  private def bridgeSources(
    typeName: String,
    source: String,
    target: String
  ): List[String] =
    graph
      .lookups(target, typeName)
      .iterator
      .flatMap(lookup => graph.sourcesForKey(typeName, lookup.key).iterator)
      .filter(candidate => candidate != source && candidate != target)
      .toList
      .distinct
      .sorted

  private def groupPending(values: List[PendingSelection]): List[PendingSelection] = {
    val grouped  = mutable.LinkedHashMap.empty[(String, List[Selection]), mutable.ListBuffer[Field]]
    values.foreach(value =>
      grouped.getOrElseUpdate(value.source -> value.requirements, mutable.ListBuffer.empty) ++= value.fields
    )
    val pending  = grouped.iterator.map { case ((source, requirements), fields) =>
      PendingSelection(source, mergeFields(fields.toList), requirements)
    }.toList
    val bySource = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[PendingSelection]]
    pending.foreach(value => bySource.getOrElseUpdate(value.source, mutable.ListBuffer.empty) += value)
    bySource.valuesIterator.flatMap { values =>
      val sourcePending = values.toList
      val target        = sourcePending.indexWhere(_.requirements.nonEmpty)
      if (target < 0) sourcePending
      else {
        val unrequired = sourcePending.filter(_.requirements.isEmpty).flatMap(_.fields)
        sourcePending.zipWithIndex.collect {
          case (value, index) if value.requirements.nonEmpty =>
            if (index == target) value.copy(fields = mergeFields(value.fields ::: unrequired)) else value
        }
      }
    }.toList
  }

  private def entityDepth(routes: List[EntityRoute]): Int = {
    val byId = routes.iterator.map(route => route.id -> route).toMap

    def depth(route: EntityRoute, visiting: Set[RouteId]): Int =
      if (visiting.contains(route.id)) 0
      else
        1 + route.dependencies.iterator
          .flatMap(byId.get)
          .map(depth(_, visiting + route.id))
          .foldLeft(0)(math.max)

    routes.foldLeft(0)((value, route) => math.max(value, depth(route, Set.empty)))
  }

  private def selectionCount(selection: RequiredSelection): Int =
    1 + selection.children.foldLeft(0)((count, child) => count + selectionCount(child))

  private def internalSelectionCount(entities: List[EntityRoute]): Int =
    entities.foldLeft(0) { (count, entity) =>
      count + entity.keys.foldLeft(0)((value, key) => value + selectionCount(key)) +
        entity.requirements.foldLeft(0)((value, requirement) => value + selectionCount(requirement)) +
        entity.typename.size
    }

  private def routeCost(candidate: PlannedCandidate): StructuralCost =
    StructuralCost(
      candidate.planned.routes.size + EntityExecutor.logicalCallCount(candidate.entities),
      entityDepth(candidate.entities),
      internalSelectionCount(candidate.entities)
    )

  private def mergeFields(fields: List[Field]): List[Field] = {
    val grouped =
      mutable.LinkedHashMap.empty[(String, String, Map[String, caliban.InputValue], Option[Set[String]]), Field]
    fields.foreach { field =>
      val key = (field.aliasedName, field.name, field.arguments, field.targets)
      grouped.get(key) match {
        case Some(existing) =>
          grouped.update(key, existing.copy(fields = mergeFields(existing.fields ::: field.fields)))
        case None           => grouped.put(key, field)
      }
    }
    grouped.values.toList
  }

  private def unsatisfiedMessage(pending: List[PendingSelection]): String = {
    val obligations = pending.map(value => s"'${value.source}:${flatten(value.fields).mkString(",")}'").mkString(", ")
    s"Entity routing obligations are unsatisfied: $obligations."
  }

  private def flatten(fields: List[Field]): List[String] =
    fields.flatMap { field =>
      if (field.fields.isEmpty) field.aliasedName :: Nil
      else flatten(field.fields).map(child => s"${field.aliasedName}.$child")
    }

  private def isCustomDirective(directive: Directive): Boolean =
    directive.name != "skip" && directive.name != "include"

  private def operationRootName(operation: OperationType): String =
    operation match {
      case OperationType.Query        => graph.rootType.queryType.name.getOrElse("Query")
      case OperationType.Mutation     => graph.rootType.mutationType.flatMap(_.name).getOrElse("Mutation")
      case OperationType.Subscription => "Subscription"
    }
}

private[gateway] object OperationPlanner {
  final case class RouteId(value: Int) extends AnyVal

  final case class PlanningFailure(message: String, exhausted: Boolean = false)

  final case class Limits(maxCandidates: Int, maxExpansions: Int, timeout: Duration)

  private final class PlanningSearch(limits: Limits) {
    private val startedAt  = System.nanoTime()
    private var considered = 0L
    private var expanded   = 0

    def check: Either[PlanningFailure, Unit] =
      if (System.nanoTime() - startedAt >= limits.timeout.toNanos)
        exhausted("Route planning exceeded the configured duration limit.")
      else Right(())

    def combine[A, B, C](left: List[A], right: List[B])(combine: (A, B) => C): Either[PlanningFailure, List[C]] = {
      val count = left.size.toLong * right.size.toLong
      (left, right) match {
        case (a :: Nil, b :: Nil) => check.map(_ => combine(a, b) :: Nil)
        case (Nil, _) | (_, Nil)  => check.map(_ => Nil)
        case _                    => capacity(count).map(_ => left.flatMap(a => right.map(combine(a, _))))
      }
    }

    def evaluate[A, B](values: List[A])(
      evaluate: A => Either[PlanningFailure, B]
    ): Either[PlanningFailure, List[B]] =
      values match {
        case value :: Nil => check.flatMap(_ => evaluate(value)).map(List(_))
        case Nil          => check.flatMap(_ => Left(PlanningFailure("No complete route candidate was found.")))
        case _            =>
          for {
            _      <- candidates(values.size)
            result <- values
                        .foldLeft[Either[PlanningFailure, (List[B], Option[PlanningFailure])]](Right(Nil -> None)) {
                          case (state, value) =>
                            for {
                              current <- state
                              _       <- expand
                              next    <- evaluate(value) match {
                                           case Right(candidate)                   =>
                                             Right((candidate :: current._1) -> current._2)
                                           case Left(failure) if failure.exhausted => Left(failure)
                                           case Left(failure)                      =>
                                             Right(current._1 -> current._2.orElse(Some(failure)))
                                         }
                            } yield next
                        }
                        .flatMap { case (successes, failure) =>
                          if (successes.nonEmpty) Right(successes.reverse)
                          else Left(failure.getOrElse(PlanningFailure("No complete route candidate was found.")))
                        }
          } yield result
      }

    private def capacity(count: Long): Either[PlanningFailure, Unit] =
      if (count > limits.maxCandidates.toLong - considered)
        exhausted("Route planning exceeded the configured candidate limit.")
      else check

    private def candidates(count: Long): Either[PlanningFailure, Unit] =
      if (count <= 1) check
      else
        capacity(count).flatMap { _ =>
          considered += count
          check
        }

    private def expand: Either[PlanningFailure, Unit] =
      if (expanded >= limits.maxExpansions)
        exhausted("Route planning exceeded the configured expansion limit.")
      else {
        expanded += 1
        check
      }

    private def exhausted(message: String): Either[PlanningFailure, Unit] =
      Left(PlanningFailure(message, exhausted = true))
  }

  private final case class StructuralCost(calls: Int, depth: Int, internalSelections: Int) {
    def betterThan(other: StructuralCost): Boolean =
      calls < other.calls ||
        calls == other.calls && depth < other.depth ||
        calls == other.calls && depth == other.depth && internalSelections < other.internalSelections
  }

  private sealed trait RootStrategy
  private object RootStrategy {
    final case class Single(source: String) extends RootStrategy
    case object Split                       extends RootStrategy
  }

  private final case class PlannedRoutes(
    routes: List[RootRoute],
    assignments: List[(PlannedRoot, RouteId)]
  )

  private final case class PlannedCandidate(
    roots: List[PlannedRoot],
    planned: PlannedRoutes,
    entities: List[EntityRoute]
  )

  final case class RequiredSelection(
    field: String,
    responseName: String,
    children: List[RequiredSelection] = Nil,
    conditions: Option[Set[String]] = None,
    runtimeTypeAlias: Option[String] = None
  )

  private final case class RequiredKeyField(
    name: String,
    field: __Field,
    children: List[RequiredKeyField],
    owned: Boolean
  ) {
    def fullyOwned: Boolean = owned && children.forall(_.fullyOwned)
  }

  def privateAlias(base: String, used: Set[String]): String = {
    var candidate = base
    var suffix    = 2
    while (used.contains(candidate)) {
      candidate = s"${base}_$suffix"
      suffix += 1
    }
    candidate
  }

  private final case class PendingSelection(source: String, fields: List[Field], requirements: List[Selection])

  private final case class FieldProvider(source: String, typeName: String)

  private final case class RoutedSelection(
    field: Field,
    supplied: Option[Field],
    providers: List[FieldProvider]
  )

  private final case class TransitionKey(source: String, target: String, entityType: String, fields: List[String]) {
    def render: String = s"$source -> $target for $entityType(${fields.mkString(",")})"
  }

  private final case class TransitionContext(
    field: Field,
    source: String,
    path: Vector[String],
    parentType: __Type,
    typeName: String,
    trail: Set[TransitionKey],
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field]
  )

  private final case class ResolvedLookup(
    entityType: String,
    parentType: __Type,
    selection: LookupSelection
  )

  private final case class CandidateResolution(
    lookupTypes: List[(String, __Type)],
    lookups: List[ResolvedLookup]
  )

  private final case class PlannedSelections(
    downstream: List[Field],
    entities: List[PlannedEntity],
    pending: List[PendingSelection],
    runtimeTypes: List[RuntimeTypeSelection]
  )

  private final case class TransitionState(
    downstream: Vector[Field],
    entities: List[PlannedEntity],
    pending: List[PendingSelection],
    runtimeTypes: List[RuntimeTypeSelection]
  )

  private sealed trait LookupSelection {
    def lookup: ComposedGraph.EntityLookup
    def requiresKeyEnrichment: Boolean
  }

  private object LookupSelection {
    final case class Static(lookup: ComposedGraph.EntityLookup, fields: List[RequiredKeyField])
        extends LookupSelection {
      val requiresKeyEnrichment: Boolean = false
    }

    final case class Client(lookup: ComposedGraph.EntityLookup, fields: List[RequiredSelection])
        extends LookupSelection {
      val requiresKeyEnrichment: Boolean = true
    }
  }

  private final case class PlannedField(
    downstream: Field,
    entities: List[PlannedEntity],
    pending: List[PendingSelection],
    runtimeTypes: List[RuntimeTypeSelection]
  )

  private final case class PlannedRoot(
    source: String,
    client: Field,
    downstream: Field,
    entities: List[PlannedEntity],
    runtimeTypes: List[RuntimeTypeSelection]
  )

  private final case class PlannedEntity(
    source: String,
    dependencySource: String,
    mergePath: Vector[String],
    entityType: String,
    keys: List[RequiredSelection],
    requirements: List[RequiredSelection],
    typename: Option[RequiredSelection],
    lookup: ComposedGraph.EntityLookup,
    fields: List[Field],
    entities: List[PlannedEntity],
    requiresKeyEnrichment: Boolean
  )

  final case class RootRoute(id: RouteId, source: String, client: List[Field], downstream: List[Field])

  final case class RuntimeTypeSelection(path: Vector[String], responseName: String)

  final case class EntityRoute(
    id: RouteId,
    root: RouteId,
    source: String,
    dependencies: Set[RouteId],
    dependencySource: String,
    mergePath: Vector[String],
    entityType: String,
    keys: List[RequiredSelection],
    requirements: List[RequiredSelection],
    typename: Option[RequiredSelection],
    lookup: ComposedGraph.EntityLookup,
    fields: List[Field],
    requiresKeyEnrichment: Boolean
  ) {

    lazy val selectionKey: String = canonicalSelectionKey(fields)
  }

  private[internal] def canonicalSelectionKey(fields: List[Field]): String = {
    def renderSelection(selection: Selection): String =
      DocumentRenderer.renderCompact(
        Document(
          Definition.ExecutableDefinition
            .OperationDefinition(OperationType.Query, None, Nil, Nil, selection :: Nil) :: Nil,
          SourceMapper.empty
        )
      )

    def canonicalDirective(directive: Directive): Directive =
      directive.copy(arguments = ListMap(directive.arguments.toList.sortBy(_._1): _*), index = 0)

    def canonicalSelection(selection: Selection): Selection =
      selection match {
        case field: Selection.Field             =>
          field.copy(
            arguments = ListMap(field.arguments.toList.sortBy(_._1): _*),
            directives = field.directives.map(canonicalDirective),
            selectionSet = field.selectionSet.map(canonicalSelection).sortBy(renderSelection),
            index = 0
          )
        case fragment: Selection.InlineFragment =>
          fragment.copy(
            dirs = fragment.dirs.map(canonicalDirective),
            selectionSet = fragment.selectionSet.map(canonicalSelection).sortBy(renderSelection)
          )
        case fragment: Selection.FragmentSpread =>
          fragment.copy(directives = fragment.directives.map(canonicalDirective))
      }

    val selections = fields.map(field => canonicalSelection(field.toSelection)).sortBy(renderSelection)
    DocumentRenderer.renderCompact(
      Document(
        Definition.ExecutableDefinition.OperationDefinition(OperationType.Query, None, Nil, Nil, selections) :: Nil,
        SourceMapper.empty
      )
    )
  }

  final case class OperationPlan(
    operation: OperationType,
    rootName: String,
    fields: List[Field],
    localFields: List[Field],
    roots: List[RootRoute],
    entities: List[EntityRoute],
    runtimeTypes: List[RuntimeTypeSelection],
    passthrough: Option[String]
  ) {

    private[internal] lazy val caches: PlanCaches = new PlanCaches

    lazy val introspectionFields: List[Field] = localFields.filter(isIntrospectionField)

    lazy val hasVariableReferences: Boolean = PlanVariables.references(this)

    private[internal] def bind(variables: Map[String, InputValue]): OperationPlan =
      PlanVariables.bind(this, variables)
  }

  private[internal] object PlanVariables {

    def references(plan: OperationPlan): Boolean =
      plan.fields.exists(fieldReferences) ||
        plan.localFields.exists(fieldReferences) ||
        plan.roots.exists(route => route.client.exists(fieldReferences) || route.downstream.exists(fieldReferences)) ||
        plan.entities.exists(route => route.fields.exists(fieldReferences))

    def bind(plan: OperationPlan, variables: Map[String, InputValue]): OperationPlan = {
      def bindValue(value: InputValue): Option[InputValue] =
        value match {
          case variable: InputValue.VariableValue =>
            variables.get(variable.name)
          case InputValue.ListValue(values)       =>
            Some(InputValue.ListValue(values.map(value => bindValue(value).getOrElse(NullValue))))
          case InputValue.ObjectValue(fields)     =>
            Some(InputValue.ObjectValue(fields.flatMap { case (name, value) => bindValue(value).map(name -> _) }))
          case value                              => Some(value)
        }

      def bindDirective(directive: Directive): Directive =
        directive.copy(arguments = directive.arguments.flatMap { case (name, value) =>
          bindValue(value).map(name -> _)
        })

      def bindFragment(fragment: Fragment): Fragment =
        fragment.copy(directives = fragment.directives.map(bindDirective))

      def bindField(field: Field): Field =
        field.copy(
          fields = field.fields.map(bindField),
          arguments = field.arguments.flatMap { case (name, value) => bindValue(value).map(name -> _) },
          directives = field.directives.map(bindDirective),
          fragment = field.fragment.map(bindFragment)
        )

      plan.copy(
        fields = plan.fields.map(bindField),
        localFields = plan.localFields.map(bindField),
        roots = plan.roots.map(route =>
          route.copy(client = route.client.map(bindField), downstream = route.downstream.map(bindField))
        ),
        entities = plan.entities.map(route => route.copy(fields = route.fields.map(bindField)))
      )
    }

    private def fieldReferences(field: Field): Boolean =
      field.arguments.valuesIterator.exists(valueReferences) ||
        field.directives.exists(directiveReferences) ||
        field.fragment.exists(fragmentReferences) ||
        field.fields.exists(fieldReferences)

    private def fragmentReferences(fragment: Fragment): Boolean =
      fragment.directives.exists(directiveReferences)

    private def directiveReferences(directive: Directive): Boolean =
      directive.arguments.valuesIterator.exists(valueReferences)

    private def valueReferences(value: InputValue): Boolean =
      value match {
        case _: InputValue.VariableValue    => true
        case InputValue.ListValue(values)   => values.exists(valueReferences)
        case InputValue.ObjectValue(fields) => fields.valuesIterator.exists(valueReferences)
        case _                              => false
      }
  }
}
