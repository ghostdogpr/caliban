package caliban.gateway.internal

import caliban.execution.{ ExecutionRequest, Field }
import caliban.gateway.internal.OperationPlanner._
import caliban.introspection.adt.{ __Field, __Type, __TypeKind }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection }
import caliban.schema.Types

import scala.collection.mutable

private[gateway] final class OperationPlanner(
  graph: ComposedGraph,
  sourceCount: Int
) {

  def plan(document: Document, execution: ExecutionRequest): Either[PlanningFailure, OperationPlan] = {
    val rootName     = operationRootName(execution.operationType)
    val fields       = execution.field.collectFields(rootName)
    val localFields  = fields.filter(isLocalField)
    val remoteFields = fields.filterNot(isLocalField)
    val planned      = remoteFields
      .foldLeft[Either[PlanningFailure, List[PlannedRoot]]](Right(Nil)) { case (result, field) =>
        for {
          roots   <- result
          sources  = graph.sources(execution.operationType, field.name)
          _       <- Either.cond(sources.nonEmpty, (), PlanningFailure(s"No subgraph owns root field '${field.name}'."))
          planned <-
            if (execution.operationType == OperationType.Mutation) {
              val attempts = sources.map(source => planRootAtSource(field, field, source, sources, true))
              attempts.collectFirst { case Right(Some(root)) => root } match {
                case Some(root) => Right(root :: roots)
                case None       =>
                  attempts.collectFirst { case Left(failure) => failure }
                    .fold[Either[PlanningFailure, List[PlannedRoot]]](
                      Right(roots)
                    )(Left(_))
              }
            } else
              sources
                .foldLeft[Either[PlanningFailure, List[PlannedRoot]]](Right(Nil)) { case (values, source) =>
                  for {
                    accumulated <- values
                    selected     = if (sources.size == 1) field else rootFieldForSource(field, source, sources)
                    planned     <- planRootAtSource(field, selected, source, sources, sources.size == 1)
                  } yield planned.toList ::: accumulated
                }
                .map(_ ::: roots)
        } yield planned
      }
      .map(_.reverse)

    planned.flatMap { roots =>
      val grouped                                                                         = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[PlannedRoot]]
      roots.foreach(root => grouped.getOrElseUpdate(root.source, mutable.ListBuffer.empty) += root)
      val (routes, routeAssignments)                                                      =
        if (execution.operationType == OperationType.Mutation) {
          val values = roots.zipWithIndex.map { case (root, index) =>
            RootRoute(RouteId(index), root.source, root.client :: Nil, root.downstream :: Nil)
          }
          values -> roots.zip(values).map { case (root, route) => root -> route.id }
        } else {
          val values        = grouped.iterator.zipWithIndex.map { case ((source, planned), index) =>
            val selected = planned.toList
            RootRoute(RouteId(index), source, selected.map(_.client), selected.map(_.downstream))
          }.toList
          val routeBySource = values.iterator.map(route => route.source -> route.id).toMap
          values -> roots.flatMap(root => routeBySource.get(root.source).map(root -> _))
        }
      var nextRouteId                                                                     = routes.size
      def flatten(
        values: List[PlannedEntity],
        root: RouteId,
        dependencies: Set[RouteId]
      ): List[EntityRoute] =
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
      val baseEntities                                                                    = routeAssignments.flatMap { case (planned, root) =>
        flatten(planned.entities, root, Set(root))
      }
      val entityById                                                                      = baseEntities.iterator.map(route => route.id -> route).toMap
      def dependsOn(route: EntityRoute, dependency: RouteId, seen: Set[RouteId]): Boolean =
        route.dependencies.contains(dependency) || route.dependencies.exists { id =>
          !seen.contains(id) && entityById.get(id).exists(dependsOn(_, dependency, seen + id))
        }
      def selectionPaths(selection: RequiredSelection): List[Vector[String]]              =
        if (selection.children.isEmpty) Vector(selection.responseName) :: Nil
        else selection.children.flatMap(selectionPaths).map(Vector(selection.responseName) ++ _)
      def fieldPaths(field: Field): List[Vector[String]]                                  =
        if (field.fields.isEmpty) Vector(field.aliasedName) :: Nil
        else field.fields.flatMap(fieldPaths).map(Vector(field.aliasedName) ++ _)
      val entities                                                                        = baseEntities.map { route =>
        if (!route.requiresKeyEnrichment) route
        else {
          val required     = (route.keys ::: route.requirements).flatMap(selectionPaths).map(route.mergePath ++ _).toSet
          val dependencies = baseEntities.iterator
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
      val runtimeTypes                                                                    = (
        roots.flatMap(_.runtimeTypes) ::: entities.flatMap(route =>
          route.typename
            .filter(_ => graph.isObjectType(route.entityType))
            .map(selection => RuntimeTypeSelection(route.mergePath, selection.responseName))
        )
      ).distinct
      val passthrough                                                                     =
        if (
          sourceCount == 1 && routes.size == 1 && entities.isEmpty && runtimeTypes.isEmpty && localFields.isEmpty &&
          routes.headOption.forall(route => graph.mapping(route.source).forall(!_.nonEmpty))
        )
          routes.headOption.map(_.source)
        else None

      validateDependencies(entities).flatMap { _ =>
        if (passthrough.isEmpty && hasCustomExecutableDirective(document, execution.operationName))
          Left(PlanningFailure("Custom executable directives are not supported by this gateway."))
        else
          Right(
            OperationPlan(
              execution.operationType,
              rootName,
              fields,
              localFields,
              routes,
              entities,
              runtimeTypes,
              passthrough
            )
          )
      }
    }
  }

  private def planRootAtSource(
    client: Field,
    selected: Field,
    source: String,
    sources: List[String],
    addRuntimeTypeFallback: Boolean
  ): Either[PlanningFailure, Option[PlannedRoot]] =
    for {
      planned <- planField(
                   selected,
                   source,
                   Vector(client.aliasedName),
                   Set.empty,
                   sources.toSet,
                   availableKeys(source, selected.fieldType.innerType),
                   Nil,
                   Set.empty
                 )
      _       <- Either.cond(
                   planned.pending.isEmpty,
                   (),
                   PlanningFailure(unsatisfiedMessage(planned.pending))
                 )
    } yield
      if (hasRootWork(planned))
        Some(PlannedRoot(source, client, planned.downstream, planned.entities, planned.runtimeTypes))
      else if (
        addRuntimeTypeFallback && Set[__TypeKind](__TypeKind.INTERFACE, __TypeKind.UNION)
          .contains(selected.fieldType.innerType.kind)
      ) {
        val alias      = privateAlias(
          "_caliban_gateway_runtime_typename",
          selected.fields.iterator.map(_.aliasedName).toSet
        )
        val downstream = planned.downstream.copy(fields =
          Field(
            "__typename",
            Types.string,
            Some(selected.fieldType.innerType),
            alias = Some(alias)
          ) :: Nil
        )
        Some(
          PlannedRoot(
            source,
            client,
            downstream,
            planned.entities,
            RuntimeTypeSelection(Vector(client.aliasedName), alias) :: planned.runtimeTypes
          )
        )
      } else None

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
        val supplied    = provided.find(sameField(_, child))
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

  private def planField(
    field: Field,
    source: String,
    path: Vector[String],
    trail: Set[TransitionKey],
    runtimeSources: Set[String],
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field],
    satisfiedRequirements: Set[(String, String)]
  ): Either[PlanningFailure, PlannedField] = {
    val parentType       = field.fieldType.innerType
    val typeName         = parentType.name.getOrElse("")
    val sourceTypeName   = field.parentType
      .flatMap(_.name)
      .flatMap(graph.field(source, _, field.name))
      .flatMap(_._type.innerType.name)
      .getOrElse(typeName)
    val possibleTypes    = field.parentType
      .flatMap(_.name)
      .map(graph.runtimeTypesForField(runtimeSources, source, _, field.name, sourceTypeName))
      .getOrElse(graph.runtimeTypes(source, sourceTypeName).toSet)
    val scoped           = mergeFields(
      provided ::: fieldSetFields(
        graph.provided(source, field.parentType.flatMap(_.name).getOrElse(""), field.name),
        field.fieldType
      )
    )
    val local            = mutable.ListBuffer.empty[(Field, List[Field])]
    val remote           = mutable.LinkedHashMap.empty[(String, List[Selection]), mutable.ListBuffer[Field]]
    var failure          = Option.empty[PlanningFailure]
    val selections       = selectedFields(field, parentType, typeName)
      .filter(graph.appliesOnSource(source, sourceTypeName, _))
      .filter(child =>
        graph.isInterfaceObject(source, sourceTypeName) ||
          child._condition.forall(condition => possibleTypes.isEmpty || condition.exists(possibleTypes))
      )
    val runtimeTypeAlias =
      if (
        graph.isInterfaceObject(source, typeName) && selections.exists(_.targets.nonEmpty) &&
        !selections.exists(_.name == "__typename")
      )
        Some(privateAlias("_caliban_gateway_runtime_typename", field.fields.iterator.map(_.aliasedName).toSet))
      else None
    val routed           = selections ::: runtimeTypeAlias.toList.map(alias =>
      Field("__typename", Types.string, Some(parentType), alias = Some(alias))
    )

    routed.flatMap { child =>
      val childParent                                       = child.parentType.flatMap(_.name).getOrElse(typeName)
      val supplied                                          = scoped.find(candidate => sameField(candidate, child))
      def provider(owner: String): Option[(String, String)] =
        if (child.name == "__typename" && graph.isInterfaceObject(source, typeName))
          graph.runtimeTypeSource(typeName, source).map(_ -> typeName)
        else if (child.name == "__typename") Some(source -> typeName)
        else
          supplied
            .map(_ => source -> owner)
            .orElse(graph.source(owner, child.name, source).map(_ -> owner))
            .orElse(
              if (owner == childParent) None
              else graph.source(childParent, child.name, source).map(_ -> childParent)
            )
            .orElse(
              if (childParent == typeName) None
              else graph.source(typeName, child.name, source).map(_ -> typeName)
            )

      val directProvider = provider(childParent)
      val conditions     =
        if (
          child.name != "__typename" && !graph.isInterfaceObject(source, typeName) &&
          (childParent == typeName || directProvider.isEmpty) &&
          Set[__TypeKind](__TypeKind.INTERFACE, __TypeKind.UNION).contains(parentType.kind)
        )
          child._condition
            .fold(possibleTypes)(condition =>
              if (possibleTypes.isEmpty) condition else condition intersect possibleTypes
            )
            .filter(condition => provider(condition).nonEmpty)
            .toList
            .sorted
        else Nil

      if (conditions.isEmpty) ((child, supplied), directProvider) :: Nil
      else
        conditions.map { condition =>
          child.copy(_condition = Some(Set(condition)), targets = Some(Set(condition))) -> supplied -> provider(
            condition
          )
        }
    }.foreach { case ((child, supplied), provider) =>
      provider match {
        case Some((`source`, owner)) =>
          val requirements = graph.required(source, owner, child.name)
          if (requirements.isEmpty || satisfiedRequirements.contains(owner -> child.name))
            local += child -> supplied.toList.flatMap(_.fields)
          else remote.getOrElseUpdate(source -> requirements, mutable.ListBuffer.empty) += child
        case Some((other, owner))    =>
          val requirements = graph.required(other, owner, child.name)
          remote.getOrElseUpdate(other -> requirements, mutable.ListBuffer.empty) += child
        case None                    =>
          if (failure.isEmpty) failure = Some(PlanningFailure(s"No subgraph owns field '$typeName.${child.name}'."))
      }
    }

    failure match {
      case Some(value) => Left(value)
      case None        =>
        for {
          localPlan <- planLocalFields(source, path, trail, runtimeSources, local.toList)
          candidates = groupPending(
                         remote.iterator.map { case ((target, requirements), fields) =>
                           PendingSelection(target, fields.toList, requirements)
                         }.toList :::
                           localPlan.pending
                       )
          planned   <- planTransitions(
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
        } yield {
          val withRuntimeType = addRuntimeType(field, source, path, parentType, planned)
          runtimeTypeAlias.fold(withRuntimeType)(alias =>
            withRuntimeType.copy(runtimeTypes = RuntimeTypeSelection(path, alias) :: withRuntimeType.runtimeTypes)
          )
        }
    }
  }

  private def planLocalFields(
    source: String,
    path: Vector[String],
    trail: Set[TransitionKey],
    runtimeSources: Set[String],
    local: List[(Field, List[Field])]
  ): Either[PlanningFailure, PlannedSelections] =
    local
      .foldLeft[Either[PlanningFailure, PlannedSelections]](Right(PlannedSelections(Nil, Nil, Nil, Nil))) {
        case (result, (child, provided)) =>
          for {
            values  <- result
            planned <- planField(
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
                       )
          } yield PlannedSelections(
            planned.downstream :: values.downstream,
            values.entities ::: planned.entities,
            values.pending ::: wrapPending(child, planned.pending),
            values.runtimeTypes ::: planned.runtimeTypes
          )
      }
      .map(value => value.copy(downstream = value.downstream.reverse))

  private def planTransitions(
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
  ): Either[PlanningFailure, PlannedField] =
    pending
      .foldLeft[Either[PlanningFailure, TransitionState]](
        Right(TransitionState(selected.toVector, nestedEntities, Nil, runtimeTypes))
      ) { case (result, candidate) =>
        for {
          current <- result
          next    <- planTransition(
                       TransitionContext(
                         field,
                         source,
                         path,
                         parentType,
                         typeName,
                         trail,
                         availableExternal,
                         provided
                       ),
                       current,
                       candidate
                     )
        } yield next
      }
      .map { state =>
        PlannedField(
          field.copy(fields = mergeFields(state.downstream.toList)),
          state.entities,
          state.pending,
          state.runtimeTypes
        )
      }

  private def planTransition(
    context: TransitionContext,
    state: TransitionState,
    candidate: PendingSelection,
    selectedLookup: Option[ResolvedLookup] = None
  ): Either[PlanningFailure, TransitionState] = {
    val resolution =
      selectedLookup.fold(resolveCandidate(context, candidate))(value => CandidateResolution(Nil, value :: Nil))
    val concrete   = resolution.lookups.filter(value => graph.isObjectType(value.entityType))
    if (
      selectedLookup.isEmpty && resolution.lookups.headOption.forall(_.entityType != context.typeName) &&
      concrete.size > 1
    )
      concrete.foldLeft[Either[PlanningFailure, TransitionState]](Right(state)) { case (result, value) =>
        result.flatMap(current =>
          planTransition(
            context,
            current,
            candidate.copy(
              fields = candidate.fields.filter(_._condition.forall(_.contains(value.entityType))),
              requirements =
                candidate.fields.flatMap(child => graph.required(candidate.source, value.entityType, child.name))
            ),
            Some(value)
          )
        )
      }
    else {
      val entityType = resolution.lookups.headOption.map(_.entityType).getOrElse(context.typeName)
      val transition = TransitionKey(context.source, candidate.source, entityType, flatten(candidate.fields))
      if (context.trail.contains(transition))
        Left(PlanningFailure(s"Entity routing cycle detected: ${transition.render}."))
      else
        resolution.lookups.headOption match {
          case Some(lookup) => applyTransition(context, state, candidate, lookup, transition)
          case None         => planBridge(context, state, candidate, resolution.lookupTypes, transition)
        }
    }
  }

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
      val selected =
        selectLookup(entityParent, entityType, context.source, candidate.source, context.availableExternal) match {
          case Right((value, fields)) => Some(LookupSelection.Static(value, fields))
          case Left(_)                => clientLookup(context.field, entityParent, entityType, candidate.source)
        }
      selected.map(ResolvedLookup(entityType, entityParent, _)).toList
    }
    CandidateResolution(lookupTypes, lookups)
  }

  private def applyTransition(
    context: TransitionContext,
    state: TransitionState,
    candidate: PendingSelection,
    resolved: ResolvedLookup,
    transition: TransitionKey
  ): Either[PlanningFailure, TransitionState] = {
    val entityField                    = context.field.copy(fieldType = resolved.parentType)
    val requirementData                =
      injectRequirementFields(
        entityField,
        state.downstream,
        fieldSetFields(candidate.requirements, resolved.parentType)
      )
    val (requiredFields, requirements) = requirementData
    for {
      requirementPlan             <- planRequirements(
                                       entityField,
                                       context.source,
                                       context.path,
                                       context.trail + transition,
                                       context.availableExternal,
                                       context.provided,
                                       requiredFields
                                     )
      _                           <- Either.cond(
                                       requirementPlan.pending.isEmpty,
                                       (),
                                       PlanningFailure(unsatisfiedMessage(requirementPlan.pending))
                                     )
      enrichedDownstream           = mergeFields(state.downstream.toList ::: requirementPlan.downstream.fields).toVector
      planned                     <- planField(
                                       entityField.copy(fields = candidate.fields),
                                       candidate.source,
                                       context.path,
                                       context.trail + transition,
                                       Set(candidate.source),
                                       resolved.selection.lookup.key,
                                       Nil,
                                       candidate.fields.iterator
                                         .flatMap(child =>
                                           (child.parentType.flatMap(_.name).toList ::: resolved.parentType.name.toList :::
                                             context.parentType.name.toList).map(_ -> child.name)
                                         )
                                         .toSet
                                     )
      keyData                      = resolved.selection match {
                                       case LookupSelection.Static(value, fields) =>
                                         injectKeyFields(
                                           entityField,
                                           resolved.parentType,
                                           enrichedDownstream,
                                           fields,
                                           value,
                                           transitionTarget(context.parentType, resolved.entityType)
                                         )
                                       case LookupSelection.Client(value, fields) =>
                                         injectSelectedKeys(
                                           entityField,
                                           resolved.parentType,
                                           enrichedDownstream,
                                           fields,
                                           value,
                                           transitionTarget(context.parentType, resolved.entityType)
                                         )
                                     }
      (downstream, keys, typename) = keyData
      entity                       = PlannedEntity(
                                       candidate.source,
                                       context.source,
                                       context.path,
                                       resolved.entityType,
                                       keys,
                                       requirements,
                                       typename,
                                       resolved.selection.lookup,
                                       planned.downstream.fields,
                                       planned.entities,
                                       resolved.selection.requiresKeyEnrichment || requirementPlan.entities.nonEmpty
                                     )
      next                         = TransitionState(
                                       downstream,
                                       state.entities ::: requirementPlan.entities ::: (entity :: Nil),
                                       state.pending,
                                       state.runtimeTypes ::: requirementPlan.runtimeTypes ::: planned.runtimeTypes
                                     )
      completed                   <- planned.pending.foldLeft[Either[PlanningFailure, TransitionState]](Right(next)) {
                                       case (result, pending) =>
                                         result.flatMap(current =>
                                           planTransition(
                                             context.copy(
                                               field = entityField,
                                               parentType = resolved.parentType,
                                               typeName = resolved.entityType,
                                               trail = context.trail + transition
                                             ),
                                             current,
                                             pending
                                           )
                                         )
                                     }
    } yield completed
  }

  private def planBridge(
    context: TransitionContext,
    state: TransitionState,
    candidate: PendingSelection,
    lookupTypes: List[(String, __Type)],
    transition: TransitionKey
  ): Either[PlanningFailure, TransitionState] = {
    val candidates = lookupTypes.flatMap { case (candidateTypeName, _) =>
      bridgeSources(candidateTypeName, context.source, candidate.source)
    }.distinct
    val attempts   = candidates.iterator.map(next =>
      planTransition(
        context.copy(trail = context.trail + transition),
        state,
        PendingSelection(next, candidate.fields, Nil)
      )
    )
    attempts.collect { case Right(next) if next.pending == state.pending => next }.reduceOption { (best, next) =>
      if (entityCount(next.entities) < entityCount(best.entities)) next else best
    }
      .map(Right(_))
      .getOrElse(Right(state.copy(pending = groupPending(state.pending ::: (candidate :: Nil)))))
  }

  private def clientLookup(
    field: Field,
    parentType: __Type,
    typeName: String,
    target: String
  ): Option[LookupSelection.Client] = {
    val fields = field.collectFields(typeName)
    graph
      .lookups(target, typeName)
      .collectFirst(
        Function.unlift { lookup =>
          clientKeySelections(fields, parentType, lookup.key)
            .map(selected => LookupSelection.Client(lookup, selected))
        }
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

  private def sameField(left: Field, right: Field): Boolean =
    left.name == right.name && left.arguments == right.arguments

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

  private def planRequirements(
    field: Field,
    source: String,
    path: Vector[String],
    trail: Set[TransitionKey],
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field],
    requirements: List[Field]
  ): Either[PlanningFailure, PlannedField] =
    if (requirements.isEmpty) Right(PlannedField(field.copy(fields = Nil), Nil, Nil, Nil))
    else
      planField(
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
        val used  =
          field.fields.iterator.map(_.aliasedName).toSet ++ planned.downstream.fields.iterator.map(_.aliasedName)
        val alias = privateAlias("_caliban_gateway_runtime_typename", used)
        planned.copy(
          downstream = planned.downstream.copy(
            fields = planned.downstream.fields :::
              Field("__typename", Types.string, Some(parentType), alias = Some(alias)) :: Nil
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
        val used  = children.iterator.map(_.aliasedName).toSet
        val alias = privateAlias("_caliban_gateway_requirement_typename", used)
        Some(alias -> Field("__typename", Types.string, Some(field.fieldType.innerType), alias = Some(alias)))
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
    lookup: ComposedGraph.EntityLookup,
    targets: Option[Set[String]]
  ): (Vector[Field], List[RequiredSelection], Option[RequiredSelection]) = {
    val usedNames = field.fields.iterator.map(_.aliasedName).toSet ++ selected.iterator.map(_.aliasedName)
    val keyData   = keyFields.foldLeft(
      (List.empty[RequiredSelection], Vector.empty[Field], usedNames)
    ) { case ((selections, fields, names), keyField) =>
      val alias = privateAlias("_caliban_gateway_key", names)
      (
        requiredSelection(keyField, alias) :: selections,
        fields :+ requiredField(keyField, parentType, alias).copy(targets = targets),
        names + alias
      )
    }
    val keys      = keyData._1.reverse
    val typename  =
      if (lookup.operation.requiresTypename || targets.nonEmpty)
        Some(RequiredSelection("__typename", privateAlias("_caliban_gateway_typename", keyData._3)))
      else None
    val typeField = typename.map(selection =>
      Field(selection.field, Types.string, Some(parentType), alias = Some(selection.responseName))
    )
    (selected ++ keyData._2 ++ typeField, keys, typename)
  }

  private def injectSelectedKeys(
    field: Field,
    parentType: __Type,
    selected: Vector[Field],
    keys: List[RequiredSelection],
    lookup: ComposedGraph.EntityLookup,
    targets: Option[Set[String]]
  ): (Vector[Field], List[RequiredSelection], Option[RequiredSelection]) = {
    val usedNames = field.fields.iterator.map(_.aliasedName).toSet ++ selected.iterator.map(_.aliasedName)
    val typename  =
      if (lookup.operation.requiresTypename || targets.nonEmpty)
        Some(RequiredSelection("__typename", privateAlias("_caliban_gateway_typename", usedNames)))
      else None
    val typeField = typename.map(selection =>
      Field(selection.field, Types.string, Some(parentType), alias = Some(selection.responseName))
    )
    (selected ++ typeField, keys, typename)
  }

  private def transitionTarget(parentType: __Type, entityType: String): Option[Set[String]] =
    parentType.kind match {
      case __TypeKind.INTERFACE | __TypeKind.UNION if graph.isObjectType(entityType) => Some(Set(entityType))
      case _                                                                         => None
    }

  private def selectLookup(
    parentType: __Type,
    typeName: String,
    source: String,
    target: String,
    availableExternal: List[ComposedGraph.KeyField]
  ): Either[PlanningFailure, (ComposedGraph.EntityLookup, List[RequiredKeyField])] = {
    val lookups = graph.lookups(target, typeName)
    lookups
      .flatMap(lookup => requiredKeyFields(parentType, source, lookup.key, availableExternal).toOption.map(lookup -> _))
      .sortBy { case (_, fields) => if (fields.forall(_.fullyOwned)) 0 else 1 }
      .headOption
      .toRight(
        PlanningFailure(
          if (lookups.isEmpty)
            s"Cannot route '$typeName': source '$target' has no resolvable entity lookup."
          else s"Cannot route '$typeName': no key accepted by '$target' is available from '$source'."
        )
      )
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

  private def entityCount(entities: List[PlannedEntity]): Int =
    entities.foldLeft(0)((count, entity) => count + 1 + entityCount(entity.entities))

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

  private def isLocalField(field: Field): Boolean =
    field.name == "__schema" || field.name == "__type" || field.name == "__typename"

  private def hasCustomExecutableDirective(document: Document, operationName: Option[String]): Boolean = {
    val fragments = document.fragmentDefinitions.iterator.map(fragment => fragment.name -> fragment).toMap

    def loop(selections: List[Selection], visitedFragments: Set[String]): Boolean =
      selections.exists {
        case Selection.Field(_, _, _, directives, selectionSet, _) =>
          directives.exists(isCustomDirective) || loop(selectionSet, visitedFragments)
        case Selection.InlineFragment(_, directives, selectionSet) =>
          directives.exists(isCustomDirective) || loop(selectionSet, visitedFragments)
        case Selection.FragmentSpread(name, directives)            =>
          directives.exists(isCustomDirective) ||
          (!visitedFragments.contains(name) && fragments
            .get(name)
            .exists(fragment =>
              fragment.directives.exists(isCustomDirective) || loop(fragment.selectionSet, visitedFragments + name)
            ))
      }

    val operation = operationName match {
      case Some(name) => document.operationDefinitions.find(_.name.contains(name))
      case None       =>
        document.operationDefinitions match {
          case operation :: Nil => Some(operation)
          case _                => None
        }
    }

    operation.exists(operation =>
      operation.directives.exists(isCustomDirective) ||
        operation.variableDefinitions.exists(_.directives.exists(isCustomDirective)) ||
        loop(operation.selectionSet, Set.empty)
    )
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

  final case class PlanningFailure(message: String)

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
  )

  final case class OperationPlan(
    operation: OperationType,
    rootName: String,
    fields: List[Field],
    localFields: List[Field],
    roots: List[RootRoute],
    entities: List[EntityRoute],
    runtimeTypes: List[RuntimeTypeSelection],
    passthrough: Option[String]
  )
}
