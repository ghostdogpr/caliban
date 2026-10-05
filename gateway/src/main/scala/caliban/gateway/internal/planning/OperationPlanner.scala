package caliban.gateway.internal.planning

import caliban.execution.{ isMetaField, ExecutionRequest, Field }
import caliban.gateway.internal.PrivateAliases
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.composition.ComposedGraph.{ OverrideLabel, Source }
import caliban.gateway.internal.composition.DirectiveComposition.{ ArgumentCoordinate, FieldCoordinate }
import caliban.gateway.internal.planning.CandidateSearch._
import caliban.gateway.internal.planning.FetchGraphOptimizer.mergeFields
import caliban.gateway.internal.planning.OperationPlan._
import caliban.gateway.internal.planning.OperationPlanner._
import caliban.gateway._
import caliban.introspection.adt.{ __Field, __Type, __TypeKind }
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection }
import caliban.parsing.SourceMapper
import caliban.schema.Types

import scala.collection.compat._
import scala.collection.mutable

private[gateway] final class OperationPlanner(
  graph: ComposedGraph,
  limits: CandidateSearch.Limits
) {

  def progressiveOverrides(fields: List[Field]): Set[OverrideLabel] =
    if (!graph.hasProgressiveOverrides) Set.empty
    else
      fields
        .flatMap(field =>
          graph.progressiveOverrides(innerParentTypeName(field), field.name) ++ progressiveOverrides(field.fields)
        )
        .toSet

  /**
   * All recursive planning for an operation shares one mutable budget through its planning session.
   * Evaluating alternatives consumes the budget. Combining alternatives checks its remaining capacity.
   * Invalid candidates can be skipped. Budget exhaustion stops the entire search, even if an earlier
   * candidate succeeded.
   */
  def plan(
    document: Document,
    execution: ExecutionRequest,
    activeOverrides: Set[OverrideLabel]
  ): Either[PlanningFailure, OperationPlan] = {
    val selectedGraph = if (graph.hasProgressiveOverrides) graph.resolveOverrides(activeOverrides) else graph
    new PlanningSession(selectedGraph, limits, execution.operationType).plan(document, execution)
  }
}

/**
 * Private planning sessions and search intermediates, including candidates and routing state;
 * none is part of the resulting plan. OperationPlan defines the execution contract, with
 * RootFetch and EntityFetch describing selected work.
 */
private[gateway] object OperationPlanner {

  private final class PlanningSession(
    graph: ComposedGraph,
    limits: CandidateSearch.Limits,
    operationType: OperationType
  ) {
    private val search = new CandidateSearch(limits)

    def plan(document: Document, execution: ExecutionRequest): Either[PlanningFailure, OperationPlan] = {
      val fields                        = execution.field.collectFields(ComposedGraph.rootName(operationType))
      val (localFields, subgraphFields) = fields.partition(isMetaField)
      val isSubscription                = operationType == OperationType.Subscription

      for {
        _                  <- search.checkTimeout
        _                  <- Either.cond(
                                !isSubscription || (subgraphFields.size == 1 && localFields.isEmpty),
                                (),
                                PlanningFailure.Rejected("Subscriptions require exactly one non-introspection root field.")
                              )
        _                  <- Either.cond(
                                !isSubscription ||
                                  !document.hasDirective(execution.operationName)(isIncrementalDirective),
                                (),
                                PlanningFailure.Rejected("Incremental delivery is not supported inside subscriptions.")
                              )
        best               <- planRoots(subgraphFields)
        typenameSelections  = collectTypenameSelections(best.roots, best.entities)
        passthroughSubgraph = findPassthroughSubgraph(best.roots, best.entities, typenameSelections, localFields)
        _                  <- Either.cond(
                                passthroughSubgraph.nonEmpty ||
                                  !document.hasDirective(execution.operationName)(isCustomDirective),
                                (),
                                PlanningFailure.Rejected("Custom executable directives are not supported by this gateway.")
                              )
        _                  <- search.checkTimeout
        _                  <- validateContextBindings(best.roots, best.entities)
      } yield OperationPlan(
        operationType,
        execution.operationName,
        fields,
        best.roots,
        best.waves,
        typenameSelections,
        passthroughSubgraph
      )
    }

    private def planRoots(fields: List[Field]): Either[PlanningFailure, PlanCandidate] =
      search
        .fold(fields, List(List.empty[RootCandidate])) { (accumulated, field) =>
          planRootOptions(field).flatMap(search.combine(accumulated, _)(_ ::: _))
        }
        .flatMap(options =>
          search
            .evaluate(options) { candidates =>
              val planned                      = rootFetches(candidates)
              val (fetches, needPrerequisites) = entityFetches(planned.assignments, planned.roots.size)
              val entities                     = FetchGraphOptimizer.optimize(planned.roots, fetches, needPrerequisites)
              FetchGraphOptimizer.waves(planned.roots, entities).map { waves =>
                val calls = waves.map(_.map(_.groupKey).distinct.size).sum
                val cost  = PlanCost(planned.roots.size + calls, waves.size, internalSelectionCount(entities))
                PlanCandidate(planned.roots, waves, cost)
              }
            }
            .map(_.minBy(_.cost))
        )

    private def planRootOptions(
      field: Field
    ): Either[PlanningFailure, List[List[RootCandidate]]] = {
      val subgraphs = graph.rootFieldSources(operationType, field.name)
      for {
        _       <-
          Either.cond(subgraphs.nonEmpty, (), PlanningFailure.Rejected(s"No subgraph owns root field '${field.name}'."))
        options <- {
          val singles    = subgraphs.map(subgraph => () => planWholeRoot(field, subgraph, subgraphs))
          val strategies =
            if (operationType == OperationType.Mutation || subgraphs.size == 1) singles
            else singles :+ (() => planSplitRoot(field, subgraphs))
          search.flatEvaluate(strategies)(_.apply())
        }
      } yield options
    }

    private def planWholeRoot(
      field: Field,
      subgraph: Source,
      subgraphs: List[Source]
    ): Either[PlanningFailure, List[List[RootCandidate]]] =
      planRootCandidates(field, subgraph, subgraphs).flatMap { candidates =>
        val executable = candidates.flatMap { candidate =>
          if (hasRootWork(candidate)) candidate :: Nil
          else if (isAbstractType(field.fieldType.innerType)) typenameFallback(field, candidate) :: Nil
          else Nil
        }
        if (executable.nonEmpty) Right(executable.map(List(_)))
        else Left(PlanningFailure.Rejected(s"Subgraph '$subgraph' has no executable work for '${field.name}'."))
      }

    private def typenameFallback(field: Field, candidate: RootCandidate): RootCandidate = {
      val typename = privateTypename(RuntimeTypenameAliasBase, field.fieldType.innerType, responseNames(field.fields))
      candidate.copy(downstream = candidate.downstream.copy(fields = typename :: Nil))
    }

    private def planSplitRoot(
      field: Field,
      subgraphs: List[Source]
    ): Either[PlanningFailure, List[List[RootCandidate]]] =
      search
        .fold(subgraphs, List(List.empty[RootCandidate])) { (options, subgraph) =>
          val selected = rootFieldForSubgraph(field, subgraph, subgraphs)
          for {
            planned  <-
              planRootCandidates(selected, subgraph, subgraphs).map(_.filter(hasRootWork))
            combined <-
              if (planned.isEmpty) Right(options)
              else search.combine(options, planned)((current, candidate) => current :+ candidate)
          } yield combined
        }
        .flatMap { options =>
          val complete = options.filter(_.nonEmpty)
          Either.cond(
            complete.nonEmpty,
            complete,
            PlanningFailure.Rejected(s"No subgraph can execute '${field.name}'.")
          )
        }

    private def rootFetches(candidates: List[RootCandidate]): PlannedRootFetches = {
      val grouped     = mutable.LinkedHashMap.empty[(Source, Option[Int]), (FetchId, mutable.ListBuffer[RootCandidate])]
      val assignments = candidates.zipWithIndex.map { case (candidate, index) =>
        val key   = candidate.source -> (if (operationType == OperationType.Mutation) Some(index) else None)
        val group = grouped.getOrElseUpdate(key, FetchId(grouped.size) -> mutable.ListBuffer.empty[RootCandidate])
        group._2 += candidate
        candidate -> group._1
      }
      val roots       = grouped.iterator.map { case ((subgraph, _), (id, planned)) =>
        val selected = planned.toList
        RootFetch(id, subgraph, selected.map(_.downstream) ::: mergeFields(selected.flatMap(_.contextRoots)))
      }.toList
      PlannedRootFetches(roots, assignments)
    }

    private def entityFetches(
      assignments: List[(RootCandidate, FetchId)],
      firstId: Int
    ): (List[EntityFetch], Set[FetchId]) = {
      var nextFetchId       = firstId
      val needPrerequisites = Set.newBuilder[FetchId]

      def flatten(values: List[EntityCandidate], root: FetchId, dependencies: Set[FetchId]): List[EntityFetch] =
        values.flatMap { entity =>
          val id       = FetchId(nextFetchId)
          nextFetchId += 1
          if (entity.mayNeedPrerequisiteFetches) needPrerequisites += id
          val current  = entity.toFetch(id, root, dependencies)
          val children = flatten(entity.entities, root, Set(id))
          current :: children
        }

      val fetches = assignments.flatMap { case (planned, root) => flatten(planned.entities, root, Set(root)) }
      fetches -> needPrerequisites.result()
    }

    private def collectTypenameSelections(
      roots: List[RootFetch],
      entities: List[EntityFetch]
    ): List[TypenameSelection] = {
      def runtime(path: Vector[String])(field: Field): List[TypenameSelection] =
        if (field.name == TypenameField && field.aliasedName.startsWith(RuntimeTypenameAliasBase))
          TypenameSelection(path, field.aliasedName) :: Nil
        else field.fields.flatMap(runtime(path :+ field.aliasedName))
      val keyed                                                                = for {
        fetch <- entities if graph.isObjectType(fetch.target.entityType)
        alias <- fetch.typenameAlias
      } yield TypenameSelection(fetch.mergePath, alias)
      (roots.flatMap(_.downstream.flatMap(runtime(Vector.empty))) :::
        entities.flatMap(fetch => fetch.fields.flatMap(runtime(fetch.mergePath))) ::: keyed).distinct
    }

    /**
     * Allows the original request to be forwarded unchanged when no gateway-side rewriting or merging is needed.
     */
    private def findPassthroughSubgraph(
      roots: List[RootFetch],
      entities: List[EntityFetch],
      typenameSelections: List[TypenameSelection],
      localFields: List[Field]
    ): Option[Source] =
      roots match {
        case fetch :: Nil
            if graph.sources.size == 1 && entities.isEmpty && typenameSelections.isEmpty && localFields.isEmpty &&
              !fetch.source.mapping.nonEmpty =>
          Some(fetch.source)
        case _ => None
      }

    private def planRootCandidates(
      selected: Field,
      currentSubgraph: Source,
      rootSubgraphs: List[Source]
    ): Either[PlanningFailure, List[RootCandidate]] = {
      val (contextRoots, contexts) =
        graph.operationRoot(operationType).filter(_ => graph.hasContextArguments) match {
          case Some(root) => activateContexts(Field("", root, None, fields = selected :: Nil), Vector.empty, Nil)
          case None       => (Nil, Nil)
        }
      val canFetchContextRoots     =
        contextRoots.forall(field =>
          field.name == TypenameField || graph.rootFieldSources(operationType, field.name).contains(currentSubgraph)
        )
      if (!canFetchContextRoots) Right(Nil)
      else
        planFieldCandidates(
          selected,
          FieldPlanningScope(
            currentSubgraph,
            Vector(selected.aliasedName),
            isObjectField(currentSubgraph, selected),
            Set.empty,
            contexts
          ),
          rootSubgraphs.toSet,
          availableKeys(currentSubgraph, selected.fieldType.innerType),
          Nil,
          Set.empty
        ).flatMap { candidates =>
          val complete = candidates.filter(_.pending.isEmpty)
          (complete, candidates) match {
            case (Nil, planned :: _) => Left(PlanningFailure.Rejected(unsatisfiedMessage(planned.pending)))
            case _                   =>
              Right(complete.map { planned =>
                RootCandidate(
                  currentSubgraph,
                  selected.copy(fields = planned.downstream),
                  contextRoots,
                  planned.entities
                )
              })
          }
        }
    }

    private def rootFieldForSubgraph(field: Field, currentSubgraph: Source, rootSubgraphs: List[Source]): Field = {
      def filter(parent: __Type, fields: List[Field], candidates: List[Source], provided: List[Field]): List[Field] = {
        val typeName = parent.name.getOrElse("")
        fields.flatMap { child =>
          val childParent = child.parentType.flatMap(_.name).getOrElse(typeName)
          val owners      = candidates.filter(graph.ownsField(_, childParent, child.name))
          val next        = if (owners.nonEmpty) owners else candidates
          val supplied    = provided.find(providedFieldCovers(_, child))
          val children    = filter(child.fieldType.innerType, child.fields, next, supplied.toList.flatMap(_.fields))
          val include     =
            if (child.name == TypenameField && candidates.contains(currentSubgraph)) true
            else if (child.fields.nonEmpty) children.nonEmpty
            else
              supplied.nonEmpty || owners.contains(currentSubgraph) ||
              owners.isEmpty && candidates.headOption.contains(currentSubgraph)

          if (include) child.copy(fields = children) :: Nil else Nil
        }
      }

      val rootType = parentTypeName(field)
      val provided = fieldSetFields(currentSubgraph.providedFieldSet(rootType, field.name), field.fieldType)
      field.copy(fields = filter(field.fieldType.innerType, field.fields, rootSubgraphs, provided))
    }

    private def hasRootWork(candidate: RootCandidate): Boolean =
      !isCompositeType(candidate.downstream.fieldType.innerType) ||
        candidate.downstream.fields.nonEmpty || candidate.entities.nonEmpty

    private def validateContextBindings(
      roots: List[RootFetch],
      entities: List[EntityFetch]
    ): Either[PlanningFailure, Unit] = {
      def uses(source: Source, fields: List[Field]): Set[ArgumentCoordinate] =
        contextBindings(source, fields).iterator.map(_.at).toSet

      val missing = roots.iterator.flatMap(fetch => uses(fetch.source, fetch.downstream)).toSet ++
        entities.iterator.flatMap(fetch =>
          uses(fetch.target.source, fetch.fields) -- fetch.target.contextArguments.map(_.at)
        )
      Either.cond(
        missing.isEmpty,
        (),
        PlanningFailure.Rejected(
          s"Context routing obligations are unsatisfied: ${missing.toList.map(_.display).sorted.map(a => s"'$a'").mkString(", ")}."
        )
      )
    }

    private def planFieldCandidates(
      field: Field,
      scope: FieldPlanningScope,
      possibleSources: Set[Source],
      carriedKeys: List[ComposedGraph.KeyField],
      provided: List[Field],
      satisfiedRequirements: Set[FieldCoordinate]
    ): Either[PlanningFailure, List[EntityFetchState]] = {
      val currentSubgraph                    = scope.currentSubgraph
      val (contextFields, availableContexts) = activateContexts(field, scope.path, scope.activeContexts)
      val selectedField                      = field.copy(fields = field.fields ::: contextFields)
      val parentType                         = selectedField.fieldType.innerType
      val typeName                           = parentType.name.getOrElse("")
      val fieldParentType                    = selectedField.parentType.flatMap(_.name)
      val subgraphTypeName                   = sourceTypeName(currentSubgraph, selectedField)
      val possibleTypes                      = fieldParentType
        .map(graph.possibleReturnTypes(possibleSources, currentSubgraph, _, selectedField.name, subgraphTypeName))
        .getOrElse(currentSubgraph.possibleTypes(subgraphTypeName))
      val providedFields                     = mergeFields(
        provided ::: fieldSetFields(
          currentSubgraph.providedFieldSet(fieldParentType.getOrElse(""), selectedField.name),
          selectedField.fieldType
        )
      )
      val subgraphInterfaceObject            = currentSubgraph.isInterfaceObject(subgraphTypeName)
      val children                           =
        if (isAbstractType(parentType)) selectedField.fields else selectedField.collectFields(typeName)
      val selections                         = children.filter(child =>
        subgraphInterfaceObject || currentSubgraph.fieldApplies(subgraphTypeName, child) &&
          child._condition.forall(condition => possibleTypes.isEmpty || condition.exists(possibleTypes))
      )
      val interfaceObject                    = currentSubgraph.isInterfaceObject(typeName)
      val typenameField                      =
        if (interfaceObject && selections.exists(_.targets.nonEmpty) && !selections.exists(_.name == TypenameField))
          Some(privateTypename(RuntimeTypenameAliasBase, parentType, responseNames(selectedField.fields)))
        else None
      val fieldsToRoute                      = selections ::: typenameField.toList
      val context                            = EntityFetchContext(
        selectedField,
        scope.copy(activeContexts = availableContexts),
        carriedKeys,
        providedFields
      )
      val routings                           = findFieldCandidates(fieldsToRoute, context, possibleTypes, satisfiedRequirements)

      for {
        _           <- routings
                         .find(_.candidates.isEmpty)
                         .map(value => PlanningFailure.Rejected(s"No subgraph owns field '$typeName.${value.field.name}'."))
                         .toLeft(())
        assignments <- candidateAssignments(routings)
        planned     <-
          search.flatEvaluate(assignments) { assignment =>
            planAssignment(context, possibleSources, satisfiedRequirements, assignment).map(_.map { value =>
              if (!interfaceObject) addRuntimeTypename(selectedField, value) else value
            })
          }
      } yield planned
    }

    private def planAssignment(
      context: EntityFetchContext,
      possibleSources: Set[Source],
      satisfiedRequirements: Set[FieldCoordinate],
      assignment: List[(FieldRouting, SourceCandidate)]
    ): Either[PlanningFailure, List[EntityFetchState]] = {
      val (sameSubgraphFields, pending) = assignment.partitionMap { case (routing, candidate) =>
        candidate match {
          case SourceCandidate.Remote(subgraph, requirements) =>
            Right(PendingFetch(subgraph, routing.field :: Nil, requirements))
          case SourceCandidate.Local(requirements)
              if usesContext(
                context.scope.currentSubgraph,
                routing.field,
                context.scope.activeContexts,
                satisfiedRequirements
              ) =>
            Right(PendingFetch(context.scope.currentSubgraph, routing.field :: Nil, requirements))
          case _: SourceCandidate.Local                       =>
            Left(routing.field -> routing.supplied.toList.flatMap(_.fields))
        }
      }
      planSameSubgraphFields(context.scope, possibleSources, sameSubgraphFields).flatMap(states =>
        search.flatEvaluate(states)(planEntityFetches(context, _, pending))
      )
    }

    private def findFieldCandidates(
      fields: List[Field],
      context: EntityFetchContext,
      possibleTypes: Set[String],
      satisfiedRequirements: Set[FieldCoordinate]
    ): List[FieldRouting] = {
      val currentSubgraph   = context.scope.currentSubgraph
      val typeName          = context.typeName
      val isInterfaceObject = currentSubgraph.isInterfaceObject(typeName)
      fields.flatMap { child =>
        val childParent                                             = child.parentType.flatMap(_.name).getOrElse(typeName)
        val supplied                                                = context.provided.find(candidate => providedFieldCovers(candidate, child))
        val isTypename                                              = child.name == TypenameField
        def candidatesForType(owner: String): List[SourceCandidate] = {
          def sourceCandidate(subgraph: Source, declaredType: String): SourceCandidate = {
            val requirements = subgraph.requiredFieldSet(declaredType, child.name)
            if (
              subgraph == currentSubgraph &&
              (requirements.isEmpty || satisfiedRequirements.contains(FieldCoordinate(declaredType, child.name)))
            ) SourceCandidate.Local(requirements)
            else SourceCandidate.Remote(subgraph, requirements)
          }
          def declaredCandidates(declaredType: String): Option[List[SourceCandidate]]  = {
            val sources = graph.fieldSources(declaredType, child.name, currentSubgraph)
            if (sources.isEmpty) None else Some(sources.map(sourceCandidate(_, declaredType)))
          }
          val candidates                                                               =
            if (isTypename && isInterfaceObject)
              graph.typenameSource(typeName, currentSubgraph).toList.map(sourceCandidate(_, typeName))
            else if (isTypename) List(sourceCandidate(currentSubgraph, typeName))
            else if (supplied.nonEmpty) List(sourceCandidate(currentSubgraph, owner))
            else
              declaredCandidates(owner)
                .orElse(if (owner == childParent) None else declaredCandidates(childParent))
                .orElse(if (childParent == typeName) None else declaredCandidates(typeName))
                .getOrElse {
                  if (isInterfaceObject) Nil
                  else
                    graph
                      .interfaceObjectFieldSources(owner, child.name, currentSubgraph)
                      .map(key => sourceCandidate(key.source, key.typeName))
                }
          candidates.collectFirst { case local: SourceCandidate.Local => local }.fold(candidates)(List(_))
        }

        val directCandidates      = candidatesForType(childParent)
        val conditionalCandidates =
          if (
            !isTypename && !isInterfaceObject && (childParent == typeName || directCandidates.isEmpty) &&
            isAbstractType(context.parentType)
          )
            child._condition
              .fold(possibleTypes)(condition =>
                if (possibleTypes.isEmpty) condition else condition intersect possibleTypes
              )
              .flatMap { condition =>
                val values = candidatesForType(condition)
                if (values.nonEmpty) Some(condition -> values) else None
              }
              .toList
              .sortBy(_._1)
          else Nil

        val sameLocal = directCandidates match {
          case (_: SourceCandidate.Local) :: Nil => conditionalCandidates.forall(_._2 == directCandidates)
          case _                                 => false
        }
        if (conditionalCandidates.isEmpty || sameLocal) FieldRouting(child, supplied, directCandidates) :: Nil
        else
          conditionalCandidates.map { case (condition, values) =>
            FieldRouting(
              child.copy(_condition = Some(Set(condition)), targets = Some(Set(condition))),
              supplied,
              values
            )
          }
      }
    }

    private def candidateAssignments(
      routings: List[FieldRouting]
    ): Either[PlanningFailure, List[List[(FieldRouting, SourceCandidate)]]] =
      search.fold(routings, List(List.empty[(FieldRouting, SourceCandidate)])) { (current, routing) =>
        search.combine(current, routing.candidates)((values, candidate) => values :+ (routing -> candidate))
      }

    private def planSameSubgraphFields(
      scope: FieldPlanningScope,
      possibleSources: Set[Source],
      fields: List[(Field, List[Field])]
    ): Either[PlanningFailure, List[EntityFetchState]] =
      search.fold(fields, List(EntityFetchState(Nil, Nil, Nil))) { case (current, (child, provided)) =>
        for {
          alternatives <- planFieldCandidates(
                            child,
                            scope.copy(
                              path = scope.path :+ child.aliasedName,
                              staticPath = scope.staticPath && isObjectField(scope.currentSubgraph, child)
                            ),
                            graph.candidateSources(possibleSources, parentTypeName(child), child.name),
                            availableKeys(scope.currentSubgraph, child.fieldType.innerType),
                            provided,
                            Set.empty
                          )
          combined     <- search.combine(current, alternatives) { case (values, planned) =>
                            EntityFetchState(
                              values.downstream :+ child.copy(fields = planned.downstream),
                              values.entities ::: planned.entities,
                              values.pending ::: wrapPending(child, planned.pending)
                            )
                          }
        } yield combined
      }

    private def planEntityFetches(
      context: EntityFetchContext,
      selected: EntityFetchState,
      pending: List[PendingFetch]
    ): Either[PlanningFailure, List[EntityFetchState]] =
      planPendingEntityFetches(
        context,
        selected.copy(pending = Nil),
        groupPending(pending ::: selected.pending)
      ).map(_.map(state => state.copy(downstream = mergeFields(state.downstream))))

    private def planPendingEntityFetches(
      context: EntityFetchContext,
      initial: EntityFetchState,
      pending: List[PendingFetch]
    ): Either[PlanningFailure, List[EntityFetchState]] =
      search.expandAll(pending, initial)((state, next) => planEntityFetchCandidates(context, state, next))

    private def planEntityFetchCandidates(
      context: EntityFetchContext,
      state: EntityFetchState,
      pending: PendingFetch
    ): Either[PlanningFailure, List[EntityFetchState]] = {
      val resolution         = resolveEntityLookups(context, pending)
      val (direct, concrete) = resolution.lookups.partition(_.entityType == context.typeName)
      if (direct.nonEmpty && concrete.nonEmpty)
        search
          .flatEvaluate(
            List(
              () => planLookupCandidates(context, state, pending, direct),
              () => planEntityFetchesPerType(context, state, pending, resolution.lookupTypes, concrete)
            )
          )(_.apply())
      else if (concrete.map(_.entityType).distinct.size > 1)
        planEntityFetchesPerType(context, state, pending, resolution.lookupTypes, concrete)
      else if (resolution.lookups.nonEmpty) planLookupCandidates(context, state, pending, resolution.lookups)
      else
        enterFetch(context, pending, context.typeName)(
          planIndirectEntityFetches(_, state, pending, resolution.lookupTypes)
        )
    }

    private def planEntityFetchesPerType(
      context: EntityFetchContext,
      state: EntityFetchState,
      pending: PendingFetch,
      lookupTypes: List[String],
      concrete: List[ResolvedLookup]
    ): Either[PlanningFailure, List[EntityFetchState]] = {
      val types                                            = concrete.map(_.entityType).distinct
      def takes(entityType: String, field: Field): Boolean =
        field._condition.forall(_.exists(graph.acceptsRuntimeType(entityType, _))) && (
          field.name == TypenameField ||
            graph.ownsField(pending.targetSubgraph, entityType, field.name) ||
            (!pending.targetSubgraph.isInterfaceObject(entityType) &&
              graph.interfaceObjectFieldSources(entityType, field.name, pending.targetSubgraph).isEmpty)
        )
      val resolved                                         = search.expandAll(types, state) { (current, entityType) =>
        val fields = pending.fields.filter(takes(entityType, _))
        if (fields.isEmpty) Right(List(current))
        else {
          val selected = pending.copy(
            fields = fields,
            requirements =
              fields.flatMap(child => pending.targetSubgraph.requiredFieldSet(entityType, child.name)).distinct
          )
          planLookupCandidates(context, current, selected, concrete.filter(_.entityType == entityType))
        }
      }
      val unresolvedFields                                 = pending.fields.flatMap { field =>
        field._condition match {
          case Some(condition) =>
            val unresolved = condition.filterNot(runtimeType =>
              types.exists(entityType => graph.acceptsRuntimeType(entityType, runtimeType) && takes(entityType, field))
            )
            if (unresolved.isEmpty) None else Some(field.copy(_condition = Some(unresolved)))
          case None            => if (types.exists(takes(_, field))) None else Some(field)
        }
      }
      if (unresolvedFields.isEmpty) resolved
      else
        resolved.flatMap { states =>
          val remaining = pending.copy(fields = unresolvedFields)
          search.flatEvaluate(states)(state =>
            enterFetch(context, remaining, context.typeName)(
              planIndirectEntityFetches(_, state, remaining, lookupTypes)
            )
          )
        }
    }

    private def planLookupCandidates(
      context: EntityFetchContext,
      state: EntityFetchState,
      pending: PendingFetch,
      lookups: List[ResolvedLookup]
    ): Either[PlanningFailure, List[EntityFetchState]] =
      search.flatEvaluate(lookups) { lookup =>
        enterFetch(context, pending, lookup.entityType)(planEntityFetch(_, state, pending, lookup))
      }

    private def enterFetch[A](
      context: EntityFetchContext,
      pending: PendingFetch,
      entityType: String
    )(plan: EntityFetchContext => Either[PlanningFailure, A]): Either[PlanningFailure, A] = {
      val fetchKey =
        EntityFetchKey(context.scope.currentSubgraph, pending.targetSubgraph, entityType, fieldPaths(pending.fields))
      if (context.scope.visitedFetches.contains(fetchKey))
        Left(PlanningFailure.Rejected(s"Entity routing cycle detected: ${fetchKey.render}."))
      else plan(context.copy(scope = context.scope.copy(visitedFetches = context.scope.visitedFetches + fetchKey)))
    }

    private def resolveEntityLookups(context: EntityFetchContext, pending: PendingFetch): EntityLookupCandidates = {
      val subgraphTypes =
        context.scope.currentSubgraph.possibleTypes(sourceTypeName(context.scope.currentSubgraph, context.field))
      val knownTypes    =
        if (subgraphTypes.nonEmpty) subgraphTypes
        else pending.targetSubgraph.possibleTypes(context.typeName)
      val conditions    = pending.fields.iterator
        .flatMap(_._condition)
        .flatMap(_.iterator)
        .filter(name => subgraphTypes.isEmpty || subgraphTypes.contains(name))
      val runtimeTypes  = (conditions ++ knownTypes).filter(graph.isObjectType).toList.distinct.sorted
      // Prefer the declared context type before concrete runtime types; selectLookups orders keys within each type.
      val inherited     = (context.typeName :: runtimeTypes).flatMap(graph.interfaceObjectTypes(pending.targetSubgraph, _))
      val lookupTypes   = (context.parentType :: runtimeTypes.flatMap(graph.rootType.types.get) ::: inherited).distinct
      val lookups       = lookupTypes.flatMap { entityParent =>
        val selected =
          selectLookups(entityParent, context.scope.currentSubgraph, pending.targetSubgraph, context.carriedKeys)
        val usable   =
          if (selected.nonEmpty) selected
          else lookupsUsingSelectedKeys(context.field, entityParent, pending.targetSubgraph)
        usable.map(ResolvedLookup(entityParent, _))
      }
      EntityLookupCandidates(lookupTypes.map(_.name.getOrElse("")), lookups)
    }

    private def planEntityFetch(
      context: EntityFetchContext,
      state: EntityFetchState,
      pending: PendingFetch,
      resolved: ResolvedLookup
    ): Either[PlanningFailure, List[EntityFetchState]] = {
      val entityField                    = context.field.copy(fieldType = resolved.parentType)
      val selectedTypes                  = traverseOption(pending.fields)(_._condition).map(_.flatten.toSet)
      val condition                      = entityTypeCondition(context.parentType, resolved.entityType).map(types =>
        selectedTypes.map(types intersect _).filter(_.nonEmpty).getOrElse(types)
      )
      val routedThroughInterfaceObject   =
        resolved.entityType != context.typeName &&
          pending.targetSubgraph.isInterfaceObject(resolved.entityType) &&
          !context.scope.currentSubgraph.isInterfaceObject(context.typeName)
      val entityFields                   =
        if (routedThroughInterfaceObject) pending.fields.map(_.copy(_condition = None, targets = None))
        else pending.fields
      val satisfiedRequirements          = pending.fields.iterator
        .flatMap(child =>
          (child.parentType
            .flatMap(_.name)
            .toList ::: resolved.parentType.name.toList ::: context.parentType.name.toList)
            .map(FieldCoordinate(_, child.name))
        )
        .toSet
      val ordinaryRequirements           = fieldSetFields(pending.requirements, resolved.parentType)
      val (requiredFields, requirements) =
        injectRequirementFields(entityField, state.downstream, ordinaryRequirements)(List(_))
      for {
        requirementPlans <- planRequirementCandidates(
                              entityField,
                              context.scope,
                              context.carriedKeys,
                              context.provided,
                              requiredFields
                            )
        completed        <-
          search.flatEvaluate(requirementPlans) { requirementPlan =>
            for {
              _                     <- Either.cond(
                                         requirementPlan.pending.isEmpty,
                                         (),
                                         PlanningFailure.Rejected(unsatisfiedMessage(requirementPlan.pending))
                                       )
              withPrerequisiteFields = mergeFields(state.downstream ::: requirementPlan.downstream)
              injected               = injectKeyFields(
                                         entityField,
                                         withPrerequisiteFields,
                                         resolved.selection,
                                         condition,
                                         isStaticEntityType(context, resolved.entityType)
                                       )
              planned               <- planFieldCandidates(
                                         entityField.copy(fields = entityFields),
                                         context.scope.copy(currentSubgraph = pending.targetSubgraph),
                                         Set(pending.targetSubgraph),
                                         resolved.selection.lookup.key,
                                         Nil,
                                         satisfiedRequirements
                                       )
              values                <- search.flatEvaluate(planned) { value =>
                                         val contextArguments = contextualArguments(
                                           pending.targetSubgraph,
                                           value.downstream,
                                           context.scope.activeContexts
                                         )
                                         val entity           = EntityCandidate(
                                           context.scope.path,
                                           EntityTarget(
                                             pending.targetSubgraph,
                                             resolved.entityType,
                                             resolved.selection.lookup,
                                             injected.keys,
                                             requirements.flatMap(_._2),
                                             contextArguments
                                           ),
                                           injected.typenameAlias,
                                           value.downstream,
                                           value.entities,
                                           resolved.selection.mayNeedPrerequisiteFetches ||
                                             requirementPlan.entities.nonEmpty || contextArguments.nonEmpty
                                         )
                                         val next             = EntityFetchState(
                                           injected.downstream,
                                           state.entities ::: requirementPlan.entities ::: (entity :: Nil),
                                           state.pending
                                         )
                                         planPendingEntityFetches(context.copy(field = entityField), next, value.pending)
                                       }
            } yield values
          }
      } yield completed
    }

    /**
     * Tries intermediate subgraphs that can supply a key accepted by the target subgraph.
     * For example, one subgraph can resolve a Product by id and supply the sku required by another.
     */
    private def planIndirectEntityFetches(
      context: EntityFetchContext,
      state: EntityFetchState,
      pending: PendingFetch,
      lookupTypes: List[String]
    ): Either[PlanningFailure, List[EntityFetchState]] = {
      val candidates =
        lookupTypes.flatMap(intermediateSubgraphs(_, context.scope.currentSubgraph, pending.targetSubgraph)).distinct
      def deferred   = Right(List(state.copy(pending = groupPending(state.pending ::: (pending :: Nil)))))
      if (candidates.isEmpty) deferred
      else
        search.flatEvaluate(candidates)(next =>
          planEntityFetchCandidates(context, state, PendingFetch(next, pending.fields, Nil)).flatMap { values =>
            val completed = values.filter(_.pending == state.pending)
            Either.cond(
              completed.nonEmpty,
              completed,
              PlanningFailure.Rejected(s"Intermediate subgraph '$next' did not complete the entity fetch.")
            )
          }
        ) match {
          case Right(values)                            => Right(values)
          case Left(failure: PlanningFailure.Exhausted) => Left(failure)
          case Left(_)                                  => deferred
        }
    }

    private def lookupsUsingSelectedKeys(
      field: Field,
      parentType: __Type,
      targetSubgraph: Source
    ): List[LookupSelection.SelectedKeys] = {
      val typeName = parentType.name.getOrElse("")
      val fields   = field.collectFields(typeName)
      targetSubgraph
        .entityLookups(typeName)
        .flatMap(lookup =>
          traverseOption(lookup.key)(selectedKeySelection(fields, parentType, _))
            .map(selected => LookupSelection.SelectedKeys(lookup, selected))
        )
    }

    private def selectedKeySelection(
      fields: List[Field],
      parentType: __Type,
      key: ComposedGraph.KeyField
    ): Option[RequiredSelection] =
      // A key under a type condition needs a __typename the selection may lack, so it is injected instead.
      fields.find(_.name == key.name).filter(_ => key.condition.isEmpty).flatMap { selected =>
        val nestedType = Option(parentType.getFieldOrNull(key.name)).map(_._type.innerType)
        nestedType.flatMap(value =>
          traverseOption(key.children)(selectedKeySelection(selected.collectFields(value.name.getOrElse("")), value, _))
            .map(children => RequiredSelection(key.name, selected.aliasedName, children))
        )
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

    private def contextBindings(source: Source, fields: List[Field]): List[ComposedGraph.ContextArgument] =
      fields.flatMap { field =>
        source.contextArguments(parentTypeName(field), field.name) ::: contextBindings(source, field.fields)
      }

    private def activateContexts(
      field: Field,
      path: Vector[String],
      active: List[ActiveContext]
    ): (List[Field], List[ActiveContext]) =
      if (!graph.hasContextArguments) Nil -> active
      else {
        val parentType                   = field.fieldType.innerType
        val typeName                     = parentType.name.getOrElse("")
        val declarations                 = graph.contextDeclarations(typeName)
        val pending                      = declarations.flatMap { case declaration @ (source, declared) =>
          contextBindings(source, field.fields)
            .filter(_.context == declared.name)
            .map(declaration -> _)
        }.distinct.filterNot { case ((source, _), binding) =>
          active.exists(context => context.matches(source, binding) && context.argument.sourcePath == path)
        }
        val (injectedFields, selections) = injectRequirementFields(field, field.fields, pending) { case (_, binding) =>
          fieldSetFields(binding.selections, parentType)
        }
        val needsTypename                = selections.exists(_._2.exists(_.conditions.nonEmpty))
        val typename                     =
          if (needsTypename)
            Some(
              privateTypename(
                ContextTypenameAliasBase,
                parentType,
                responseNames(field.fields) ++ responseNames(injectedFields)
              )
            )
          else None
        val activated                    = selections.map { case (((source, declaration), binding), selected) =>
          ActiveContext(
            source,
            ContextualArgument(
              binding.at,
              declaration.name,
              path,
              selected,
              typename.filter(_ => selected.exists(_.conditions.nonEmpty)).map(_.aliasedName)
            )
          )
        }
        (injectedFields ::: typename.toList) -> (active ::: activated)
      }

    private def contextualArguments(
      source: Source,
      fields: List[Field],
      active: List[ActiveContext]
    ): List[ContextualArgument] =
      contextBindings(source, fields).flatMap { binding =>
        active
          .filter(_.matches(source, binding))
          .sortBy(_.argument.sourcePath.size)
          .lastOption
          .map(_.argument)
      }.distinct

    private def usesContext(
      source: Source,
      field: Field,
      active: List[ActiveContext],
      satisfied: Set[FieldCoordinate]
    ): Boolean = {
      val parent = parentTypeName(field)
      !satisfied.contains(FieldCoordinate(parent, field.name)) &&
      source
        .contextArguments(parent, field.name)
        .exists(argument => active.exists(_.matches(source, argument)))
    }

    private def providedFieldCovers(provided: Field, requested: Field): Boolean =
      provided.name == requested.name && provided.arguments == requested.arguments &&
        provided._condition.forall(condition => requested._condition.exists(_.subsetOf(condition)))

    private def injectRequirementFields[A](
      field: Field,
      selected: List[Field],
      groups: List[A],
      aliasBase: Field => String = requirementAliasBase
    )(requirements: A => List[Field]): (List[Field], List[(A, List[RequiredSelection])]) = {
      val aliases    = new PrivateAliases(responseNames(field.fields) ++ responseNames(selected))
      val injected   = mutable.ListBuffer.empty[Field]
      val selections = groups.map(group =>
        group -> requirements(group).map { requirement =>
          val base = aliasBase(requirement)
          (selected ::: injected.toList).find(existing =>
            existing.aliasedName.startsWith(base) && covers(existing, requirement)
          ) match {
            case Some(existing) => requirementSelection(existing)._2
            case None           =>
              val (aliased, selection) = requirementSelection(requirement.copy(alias = Some(aliases.next(base))))
              injected += aliased
              selection
          }
        }
      )
      injected.toList -> selections
    }

    private def planRequirementCandidates(
      field: Field,
      scope: FieldPlanningScope,
      carriedKeys: List[ComposedGraph.KeyField],
      provided: List[Field],
      requirements: List[Field]
    ): Either[PlanningFailure, List[EntityFetchState]] =
      if (requirements.isEmpty) Right(List(EntityFetchState(Nil, Nil, Nil)))
      else
        planFieldCandidates(
          field.copy(fields = requirements),
          scope.copy(activeContexts = Nil),
          Set(scope.currentSubgraph),
          carriedKeys,
          provided,
          Set.empty
        )

    private def addRuntimeTypename(field: Field, planned: EntityFetchState): EntityFetchState = {
      val parentType = field.fieldType.innerType
      if (isAbstractType(parentType) && (planned.downstream.isEmpty || planned.downstream.exists(_.targets.nonEmpty))) {
        val used = responseNames(field.fields) ++ responseNames(planned.downstream)
        planned.copy(downstream = planned.downstream :+ privateTypename(RuntimeTypenameAliasBase, parentType, used))
      } else planned
    }

    private def requirementSelection(field: Field): (Field, RequiredSelection) = {
      val prepared      = field.fields.map(requirementSelection)
      val children      = prepared.map(_._1)
      val selections    = prepared.map(_._2)
      val needsTypename = selections.exists(_.conditions.nonEmpty)
      val typenameField =
        if (needsTypename)
          Some(privateTypename(RequirementTypenameAliasBase, field.fieldType.innerType, responseNames(children)))
        else None
      val downstream    = field.copy(fields = children ::: typenameField.toList)
      downstream -> RequiredSelection(
        field.name,
        field.aliasedName,
        selections,
        field._condition.orElse(field.targets),
        typenameField.map(_.aliasedName)
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

      RequirementAliasPrefix + parts(field).mkString("_").replaceAll("[^A-Za-z0-9_]", "_")
    }

    private def wrapPending(field: Field, pending: List[PendingFetch]): List[PendingFetch] =
      pending.map(value => value.copy(fields = field.copy(fields = value.fields) :: Nil))

    private def injectKeyFields(
      field: Field,
      selected: List[Field],
      selection: LookupSelection,
      targets: Option[Set[String]],
      staticType: Boolean
    ): InjectedKeyFields = {
      val (withKeys, selections) = selection match {
        case LookupSelection.SelectedKeys(_, keys)      => (selected, keys)
        case LookupSelection.InjectedKeys(_, keyFields) =>
          val (injected, keys) = injectRequirementFields(field, selected, keyFields, _ => KeyAliasBase)(keyField =>
            requiredField(keyField, field.fieldType).copy(targets = targets) :: Nil
          )
          (selected ::: injected, keys.flatMap(_._2.map(_.copy(conditions = None))))
      }
      val requiresTypename       =
        (selection.lookup.operation == ComposedGraph.LookupOperation.FederationEntities && !staticType) || targets.nonEmpty
      val (typename, typenames)  =
        if (!requiresTypename) (Nil, Nil)
        else
          injectRequirementFields(field, withKeys, List(()), _ => TypenameAliasBase)(_ =>
            Field(TypenameField, Types.string, Some(field.fieldType)) :: Nil
          )
      InjectedKeyFields(withKeys ::: typename, selections, typenames.flatMap(_._2).headOption.map(_.responseName))
    }

    private def sourceTypeName(source: Source, field: Field): String =
      field.parentType
        .flatMap(_.name)
        .flatMap(source.fieldTypeName(_, field.name))
        .getOrElse(field.fieldType.innerType.name.getOrElse(""))

    private def composedFieldType(field: Field): Option[__Type] =
      field.parentType.flatMap(fieldDefinition(_, field.name)).map(_._type.innerType)

    private def isObjectField(currentSubgraph: Source, field: Field): Boolean =
      composedFieldType(field).exists { composed =>
        composed.kind == __TypeKind.OBJECT &&
        field.parentType.flatMap(_.name).flatMap(currentSubgraph.sourceField(_, field.name)).exists { declared =>
          val fieldType = declared._type.innerType
          fieldType.kind == __TypeKind.OBJECT && fieldType.name == composed.name
        }
      }

    private def isStaticEntityType(context: EntityFetchContext, entityType: String): Boolean =
      context.scope.staticPath && composedFieldType(context.field).exists(_.name.contains(entityType))

    private def covers(existing: Field, field: Field): Boolean =
      existing.name == field.name && existing.targets == field.targets &&
        existing.fragment.forall(_.directives.isEmpty) &&
        existing.copy(alias = None).toSelection == field.copy(alias = None).toSelection

    private def privateTypename(base: String, parentType: __Type, used: Set[String]): Field =
      Field(TypenameField, Types.string, Some(parentType), alias = Some(new PrivateAliases(used).next(base)))

    private def entityTypeCondition(parentType: __Type, entityType: String): Option[Set[String]] =
      if (!isAbstractType(parentType) || parentType.name.contains(entityType)) None
      else if (graph.isObjectType(entityType)) Some(Set(entityType))
      else {
        val implementations = graph.rootType.types.get(entityType).fold(Set.empty[String])(_.possibleTypeNames)
        val condition       = parentType.possibleTypeNames intersect implementations
        if (parentType.kind == __TypeKind.INTERFACE && condition == parentType.possibleTypeNames) None
        else Some(condition)
      }

    private def selectLookups(
      parentType: __Type,
      currentSubgraph: Source,
      targetSubgraph: Source,
      carriedKeys: List[ComposedGraph.KeyField]
    ): List[LookupSelection.InjectedKeys] =
      targetSubgraph
        .entityLookups(parentType.name.getOrElse(""))
        .flatMap(lookup =>
          requiredKeyFields(parentType, currentSubgraph, lookup.key, carriedKeys)
            .map(LookupSelection.InjectedKeys(lookup, _))
        )
        .sortBy(selection => (if (selection.fields.forall(_.fullyOwned)) 0 else 1, lookupSignature(selection.lookup)))

    private def lookupSignature(lookup: ComposedGraph.EntityLookup): String = {
      def keys(values: List[ComposedGraph.KeyField]): String =
        values.map(value => s"${value.condition.fold("")(_ + ".")}${value.name}{${keys(value.children)}}").mkString(",")

      val operation = lookup.operation match {
        case ComposedGraph.LookupOperation.FederationEntities => EntitiesField
        case value: ComposedGraph.LookupOperation.Single      => (value.path :+ value.field).mkString(".")
        case value: ComposedGraph.LookupOperation.ByKey       => value.field
      }
      s"$operation:${keys(lookup.key)}"
    }

    private def requiredKeyFields(
      parentType: __Type,
      currentSubgraph: Source,
      keys: List[ComposedGraph.KeyField],
      carriedKeys: List[ComposedGraph.KeyField]
    ): Option[List[RequiredKeyField]] =
      traverseOption(keys)(requiredKeyField(parentType, currentSubgraph, _, carriedKeys))

    private def requiredKeyField(
      parentType: __Type,
      currentSubgraph: Source,
      key: ComposedGraph.KeyField,
      carriedKeys: List[ComposedGraph.KeyField]
    ): Option[RequiredKeyField] =
      for {
        typeName <- parentType.name
        owner     = key.condition.getOrElse(typeName)
        field    <- currentSubgraph.sourceField(owner, key.name)
        carried   = carriedKeys.find(carried => carried.name == key.name && carried.condition == key.condition)
        owned     = graph.ownsField(currentSubgraph, owner, key.name)
        if owned || carried.nonEmpty
        children <-
          requiredKeyFields(field._type.innerType, currentSubgraph, key.children, carried.toList.flatMap(_.children))
      } yield RequiredKeyField(field, children, owned, key.condition)

    private def availableKeys(currentSubgraph: Source, tpe: __Type): List[ComposedGraph.KeyField] =
      tpe.name.toList
        .flatMap(typeName =>
          currentSubgraph
            .entityLookups(typeName)
            .flatMap(_.key)
            .filter(currentSubgraph.definesKeyField(typeName, _))
        )
        .distinct

    private def requiredField(field: RequiredKeyField, parentType: __Type): Field =
      Field(
        field.field.name,
        field.field._type,
        Some(parentType),
        fields = field.children.map(requiredField(_, field.field._type.innerType)),
        alias = Some(field.field.name),
        targets = field.condition.map(Set(_)),
        _condition = field.condition.map(name => graph.possibleTypesByName.getOrElse(name, Set(name)))
      )

    private def intermediateSubgraphs(typeName: String, currentSubgraph: Source, targetSubgraph: Source): List[Source] =
      targetSubgraph
        .entityLookups(typeName)
        .iterator
        .flatMap(lookup => graph.sourcesDefiningKey(typeName, lookup.key).iterator)
        .filter(candidate => candidate != currentSubgraph && candidate != targetSubgraph)
        .toList
        .distinct
        .sorted

    private def groupPending(values: List[PendingFetch]): List[PendingFetch] = {
      val grouped  = mutable.LinkedHashMap.empty[(Source, List[Selection]), mutable.ListBuffer[Field]]
      values.foreach(value =>
        grouped.getOrElseUpdate(value.targetSubgraph -> value.requirements, mutable.ListBuffer.empty) ++= value.fields
      )
      val pending  = grouped.iterator.map { case ((source, requirements), fields) =>
        PendingFetch(source, mergeFields(fields.toList), requirements)
      }.toList
      val bySource = mutable.LinkedHashMap.empty[Source, mutable.ListBuffer[PendingFetch]]
      pending.foreach(value => bySource.getOrElseUpdate(value.targetSubgraph, mutable.ListBuffer.empty) += value)
      bySource.valuesIterator.flatMap { values =>
        val (required, unrequired) = values.toList.partition(_.requirements.nonEmpty)
        required match {
          case Nil           => unrequired
          case first :: rest => first.copy(fields = mergeFields(first.fields ::: unrequired.flatMap(_.fields))) :: rest
        }
      }.toList
    }

    private def selectionCount(selection: RequiredSelection): Int =
      1 + selection.children.map(selectionCount).sum

    private def internalSelectionCount(entities: List[EntityFetch]): Int =
      entities.map { entity =>
        val contexts = entity.target.contextArguments.flatMap(_.selections)
        (entity.target.keys ::: entity.target.requirements ::: contexts)
          .map(selectionCount)
          .sum + entity.typenameAlias.size
      }.sum

    private def unsatisfiedMessage(pending: List[PendingFetch]): String = {
      val obligations =
        pending.map(value => s"'${value.targetSubgraph}:${fieldPaths(value.fields).mkString(",")}'").mkString(", ")
      s"Entity routing obligations are unsatisfied: $obligations."
    }

    private def isCustomDirective(directive: Directive): Boolean =
      !isInclusionDirective(directive)

    private def isIncrementalDirective(directive: Directive): Boolean =
      directive.name == "defer" || directive.name == "stream"

  }

  /**
   * Lexicographic cost: calls first, then dependency depth, then internal selections.
   */
  private final case class PlanCost(logicalCalls: Int, dependencyDepth: Int, internalSelections: Int)

  private object PlanCost {
    implicit val ordering: Ordering[PlanCost] =
      Ordering.by(cost => (cost.logicalCalls, cost.dependencyDepth, cost.internalSelections))
  }

  private final case class PlannedRootFetches(roots: List[RootFetch], assignments: List[(RootCandidate, FetchId)])

  private final case class PlanCandidate(
    roots: List[RootFetch],
    waves: List[List[EntityFetch]],
    cost: PlanCost
  ) {
    def entities: List[EntityFetch] = waves.flatten
  }

  private final case class RequiredKeyField(
    field: __Field,
    children: List[RequiredKeyField],
    owned: Boolean,
    condition: Option[String]
  ) {
    def fullyOwned: Boolean = owned && children.forall(_.fullyOwned)
  }

  /**
   * Fields still needing an entity fetch, together with their target subgraph and @requires prerequisites.
   * A nested selection may remain pending until an ancestor provides a usable entity lookup.
   */
  private final case class PendingFetch(targetSubgraph: Source, fields: List[Field], requirements: List[Selection])

  private sealed trait SourceCandidate

  private object SourceCandidate {
    final case class Local(requirements: List[Selection])                    extends SourceCandidate
    final case class Remote(subgraph: Source, requirements: List[Selection]) extends SourceCandidate
  }

  private final case class FieldRouting(field: Field, supplied: Option[Field], candidates: List[SourceCandidate])

  private final case class EntityFetchKey(
    currentSubgraph: Source,
    targetSubgraph: Source,
    entityType: String,
    fields: List[String]
  ) {
    def render: String = s"$currentSubgraph -> $targetSubgraph for $entityType(${fields.mkString(",")})"
  }

  private final case class FieldPlanningScope(
    currentSubgraph: Source,
    path: Vector[String],
    staticPath: Boolean,
    visitedFetches: Set[EntityFetchKey],
    activeContexts: List[ActiveContext]
  )

  private final case class EntityFetchContext(
    field: Field,
    scope: FieldPlanningScope,
    carriedKeys: List[ComposedGraph.KeyField],
    provided: List[Field]
  ) {
    def parentType: __Type = field.fieldType.innerType
    def typeName: String   = parentType.name.getOrElse("")
  }

  private final case class ResolvedLookup(parentType: __Type, selection: LookupSelection) {
    def entityType: String = parentType.name.getOrElse("")
  }

  private final case class EntityLookupCandidates(lookupTypes: List[String], lookups: List[ResolvedLookup])

  private final case class EntityFetchState(
    downstream: List[Field],
    entities: List[EntityCandidate],
    pending: List[PendingFetch]
  )

  private sealed trait LookupSelection {
    def lookup: ComposedGraph.EntityLookup

    /**
     * Whether lookup keys may come from another entity fetch instead of the current subgraph response.
     */
    def mayNeedPrerequisiteFetches: Boolean
  }

  private object LookupSelection {
    final case class InjectedKeys(lookup: ComposedGraph.EntityLookup, fields: List[RequiredKeyField])
        extends LookupSelection {
      val mayNeedPrerequisiteFetches: Boolean = false
    }

    final case class SelectedKeys(lookup: ComposedGraph.EntityLookup, fields: List[RequiredSelection])
        extends LookupSelection {
      val mayNeedPrerequisiteFetches: Boolean = true
    }
  }

  private final case class RootCandidate(
    source: Source,
    downstream: Field,
    contextRoots: List[Field],
    entities: List[EntityCandidate]
  )

  private final case class EntityCandidate(
    mergePath: Vector[String],
    target: EntityTarget,
    typenameAlias: Option[String],
    fields: List[Field],
    entities: List[EntityCandidate],
    mayNeedPrerequisiteFetches: Boolean
  ) {
    def toFetch(id: FetchId, root: FetchId, dependencies: Set[FetchId]): EntityFetch =
      EntityFetch(id, root, dependencies, mergePath, target, typenameAlias, fields)
  }

  private final case class InjectedKeyFields(
    downstream: List[Field],
    keys: List[RequiredSelection],
    typenameAlias: Option[String]
  )

  private final case class ActiveContext(source: Source, argument: ContextualArgument) {
    def matches(fetchSource: Source, binding: ComposedGraph.ContextArgument): Boolean =
      source == fetchSource && argument.at == binding.at
  }

  private final val KeyAliasBase                 = "_caliban_gateway_key"
  private final val TypenameAliasBase            = "_caliban_gateway_typename"
  private final val RuntimeTypenameAliasBase     = "_caliban_gateway_runtime_typename"
  private final val ContextTypenameAliasBase     = "_caliban_gateway_context_typename"
  private final val RequirementTypenameAliasBase = "_caliban_gateway_requirement_typename"
  private final val RequirementAliasPrefix       = "_caliban_gateway_requirement_"

}
