package caliban.gateway.internal.planning

import caliban.execution.{ isMetaField, ExecutionRequest, Field }
import caliban.gateway.internal.PrivateAliases
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.composition.ComposedGraph.{ FieldArgument, OverrideLabel }
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
  subgraphCount: Int,
  limits: CandidateSearch.Limits
) {

  def hasProgressiveOverrides: Boolean = graph.hasProgressiveOverrides

  def progressiveOverrides(
    document: Document,
    operationName: Option[String]
  ): Map[OverrideLabel, Option[BigDecimal]] = {
    val selected = Set.newBuilder[String]
    document.foreachSelection(operationName) {
      case Selection.Field(_, name, _, _, _, _) => selected += name
      case _                                    => ()
    }
    graph.progressiveOverrides(selected.result())
  }

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
    new PlanningSession(selectedGraph, subgraphCount, limits).plan(document, execution)
  }
}

/**
 * Private planning sessions and search intermediates, including candidates and routing state;
 * none is part of the resulting plan. OperationPlan defines the execution contract, with
 * RootFetch and EntityFetch describing selected work.
 */
private[gateway] object OperationPlanner {

  private final class PlanningSession(graph: ComposedGraph, subgraphCount: Int, limits: CandidateSearch.Limits) {
    private val search = new CandidateSearch(limits)

    def plan(document: Document, execution: ExecutionRequest): Either[PlanningFailure, OperationPlan] = {
      val rootName                      = operationRootName(execution.operationType)
      val fields                        = execution.field.collectFields(rootName)
      val (localFields, subgraphFields) = fields.partition(isMetaField)
      val isSubscription                = execution.operationType == OperationType.Subscription

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
        best               <- planRoots(subgraphFields, execution.operationType)
        typenameSelections  = collectTypenameSelections(best.rootCandidates, best.entities)
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
        execution.operationType,
        rootName,
        fields,
        localFields,
        best.roots,
        best.entities,
        typenameSelections,
        passthroughSubgraph
      )
    }

    private def planRoots(fields: List[Field], operationType: OperationType): Either[PlanningFailure, PlanCandidate] =
      search
        .fold(fields, List(List.empty[RootCandidate])) { (accumulated, field) =>
          planRootOptions(field, operationType).flatMap(search.combine(accumulated, _)(_ ::: _))
        }
        .flatMap(options =>
          search
            .evaluate(options) { candidates =>
              val planned                      = rootFetches(candidates, operationType)
              val (fetches, needPrerequisites) = entityFetches(planned.assignments, planned.roots.size)
              val entities                     = FetchGraphOptimizer.optimize(planned.roots, fetches, needPrerequisites)
              FetchGraphOptimizer.waves(planned.roots, entities).map { waves =>
                val calls = waves.map(_.map(_.groupKey).distinct.size).sum
                val cost  = PlanCost(planned.roots.size + calls, waves.size, internalSelectionCount(entities))
                PlanCandidate(candidates, planned.roots, entities, cost)
              }
            }
            .map(_.minBy(_.cost))
        )

    private def planRootOptions(
      field: Field,
      operationType: OperationType
    ): Either[PlanningFailure, List[List[RootCandidate]]] = {
      val subgraphs = graph.rootFieldSources(operationType, field.name)
      for {
        _       <-
          Either.cond(subgraphs.nonEmpty, (), PlanningFailure.Rejected(s"No subgraph owns root field '${field.name}'."))
        options <- {
          val singles    = subgraphs.map(subgraph => () => planWholeRoot(field, subgraph, subgraphs, operationType))
          val strategies =
            if (operationType == OperationType.Mutation || subgraphs.size == 1) singles
            else singles :+ (() => planSplitRoot(field, subgraphs, operationType))
          search.flatEvaluate(strategies)(_.apply())
        }
      } yield options
    }

    private def planWholeRoot(
      field: Field,
      subgraph: String,
      subgraphs: List[String],
      operationType: OperationType
    ): Either[PlanningFailure, List[List[RootCandidate]]] =
      planRootCandidates(field, field, subgraph, subgraphs, operationType).flatMap { candidates =>
        val executable = candidates.flatMap { candidate =>
          if (hasRootWork(candidate)) candidate :: Nil
          else if (isAbstractType(field.fieldType.innerType)) typenameFallback(field, candidate) :: Nil
          else Nil
        }
        if (executable.nonEmpty) Right(executable.map(List(_)))
        else Left(PlanningFailure.Rejected(s"Subgraph '$subgraph' has no executable work for '${field.name}'."))
      }

    private def typenameFallback(field: Field, candidate: RootCandidate): RootCandidate = {
      val (typename, selection) =
        runtimeTypename(Vector(field.aliasedName), field.fieldType.innerType, responseNames(field.fields))
      candidate.copy(
        downstream = candidate.downstream.copy(fields = typename :: Nil),
        typenameSelections = selection :: candidate.typenameSelections
      )
    }

    private def planSplitRoot(
      field: Field,
      subgraphs: List[String],
      operationType: OperationType
    ): Either[PlanningFailure, List[List[RootCandidate]]] =
      search
        .fold(subgraphs, List(List.empty[RootCandidate])) { (options, subgraph) =>
          val selected = rootFieldForSubgraph(field, subgraph, subgraphs)
          for {
            planned  <-
              planRootCandidates(field, selected, subgraph, subgraphs, operationType).map(_.filter(hasRootWork))
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

    private def rootFetches(candidates: List[RootCandidate], operationType: OperationType): PlannedRootFetches = {
      val grouped     = mutable.LinkedHashMap.empty[(String, Option[Int]), (FetchId, mutable.ListBuffer[RootCandidate])]
      val assignments = candidates.zipWithIndex.map { case (candidate, index) =>
        val key   = candidate.source -> (if (operationType == OperationType.Mutation) Some(index) else None)
        val group = grouped.getOrElseUpdate(key, FetchId(grouped.size) -> mutable.ListBuffer.empty[RootCandidate])
        group._2 += candidate
        candidate -> group._1
      }
      val roots       = grouped.iterator.map { case ((subgraph, _), (id, planned)) =>
        val selected = planned.toList
        RootFetch(
          id,
          subgraph,
          selected.map(_.client),
          mergeFields(selected.map(_.downstream)),
          mergeFields(selected.flatMap(_.contextRoots))
        )
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
      candidates: List[RootCandidate],
      entities: List[EntityFetch]
    ): List[TypenameSelection] = {
      val entitySelections = for {
        fetch     <- entities if graph.isObjectType(fetch.entityType)
        selection <- fetch.typename
      } yield TypenameSelection(fetch.mergePath, selection.responseName)
      (candidates.flatMap(_.typenameSelections) ::: entitySelections).distinct
    }

    /**
     * Allows the original request to be forwarded unchanged when no gateway-side rewriting or merging is needed.
     */
    private def findPassthroughSubgraph(
      roots: List[RootFetch],
      entities: List[EntityFetch],
      typenameSelections: List[TypenameSelection],
      localFields: List[Field]
    ): Option[String] =
      roots match {
        case fetch :: Nil
            if subgraphCount == 1 && entities.isEmpty && typenameSelections.isEmpty && localFields.isEmpty &&
              !graph.schemaMapping(fetch.source).nonEmpty =>
          Some(fetch.source)
        case _ => None
      }

    private def planRootCandidates(
      client: Field,
      selected: Field,
      currentSubgraph: String,
      rootSubgraphs: List[String],
      operationType: OperationType
    ): Either[PlanningFailure, List[RootCandidate]] = {
      val (selectedRoot, contextRoots, contexts) =
        if (!graph.hasContextArguments) (selected, Nil, Nil)
        else {
          val rootType                        = graph.rootType.types(operationRootName(operationType))
          val contextRoot                     = Field("", rootType, None, fields = selected :: Nil)
          val (selectedContextRoot, contexts) = activateContexts(contextRoot, Vector.empty, Nil)
          val (selectedRoots, contextRoots)   = selectedContextRoot.fields.partition(field =>
            field.name == selected.name && field.aliasedName == selected.aliasedName &&
              field.arguments == selected.arguments
          )
          (selectedRoots.headOption.getOrElse(selected), contextRoots, contexts)
        }
      val canFetchContextRoots                   =
        contextRoots.forall(field =>
          field.name == TypenameField || graph.rootFieldSources(operationType, field.name).contains(currentSubgraph)
        )
      if (!canFetchContextRoots) Right(Nil)
      else
        planFieldCandidates(
          selectedRoot,
          FieldPlanningScope(
            currentSubgraph,
            Vector(client.aliasedName),
            isObjectField(currentSubgraph, selectedRoot),
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
                  client,
                  planned.downstream,
                  contextRoots,
                  planned.entities,
                  planned.typenameSelections
                )
              })
          }
        }
    }

    private def rootFieldForSubgraph(field: Field, currentSubgraph: String, rootSubgraphs: List[String]): Field = {
      def filter(parent: __Type, fields: List[Field], candidates: List[String], provided: List[Field]): List[Field] = {
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
      val provided = fieldSetFields(graph.providedFieldSet(currentSubgraph, rootType, field.name), field.fieldType)
      field.copy(fields = filter(field.fieldType.innerType, field.fields, rootSubgraphs, provided))
    }

    private def hasRootWork(candidate: RootCandidate): Boolean =
      !isCompositeType(candidate.downstream.fieldType.innerType) ||
        candidate.downstream.fields.nonEmpty || candidate.entities.nonEmpty

    private def validateContextBindings(
      roots: List[RootFetch],
      entities: List[EntityFetch]
    ): Either[PlanningFailure, Unit] =
      if (!graph.hasContextArguments) Right(())
      else {
        def uses(source: String, fields: List[Field]): Set[FieldArgument] =
          contextBindings(source, fields).iterator.map(_.fieldArgument).toSet

        val missing = roots.iterator.flatMap(fetch => uses(fetch.source, fetch.selections)).toSet ++
          entities.iterator.flatMap(fetch =>
            uses(fetch.source, fetch.fields) -- fetch.contextArguments.map(_.fieldArgument)
          )
        Either.cond(
          missing.isEmpty,
          (),
          PlanningFailure.Rejected(
            s"Context routing obligations are unsatisfied: ${missing.toList
                .sortBy(argument => (argument.typeName, argument.fieldName, argument.argumentName))
                .map(argument => s"'${argument.typeName}.${argument.fieldName}(${argument.argumentName}:)'")
                .mkString(", ")}."
          )
        )
      }

    private def planFieldCandidates(
      field: Field,
      scope: FieldPlanningScope,
      possibleSources: Set[String],
      carriedKeys: List[ComposedGraph.KeyField],
      provided: List[Field],
      satisfiedRequirements: Set[(String, String)]
    ): Either[PlanningFailure, List[FieldCandidate]] = {
      val currentSubgraph                    = scope.currentSubgraph
      val (selectedField, availableContexts) = activateContexts(field, scope.path, scope.activeContexts)
      val parentType                         = selectedField.fieldType.innerType
      val typeName                           = parentType.name.getOrElse("")
      val fieldParentType                    = selectedField.parentType.flatMap(_.name)
      val subgraphTypeName                   = fieldParentType
        .flatMap(graph.sourceFieldTypeName(currentSubgraph, _, selectedField.name))
        .getOrElse(typeName)
      val possibleTypes                      = fieldParentType
        .map(graph.possibleReturnTypes(possibleSources, currentSubgraph, _, selectedField.name, subgraphTypeName))
        .getOrElse(graph.possibleTypes(currentSubgraph, subgraphTypeName).toSet)
      val providedFields                     = mergeFields(
        provided ::: fieldSetFields(
          graph.providedFieldSet(currentSubgraph, fieldParentType.getOrElse(""), selectedField.name),
          selectedField.fieldType
        )
      )
      val selections                         = selectedFields(selectedField, parentType, typeName)
        .filter(graph.fieldApplies(currentSubgraph, subgraphTypeName, _))
        .filter(child =>
          graph.isInterfaceObject(currentSubgraph, subgraphTypeName) ||
            child._condition.forall(condition => possibleTypes.isEmpty || condition.exists(possibleTypes))
        )
      val typenameField                      =
        if (
          graph.isInterfaceObject(currentSubgraph, typeName) && selections.exists(_.targets.nonEmpty) &&
          !selections.exists(_.name == TypenameField)
        ) Some(runtimeTypename(scope.path, parentType, responseNames(selectedField.fields)))
        else None
      val fieldsToRoute                      = selections ::: typenameField.toList.map(_._1)
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
              val withTypename =
                addTypenameSelection(selectedField, currentSubgraph, scope.path, parentType, value)
              typenameField.fold(withTypename) { case (_, selection) =>
                withTypename.copy(typenameSelections = selection :: withTypename.typenameSelections)
              }
            })
          }
      } yield planned
    }

    private def planAssignment(
      context: EntityFetchContext,
      possibleSources: Set[String],
      satisfiedRequirements: Set[(String, String)],
      assignment: List[(FieldRouting, SourceCandidate)]
    ): Either[PlanningFailure, List[FieldCandidate]] = {
      val (sameSubgraphFields, pending) = assignment.partitionMap { case (routing, candidate) =>
        candidate match {
          case SourceCandidate.Remote(subgraph, requirements) =>
            Right(PendingFetch(subgraph, routing.field :: Nil, requirements))
          case SourceCandidate.Local(requirements)
              if graph.hasContextArguments &&
                usesContext(
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
      satisfiedRequirements: Set[(String, String)]
    ): List[FieldRouting] = {
      val currentSubgraph   = context.scope.currentSubgraph
      val typeName          = context.typeName
      val isInterfaceObject = graph.isInterfaceObject(currentSubgraph, typeName)
      fields.flatMap { child =>
        val childParent                                             = child.parentType.flatMap(_.name).getOrElse(typeName)
        val supplied                                                = context.provided.find(candidate => providedFieldCovers(candidate, child))
        val isTypename                                              = child.name == TypenameField
        def candidatesForType(owner: String): List[SourceCandidate] = {
          def sourceCandidate(subgraph: String, declaredType: String): SourceCandidate = {
            val requirements = graph.requiredFieldSet(subgraph, declaredType, child.name)
            if (
              subgraph == currentSubgraph &&
              (requirements.isEmpty || satisfiedRequirements.contains(declaredType -> child.name))
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
                    graph.interfaceObjectFieldSources(owner, child.name, currentSubgraph).map {
                      case (source, interfaceName) => sourceCandidate(source, interfaceName)
                    }
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

        if (conditionalCandidates.isEmpty) FieldRouting(child, supplied, directCandidates) :: Nil
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
      possibleSources: Set[String],
      fields: List[(Field, List[Field])]
    ): Either[PlanningFailure, List[EntityFetchState]] =
      search.fold(fields, List(EntityFetchState(Nil, Nil, Nil, Nil))) { case (current, (child, provided)) =>
        for {
          alternatives <-
            if (current.isEmpty) Left(NoCompleteCandidate)
            else
              planFieldCandidates(
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
                              values.downstream :+ planned.downstream,
                              values.entities ::: planned.entities,
                              values.pending ::: wrapPending(child, planned.pending),
                              values.typenameSelections ::: planned.typenameSelections
                            )
                          }
        } yield combined
      }

    private def planEntityFetches(
      context: EntityFetchContext,
      selected: EntityFetchState,
      pending: List[PendingFetch]
    ): Either[PlanningFailure, List[FieldCandidate]] =
      planPendingEntityFetches(
        context,
        selected.copy(pending = Nil),
        groupPending(pending ::: selected.pending)
      ).map { states =>
        states.map { state =>
          FieldCandidate(
            context.field.copy(fields = mergeFields(state.downstream)),
            state.entities,
            state.pending,
            state.typenameSelections
          )
        }
      }

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
      val resolution = resolveEntityLookups(context, pending)
      val direct     = resolution.lookups.filter(_.entityType == context.typeName)
      val concrete   = resolution.lookups.filter(value =>
        value.entityType != context.typeName &&
          (graph.isObjectType(value.entityType) || graph.isInterfaceObject(pending.targetSubgraph, value.entityType))
      )
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
      lookupTypes: List[(String, __Type)],
      concrete: List[ResolvedLookup]
    ): Either[PlanningFailure, List[EntityFetchState]] = {
      val types                                            = concrete.map(_.entityType).distinct
      def takes(entityType: String, field: Field): Boolean =
        field._condition.forall(_.exists(graph.acceptsRuntimeType(entityType, _))) && (
          field.name == TypenameField ||
            graph.ownsField(pending.targetSubgraph, entityType, field.name) ||
            (!graph.isInterfaceObject(pending.targetSubgraph, entityType) &&
              graph.interfaceObjectFieldSources(entityType, field.name, pending.targetSubgraph).isEmpty)
        )
      val resolved                                         = search.expandAll(types, state) { (current, entityType) =>
        val fields = pending.fields.filter(takes(entityType, _))
        if (fields.isEmpty) Right(List(current))
        else {
          val selected = pending.copy(
            fields = fields,
            requirements =
              fields.flatMap(child => graph.requiredFieldSet(pending.targetSubgraph, entityType, child.name)).distinct
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
      val subgraphType  = context.field.parentType
        .flatMap(_.name)
        .flatMap(graph.sourceFieldTypeName(context.scope.currentSubgraph, _, context.field.name))
        .getOrElse(context.typeName)
      val subgraphTypes = graph.possibleTypes(context.scope.currentSubgraph, subgraphType)
      val knownTypes    =
        if (subgraphTypes.nonEmpty) subgraphTypes
        else graph.possibleTypes(pending.targetSubgraph, context.typeName)
      val conditions    = pending.fields.iterator
        .flatMap(_._condition)
        .flatMap(_.iterator)
        .filter(name => subgraphTypes.isEmpty || subgraphTypes.contains(name))
      val runtimeTypes  = (conditions ++ knownTypes).filter(graph.isObjectType).toList.distinct.sorted
      // Prefer the declared context type before concrete runtime types; selectLookups orders keys within each type.
      val inherited     = (context.typeName :: runtimeTypes).flatMap(graph.interfaceObjectTypes(pending.targetSubgraph, _))
      val lookupTypes   = ((context.typeName, context.parentType) :: runtimeTypes
        .flatMap(name => graph.rootType.types.get(name).map(name -> _)) ::: inherited).distinct
      val lookups       = lookupTypes.flatMap { case (entityType, entityParent) =>
        val selected = selectLookups(
          entityParent,
          entityType,
          context.scope.currentSubgraph,
          pending.targetSubgraph,
          context.carriedKeys
        ).map { case (value, fields) => LookupSelection.InjectedKeys(value, fields) }
        val usable   =
          if (selected.nonEmpty) selected
          else lookupsUsingSelectedKeys(context.field, entityParent, entityType, pending.targetSubgraph)
        usable.map(ResolvedLookup(entityType, entityParent, _))
      }
      EntityLookupCandidates(lookupTypes, lookups)
    }

    private def planEntityFetch(
      context: EntityFetchContext,
      state: EntityFetchState,
      pending: PendingFetch,
      resolved: ResolvedLookup
    ): Either[PlanningFailure, List[EntityFetchState]] = {
      val entityField                    = context.field.copy(fieldType = resolved.parentType)
      val selectedTypes                  =
        if (pending.fields.forall(_._condition.nonEmpty)) pending.fields.flatMap(_._condition).flatten.toSet
        else Set.empty[String]
      val condition                      = entityTypeCondition(context.parentType, resolved.entityType).map { types =>
        if (selectedTypes.isEmpty) types
        else {
          val selected = types intersect selectedTypes
          if (selected.isEmpty) types else selected
        }
      }
      val routedThroughInterfaceObject   =
        resolved.entityType != context.typeName &&
          graph.isInterfaceObject(pending.targetSubgraph, resolved.entityType) &&
          !graph.isInterfaceObject(context.scope.currentSubgraph, context.typeName)
      val entityFields                   =
        if (routedThroughInterfaceObject) pending.fields.map(_.copy(_condition = None, targets = None))
        else pending.fields
      val satisfiedRequirements          = pending.fields.iterator
        .flatMap(child =>
          (child.parentType
            .flatMap(_.name)
            .toList ::: resolved.parentType.name.toList ::: context.parentType.name.toList)
            .map(_ -> child.name)
        )
        .toSet
      val ordinaryRequirements           = fieldSetFields(pending.requirements, resolved.parentType)
      val (requiredFields, requirements) =
        injectRequirementFields(entityField, state.downstream, ordinaryRequirements)
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
              withPrerequisiteFields = mergeFields(state.downstream ::: requirementPlan.downstream.fields)
              injected               = injectKeyFields(
                                         entityField,
                                         resolved.parentType,
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
                                           value.downstream.fields,
                                           context.scope.activeContexts
                                         )
                                         val entity           = EntityCandidate(
                                           pending.targetSubgraph,
                                           context.scope.path,
                                           resolved.entityType,
                                           injected.keys,
                                           requirements,
                                           contextArguments,
                                           injected.typename,
                                           resolved.selection.lookup,
                                           value.downstream.fields,
                                           value.entities,
                                           resolved.selection.mayNeedPrerequisiteFetches ||
                                             requirementPlan.entities.nonEmpty || contextArguments.nonEmpty
                                         )
                                         val next             = EntityFetchState(
                                           injected.downstream,
                                           state.entities ::: requirementPlan.entities ::: (entity :: Nil),
                                           state.pending,
                                           state.typenameSelections ::: requirementPlan.typenameSelections :::
                                             value.typenameSelections
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
      lookupTypes: List[(String, __Type)]
    ): Either[PlanningFailure, List[EntityFetchState]] = {
      val candidates = lookupTypes.flatMap { case (candidateTypeName, _) =>
        intermediateSubgraphs(candidateTypeName, context.scope.currentSubgraph, pending.targetSubgraph)
      }.distinct
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
      typeName: String,
      targetSubgraph: String
    ): List[LookupSelection.SelectedKeys] = {
      val fields = field.collectFields(typeName)
      graph
        .entityLookups(targetSubgraph, typeName)
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
      fields.find(_.name == key.name).flatMap { selected =>
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

    private def contextBindings(source: String, fields: List[Field]): List[ContextBinding] =
      fields.flatMap { field =>
        val parent = parentTypeName(field)
        graph.contextArguments(source, parent, field.name).map(ContextBinding(parent, field.name, _)) :::
          contextBindings(source, field.fields)
      }

    private def activateContexts(
      field: Field,
      path: Vector[String],
      active: List[ActiveContext]
    ): (Field, List[ActiveContext]) =
      if (!graph.hasContextArguments) field -> active
      else {
        val parentType   = field.fieldType.innerType
        val typeName     = parentType.name.getOrElse("")
        val declarations = graph.contextDeclarations(typeName)
        val pending      = declarations.flatMap { declaration =>
          contextBindings(declaration.source, field.fields)
            .filter(_.argument.context == declaration.name)
            .map(declaration -> _)
        }.distinct.filterNot { case (declaration, binding) =>
          active.exists(context => context.matches(declaration.source, binding) && context.argument.sourcePath == path)
        }
        if (pending.isEmpty) field -> active
        else {
          val requirements                 = pending.map { case (_, binding) =>
            binding -> fieldSetFields(binding.argument.selections, parentType)
          }
          val (injectedFields, selections) = injectRequirementFields(field, field.fields, requirements.flatMap(_._2))
          val withSelections               = requirements
            .foldLeft((List.empty[(ContextBinding, List[RequiredSelection])], selections)) {
              case ((values, remaining), (binding, fields)) =>
                val (selected, tail) = remaining.splitAt(fields.size)
                ((binding -> selected) :: values, tail)
            }
            ._1
            .reverse
          val needsTypename                = withSelections.exists(_._2.exists(_.conditions.nonEmpty))
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
          val selectedField                = field.copy(fields = field.fields ::: injectedFields ::: typename.toList)
          val activated                    = pending.zip(withSelections).map { case ((declaration, _), (binding, selected)) =>
            ActiveContext(
              declaration.source,
              ContextualArgument(
                binding.parentType,
                binding.field,
                binding.argument.argument,
                declaration.name,
                path,
                typeName,
                contextProjection(selected, typename.map(_.aliasedName))
              )
            )
          }
          selectedField -> (active ::: activated)
        }
      }

    private def contextProjection(
      selections: List[RequiredSelection],
      typenameAlias: Option[String]
    ): ContextProjection = {
      def chain(selection: RequiredSelection): List[String] =
        selection.responseName :: selection.children.headOption.fold(List.empty[String])(chain)

      typenameAlias match {
        case Some(alias) if selections.exists(_.conditions.nonEmpty) =>
          ContextProjection.ByType(
            alias,
            selections.foldLeft(Map.empty[String, List[String]]) { (byType, selection) =>
              val names = chain(selection)
              selection.conditions.fold(byType)(_.foldLeft(byType) { (byType, runtimeType) =>
                if (byType.contains(runtimeType)) byType else byType.updated(runtimeType, names)
              })
            }
          )
        case _                                                       => ContextProjection.Path(selections.headOption.fold(List.empty[String])(chain))
      }
    }

    private def contextualArguments(
      source: String,
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
      source: String,
      field: Field,
      active: List[ActiveContext],
      satisfied: Set[(String, String)]
    ): Boolean = {
      val parent = parentTypeName(field)
      !satisfied.contains(parent -> field.name) &&
      graph
        .contextArguments(source, parent, field.name)
        .exists(argument => active.exists(_.matches(source, ContextBinding(parent, field.name, argument))))
    }

    private def providedFieldCovers(provided: Field, requested: Field): Boolean =
      provided.name == requested.name && provided.arguments == requested.arguments &&
        provided._condition.forall(condition => requested._condition.exists(_.subsetOf(condition)))

    private def selectedFields(field: Field, parentType: __Type, typeName: String): List[Field] =
      if (isAbstractType(parentType)) field.fields else field.collectFields(typeName)

    private def injectRequirementFields(
      field: Field,
      selected: List[Field],
      requirements: List[Field]
    ): (List[Field], List[RequiredSelection]) = {
      val aliases    = new PrivateAliases(responseNames(field.fields) ++ responseNames(selected))
      val injected   = mutable.ListBuffer.empty[Field]
      val selections = requirements.map { requirement =>
        val base = requirementAliasBase(requirement)
        (selected.iterator ++ injected.iterator).find(existing =>
          existing.aliasedName.startsWith(base) && covers(existing, requirement)
        ) match {
          case Some(existing) => requirementSelection(existing)._2
          case None           =>
            val (aliased, selection) = requirementSelection(requirement.copy(alias = Some(aliases.next(base))))
            injected += aliased
            selection
        }
      }
      injected.toList -> selections
    }

    private def planRequirementCandidates(
      field: Field,
      scope: FieldPlanningScope,
      carriedKeys: List[ComposedGraph.KeyField],
      provided: List[Field],
      requirements: List[Field]
    ): Either[PlanningFailure, List[FieldCandidate]] =
      if (requirements.isEmpty) Right(List(FieldCandidate(field.copy(fields = Nil), Nil, Nil, Nil)))
      else
        planFieldCandidates(
          field.copy(fields = requirements),
          scope.copy(activeContexts = Nil),
          Set(scope.currentSubgraph),
          carriedKeys,
          provided,
          Set.empty
        )

    private def addTypenameSelection(
      field: Field,
      currentSubgraph: String,
      path: Vector[String],
      parentType: __Type,
      planned: FieldCandidate
    ): FieldCandidate =
      if (
        isAbstractType(parentType) && !parentType.name.exists(graph.isInterfaceObject(currentSubgraph, _)) &&
        (planned.downstream.fields.isEmpty || planned.downstream.fields.exists(_.targets.nonEmpty))
      ) {
        val (typename, selection) =
          runtimeTypename(path, parentType, responseNames(field.fields) ++ responseNames(planned.downstream.fields))
        planned.copy(
          downstream = planned.downstream.copy(fields = planned.downstream.fields ::: typename :: Nil),
          typenameSelections = selection :: planned.typenameSelections
        )
      } else planned

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
      parentType: __Type,
      selected: List[Field],
      selection: LookupSelection,
      targets: Option[Set[String]],
      staticType: Boolean
    ): InjectedKeyFields = {
      val (keyFields, keys)                 = selection match {
        case LookupSelection.InjectedKeys(_, fields) => fields                       -> List.empty[RequiredSelection]
        case LookupSelection.SelectedKeys(_, fields) => List.empty[RequiredKeyField] -> fields
      }
      val usedNames                         = responseNames(field.fields) ++ responseNames(selected)
      val aliases                           = new PrivateAliases(usedNames)
      val injectedFields                    = mutable.ListBuffer.empty[Field]
      val injectedKeys                      = keyFields.map { keyField =>
        val candidate = requiredField(keyField, parentType, KeyAliasBase).copy(targets = targets)
        sharedInjectedField(selected, KeyAliasBase, candidate) match {
          case Some(existing) => requiredSelection(keyField, existing.aliasedName)
          case None           =>
            val alias = aliases.next(KeyAliasBase)
            injectedFields += candidate.copy(alias = Some(alias))
            requiredSelection(keyField, alias)
        }
      }
      val selections                        = keys ::: injectedKeys
      val requiresTypename                  = (selection.lookup.operation.requiresTypename && !staticType) || targets.nonEmpty
      val (typenameAlias, injectedTypename) =
        if (!requiresTypename) (None, None)
        else {
          val candidate = privateTypename(TypenameAliasBase, parentType, usedNames)
          sharedInjectedField(selected, TypenameAliasBase, candidate) match {
            case Some(existing) => (Some(existing.aliasedName), None)
            case None           => (Some(candidate.aliasedName), Some(candidate))
          }
        }
      val typename                          = typenameAlias.map(RequiredSelection(TypenameField, _))
      InjectedKeyFields(selected ::: injectedFields.toList ::: injectedTypename.toList, selections, typename)
    }

    private def composedFieldType(field: Field): Option[__Type] =
      field.parentType.flatMap(fieldDefinition(_, field.name)).map(_._type.innerType)

    private def isObjectField(currentSubgraph: String, field: Field): Boolean =
      composedFieldType(field).exists { composed =>
        composed.kind == __TypeKind.OBJECT &&
        field.parentType.flatMap(_.name).flatMap(graph.sourceField(currentSubgraph, _, field.name)).exists { declared =>
          val fieldType = declared._type.innerType
          fieldType.kind == __TypeKind.OBJECT && fieldType.name == composed.name
        }
      }

    private def isStaticEntityType(context: EntityFetchContext, entityType: String): Boolean =
      context.scope.staticPath && composedFieldType(context.field).exists(_.name.contains(entityType))

    private def sharedInjectedField(selected: List[Field], base: String, candidate: Field): Option[Field] =
      selected.find(existing => existing.aliasedName.startsWith(base) && covers(existing, candidate))

    private def covers(existing: Field, field: Field): Boolean =
      existing.name == field.name && existing.targets == field.targets &&
        existing.fragment.forall(_.directives.isEmpty) &&
        existing.copy(alias = None).toSelection == field.copy(alias = None).toSelection

    private def privateTypename(base: String, parentType: __Type, used: Set[String]): Field =
      Field(TypenameField, Types.string, Some(parentType), alias = Some(PrivateAliases.privateAlias(base, used)))

    private def runtimeTypename(
      path: Vector[String],
      parentType: __Type,
      used: Set[String]
    ): (Field, TypenameSelection) = {
      val typename = privateTypename(RuntimeTypenameAliasBase, parentType, used)
      typename -> TypenameSelection(path, typename.aliasedName)
    }

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
      typeName: String,
      currentSubgraph: String,
      targetSubgraph: String,
      carriedKeys: List[ComposedGraph.KeyField]
    ): List[(ComposedGraph.EntityLookup, List[RequiredKeyField])] =
      graph
        .entityLookups(targetSubgraph, typeName)
        .flatMap(lookup => requiredKeyFields(parentType, currentSubgraph, lookup.key, carriedKeys).map(lookup -> _))
        .sortBy { case (lookup, fields) => (if (fields.forall(_.fullyOwned)) 0 else 1, lookupSignature(lookup)) }

    private def lookupSignature(lookup: ComposedGraph.EntityLookup): String = {
      def keys(values: List[ComposedGraph.KeyField]): String =
        values.map(value => s"${value.name}{${keys(value.children)}}").mkString(",")

      val operation = lookup.operation match {
        case ComposedGraph.LookupOperation.FederationEntities  => EntitiesField
        case value: ComposedGraph.LookupOperation.GraphQLQuery => value.field
      }
      s"$operation:${keys(lookup.key)}"
    }

    private def requiredKeyFields(
      parentType: __Type,
      currentSubgraph: String,
      keys: List[ComposedGraph.KeyField],
      carriedKeys: List[ComposedGraph.KeyField]
    ): Option[List[RequiredKeyField]] =
      traverseOption(keys)(requiredKeyField(parentType, currentSubgraph, _, carriedKeys))

    private def requiredKeyField(
      parentType: __Type,
      currentSubgraph: String,
      key: ComposedGraph.KeyField,
      carriedKeys: List[ComposedGraph.KeyField]
    ): Option[RequiredKeyField] =
      for {
        typeName <- parentType.name
        field    <- graph.sourceField(currentSubgraph, typeName, key.name)
        carried   = carriedKeys.find(_.name == key.name)
        owned     = graph.ownsField(currentSubgraph, typeName, key.name)
        if owned || carried.nonEmpty
        children <-
          requiredKeyFields(field._type.innerType, currentSubgraph, key.children, carried.toList.flatMap(_.children))
      } yield RequiredKeyField(key.name, field, children, owned)

    private def availableKeys(currentSubgraph: String, tpe: __Type): List[ComposedGraph.KeyField] =
      tpe.name.toList
        .flatMap(typeName =>
          graph
            .entityLookups(currentSubgraph, typeName)
            .flatMap(_.key)
            .filter(graph.definesKeyField(currentSubgraph, typeName, _))
        )
        .distinct

    private def requiredSelection(field: RequiredKeyField, responseName: String): RequiredSelection =
      RequiredSelection(field.name, responseName, field.children.map(child => requiredSelection(child, child.name)))

    private def requiredField(field: RequiredKeyField, parentType: __Type, responseName: String): Field =
      Field(
        field.name,
        field.field._type,
        Some(parentType),
        fields = field.children.map(child => requiredField(child, field.field._type.innerType, child.name)),
        alias = Some(responseName)
      )

    private def intermediateSubgraphs(typeName: String, currentSubgraph: String, targetSubgraph: String): List[String] =
      graph
        .entityLookups(targetSubgraph, typeName)
        .iterator
        .flatMap(lookup => graph.sourcesDefiningKey(typeName, lookup.key).iterator)
        .filter(candidate => candidate != currentSubgraph && candidate != targetSubgraph)
        .toList
        .distinct
        .sorted

    private def groupPending(values: List[PendingFetch]): List[PendingFetch] = {
      val grouped  = mutable.LinkedHashMap.empty[(String, List[Selection]), mutable.ListBuffer[Field]]
      values.foreach(value =>
        grouped.getOrElseUpdate(value.targetSubgraph -> value.requirements, mutable.ListBuffer.empty) ++= value.fields
      )
      val pending  = grouped.iterator.map { case ((source, requirements), fields) =>
        PendingFetch(source, mergeFields(fields.toList), requirements)
      }.toList
      val bySource = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[PendingFetch]]
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
        (entity.keys ::: entity.requirements).map(selectionCount).sum +
          entity.contextArguments.flatMap(_.projection.paths).map(_.size).sum + entity.typename.size
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

  private def operationRootName(operation: OperationType): String =
    operation match {
      case OperationType.Query        => "Query"
      case OperationType.Mutation     => "Mutation"
      case OperationType.Subscription => "Subscription"
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
    rootCandidates: List[RootCandidate],
    roots: List[RootFetch],
    entities: List[EntityFetch],
    cost: PlanCost
  )

  private final case class RequiredKeyField(
    name: String,
    field: __Field,
    children: List[RequiredKeyField],
    owned: Boolean
  ) {
    def fullyOwned: Boolean = owned && children.forall(_.fullyOwned)
  }

  /**
   * Fields still needing an entity fetch, together with their target subgraph and @requires prerequisites.
   * A nested selection may remain pending until an ancestor provides a usable entity lookup.
   */
  private final case class PendingFetch(targetSubgraph: String, fields: List[Field], requirements: List[Selection])

  private sealed trait SourceCandidate

  private object SourceCandidate {
    final case class Local(requirements: List[Selection])                    extends SourceCandidate
    final case class Remote(subgraph: String, requirements: List[Selection]) extends SourceCandidate
  }

  private final case class FieldRouting(field: Field, supplied: Option[Field], candidates: List[SourceCandidate])

  private final case class EntityFetchKey(
    currentSubgraph: String,
    targetSubgraph: String,
    entityType: String,
    fields: List[String]
  ) {
    def render: String = s"$currentSubgraph -> $targetSubgraph for $entityType(${fields.mkString(",")})"
  }

  private final case class FieldPlanningScope(
    currentSubgraph: String,
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

  private final case class ResolvedLookup(entityType: String, parentType: __Type, selection: LookupSelection)

  private final case class EntityLookupCandidates(lookupTypes: List[(String, __Type)], lookups: List[ResolvedLookup])

  private final case class EntityFetchState(
    downstream: List[Field],
    entities: List[EntityCandidate],
    pending: List[PendingFetch],
    typenameSelections: List[TypenameSelection]
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

  private final case class FieldCandidate(
    downstream: Field,
    entities: List[EntityCandidate],
    pending: List[PendingFetch],
    typenameSelections: List[TypenameSelection]
  )

  private final case class RootCandidate(
    source: String,
    client: Field,
    downstream: Field,
    contextRoots: List[Field],
    entities: List[EntityCandidate],
    typenameSelections: List[TypenameSelection]
  )

  private final case class EntityCandidate(
    source: String,
    mergePath: Vector[String],
    entityType: String,
    keys: List[RequiredSelection],
    requirements: List[RequiredSelection],
    contextArguments: List[ContextualArgument],
    typename: Option[RequiredSelection],
    lookup: ComposedGraph.EntityLookup,
    fields: List[Field],
    entities: List[EntityCandidate],
    mayNeedPrerequisiteFetches: Boolean
  ) {
    def toFetch(id: FetchId, root: FetchId, dependencies: Set[FetchId]): EntityFetch =
      EntityFetch(
        id,
        root,
        source,
        dependencies,
        mergePath,
        entityType,
        keys,
        requirements,
        contextArguments,
        typename,
        lookup,
        fields
      )
  }

  private final case class ContextBinding(parentType: String, field: String, argument: ComposedGraph.ContextArgument) {
    def fieldArgument: FieldArgument = FieldArgument(parentType, field, argument.argument)
  }

  private final case class InjectedKeyFields(
    downstream: List[Field],
    keys: List[RequiredSelection],
    typename: Option[RequiredSelection]
  )

  private final case class ActiveContext(source: String, argument: ContextualArgument) {
    def matches(fetchSource: String, binding: ContextBinding): Boolean =
      source == fetchSource && argument.context == binding.argument.context &&
        argument.fieldArgument == binding.fieldArgument
  }

  private final val KeyAliasBase                 = "_caliban_gateway_key"
  private final val TypenameAliasBase            = "_caliban_gateway_typename"
  private final val RuntimeTypenameAliasBase     = "_caliban_gateway_runtime_typename"
  private final val ContextTypenameAliasBase     = "_caliban_gateway_context_typename"
  private final val RequirementTypenameAliasBase = "_caliban_gateway_requirement_typename"
  private final val RequirementAliasPrefix       = "_caliban_gateway_requirement_"

}
