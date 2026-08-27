package caliban.gateway.internal.planning

import caliban.execution.{ isMetaField, ExecutionRequest, Field }
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.planning.CandidateSearch._
import caliban.gateway.internal.planning.OperationPlan
import caliban.gateway.internal.planning.OperationPlan._
import caliban.gateway.internal.planning.OperationPlanner._
import caliban.gateway.traverseOption
import caliban.introspection.adt.{ __Field, __Type, __TypeKind }
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection }
import caliban.parsing.SourceMapper
import caliban.schema.Types

import scala.collection.mutable

private[gateway] final class OperationPlanner(
  graph: ComposedGraph,
  subgraphCount: Int,
  limits: CandidateSearch.Limits
) {

  def plan(document: Document, execution: ExecutionRequest): Either[PlanningFailure, OperationPlan] = {
    implicit val search: CandidateSearch = new CandidateSearch(limits)
    val rootName                         = operationRootName(execution.operationType)
    val fields                           = execution.field.collectFields(rootName)
    val (localFields, subgraphFields)    = fields.partition(isMetaField)

    for {
      _                  <- search.check
      candidate          <- planRoots(subgraphFields, execution.operationType)
      typenameSelections  = collectTypenameSelections(candidate.roots, candidate.entities)
      passthroughSubgraph =
        findPassthroughSubgraph(candidate.fetches, candidate.entities, typenameSelections, localFields)
      _                  <- Either.cond(
                              passthroughSubgraph.nonEmpty || !document.hasDirective(execution.operationName)(isCustomDirective),
                              (),
                              PlanningFailure("Custom executable directives are not supported by this gateway.")
                            )
      _                  <- search.check
    } yield OperationPlan(
      execution.operationType,
      rootName,
      fields,
      localFields,
      candidate.fetches,
      candidate.entities,
      typenameSelections,
      passthroughSubgraph
    )
  }

  private def planRoots(
    fields: List[Field],
    operationType: OperationType
  )(implicit search: CandidateSearch): Either[PlanningFailure, PlanCandidate] =
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
            val planned  = rootFetches(roots, operationType)
            val entities = addFetchDependencies(entityFetches(planned.assignments, planned.fetches.size))
            validateDependencies(entities).map(_ => PlanCandidate(roots, planned.fetches, entities))
          }
          .map(_.minBy(planCost))
      )

  private def planRootOptions(
    field: Field,
    operationType: OperationType
  )(implicit search: CandidateSearch): Either[PlanningFailure, List[List[PlannedRoot]]] = {
    val subgraphs = graph.sources(operationType, field.name)
    for {
      _       <- Either.cond(subgraphs.nonEmpty, (), PlanningFailure(s"No subgraph owns root field '${field.name}'."))
      options <-
        if (subgraphs.size == 1)
          planRootsAtSubgraph(field, field, subgraphs.head, subgraphs, addTypenameFallback = true).flatMap {
            case roots if roots.nonEmpty => Right(roots.map(List(_)))
            case _                       => Left(PlanningFailure(s"No subgraph can execute root field '${field.name}'."))
          }
        else {
          val strategies =
            if (operationType == OperationType.Mutation) subgraphs.map(RootStrategy.Single.apply)
            else subgraphs.map(RootStrategy.Single.apply) ::: RootStrategy.Split :: Nil
          search
            .evaluate(strategies) {
              case RootStrategy.Single(subgraph) =>
                planRootsAtSubgraph(field, field, subgraph, subgraphs, addTypenameFallback = true).flatMap {
                  case roots if roots.nonEmpty => Right(roots.map(List(_)))
                  case _                       =>
                    Left(PlanningFailure(s"Subgraph '$subgraph' has no executable work for '${field.name}'."))
                }
              case RootStrategy.Split            =>
                subgraphs
                  .foldLeft[Either[PlanningFailure, List[List[PlannedRoot]]]](Right(List(Nil))) {
                    case (result, subgraph) =>
                      for {
                        roots    <- result
                        selected  = rootFieldForSubgraph(field, subgraph, subgraphs)
                        planned  <-
                          planRootsAtSubgraph(field, selected, subgraph, subgraphs, addTypenameFallback = false)
                        combined <-
                          if (planned.isEmpty) Right(roots)
                          else search.combine(roots, planned)((current, root) => current :+ root)
                      } yield combined
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

  private def rootFetches(roots: List[PlannedRoot], operationType: OperationType): PlannedRootFetches =
    if (operationType == OperationType.Mutation) {
      val fetches = roots.zipWithIndex.map { case (root, index) =>
        RootFetch(FetchId(index), root.source, root.client :: Nil, root.downstream :: Nil)
      }
      PlannedRootFetches(fetches, roots.zip(fetches).map { case (root, fetch) => root -> fetch.id })
    } else {
      val grouped  = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[PlannedRoot]]
      roots.foreach(root => grouped.getOrElseUpdate(root.source, mutable.ListBuffer.empty) += root)
      val fetches  = grouped.iterator.zipWithIndex.map { case ((subgraph, planned), index) =>
        val selected = planned.toList
        RootFetch(FetchId(index), subgraph, selected.map(_.client), selected.map(_.downstream))
      }.toList
      val bySource = fetches.iterator.map(fetch => fetch.source -> fetch.id).toMap
      PlannedRootFetches(fetches, roots.flatMap(root => bySource.get(root.source).map(root -> _)))
    }

  private def entityFetches(assignments: List[(PlannedRoot, FetchId)], firstId: Int): List[EntityFetch] = {
    var nextFetchId = firstId

    def flatten(values: List[PlannedEntity], root: FetchId, dependencies: Set[FetchId]): List[EntityFetch] =
      values.flatMap { entity =>
        val id       = FetchId(nextFetchId)
        nextFetchId += 1
        val current  = entity.toFetch(id, root, dependencies)
        val children = flatten(entity.entities, root, Set(id))
        current :: children
      }

    assignments.flatMap { case (planned, root) => flatten(planned.entities, root, Set(root)) }
  }

  /**
   * Adds dependencies on fetches that supply entity keys or fields needed by @requires.
   */
  private def addFetchDependencies(fetches: List[EntityFetch]): List[EntityFetch] = {
    val byId = fetches.iterator.map(fetch => fetch.id -> fetch).toMap

    def dependsOn(fetch: EntityFetch, dependency: FetchId, seen: Set[FetchId]): Boolean =
      fetch.dependencies.contains(dependency) || fetch.dependencies.exists { id =>
        !seen.contains(id) && byId.get(id).exists(dependsOn(_, dependency, seen + id))
      }
    def selectionPaths(selection: RequiredSelection): List[Vector[String]]              =
      if (selection.children.isEmpty) Vector(selection.responseName) :: Nil
      else selection.children.flatMap(selectionPaths).map(Vector(selection.responseName) ++ _)
    def fieldPaths(field: Field): List[Vector[String]]                                  =
      if (field.fields.isEmpty) Vector(field.aliasedName) :: Nil
      else field.fields.flatMap(fieldPaths).map(Vector(field.aliasedName) ++ _)

    fetches.map { fetch =>
      if (!fetch.mayNeedPrerequisiteFetches) fetch
      else {
        val required     = (fetch.keys ::: fetch.requirements).flatMap(selectionPaths).map(fetch.mergePath ++ _).toSet
        val dependencies = fetches.iterator
          .filter(_.root == fetch.root)
          .filterNot(candidate => dependsOn(candidate, fetch.id, Set.empty))
          .filter(candidate =>
            candidate.fields.flatMap(fieldPaths).exists(path => required.contains(candidate.mergePath ++ path))
          )
          .map(_.id)
          .toSet
        fetch.copy(dependencies = fetch.dependencies ++ dependencies)
      }
    }
  }

  private def collectTypenameSelections(
    roots: List[PlannedRoot],
    entities: List[EntityFetch]
  ): List[TypenameSelection] =
    (roots.flatMap(_.typenameSelections) ::: entities.flatMap(fetch =>
      fetch.typename
        .filter(_ => graph.isObjectType(fetch.entityType))
        .map(selection => TypenameSelection(fetch.mergePath, selection.responseName))
    )).distinct

  /**
   * Allows the original request to be forwarded unchanged when no gateway-side rewriting or merging is needed.
   */
  private def findPassthroughSubgraph(
    fetches: List[RootFetch],
    entities: List[EntityFetch],
    typenameSelections: List[TypenameSelection],
    localFields: List[Field]
  ): Option[String] =
    fetches match {
      case fetch :: Nil
          if subgraphCount == 1 && entities.isEmpty && typenameSelections.isEmpty && localFields.isEmpty &&
            graph.mapping(fetch.source).forall(!_.nonEmpty) =>
        Some(fetch.source)
      case _ => None
    }

  private def planRootsAtSubgraph(
    client: Field,
    selected: Field,
    currentSubgraph: String,
    rootSubgraphs: List[String],
    addTypenameFallback: Boolean
  )(implicit search: CandidateSearch): Either[PlanningFailure, List[PlannedRoot]] =
    planFieldCandidates(
      selected,
      currentSubgraph,
      Vector(client.aliasedName),
      Set.empty,
      rootSubgraphs.toSet,
      availableKeys(currentSubgraph, selected.fieldType.innerType),
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
            List(PlannedRoot(currentSubgraph, client, planned.downstream, planned.entities, planned.typenameSelections))
          else if (
            addTypenameFallback && (selected.fieldType.innerType.kind match {
              case __TypeKind.INTERFACE | __TypeKind.UNION => true
              case _                                       => false
            })
          ) {
            val (alias, typename) = privateTypename(
              "_caliban_gateway_runtime_typename",
              selected.fieldType.innerType,
              selected.fields.iterator.map(_.aliasedName).toSet
            )
            val downstream        = planned.downstream.copy(fields = typename :: Nil)
            List(
              PlannedRoot(
                currentSubgraph,
                client,
                downstream,
                planned.entities,
                TypenameSelection(Vector(client.aliasedName), alias) :: planned.typenameSelections
              )
            )
          } else Nil
        })
    }

  private def rootFieldForSubgraph(field: Field, currentSubgraph: String, rootSubgraphs: List[String]): Field = {
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
          child.name == "__typename" && candidates.contains(currentSubgraph) ||
            (if (child.fields.nonEmpty) children.nonEmpty
             else
               supplied.nonEmpty || owners.contains(currentSubgraph) || owners.isEmpty && candidates.headOption
                 .contains(currentSubgraph))

        if (include) child.copy(fields = children) :: Nil else Nil
      }
    }

    val rootType = field.parentType.flatMap(_.name).getOrElse("")
    val provided = fieldSetFields(graph.provided(currentSubgraph, rootType, field.name), field.fieldType)
    field.copy(fields = filter(field.fieldType.innerType, field.fields, rootSubgraphs, provided))
  }

  private def hasRootWork(plan: PlannedField): Boolean =
    plan.downstream.fieldType.innerType.kind match {
      case __TypeKind.OBJECT | __TypeKind.INTERFACE | __TypeKind.UNION =>
        plan.downstream.fields.nonEmpty || plan.entities.nonEmpty
      case _                                                           => true
    }

  private def validateDependencies(fetches: List[EntityFetch]): Either[PlanningFailure, Unit] = {
    val fetchById = fetches.iterator.map(fetch => fetch.id -> fetch).toMap

    def visit(
      id: FetchId,
      visiting: Set[FetchId],
      visited: Set[FetchId]
    ): Either[PlanningFailure, Set[FetchId]] =
      if (visited.contains(id)) Right(visited)
      else if (visiting.contains(id)) Left(PlanningFailure("Entity routing dependency cycle detected."))
      else
        fetchById.get(id) match {
          case None        => Right(visited)
          case Some(fetch) =>
            fetch.dependencies.toList
              .sortBy(_.value)
              .foldLeft[Either[PlanningFailure, Set[FetchId]]](Right(visited)) { case (result, dependency) =>
                result.flatMap(visit(dependency, visiting + id, _))
              }
              .map(_ + id)
        }

    fetches
      .sortBy(_.id.value)
      .foldLeft[Either[PlanningFailure, Set[FetchId]]](Right(Set.empty)) { case (result, fetch) =>
        result.flatMap(visit(fetch.id, Set.empty, _))
      }
      .map(_ => ())
  }

  private def planFieldCandidates(
    field: Field,
    currentSubgraph: String,
    path: Vector[String],
    visitedFetches: Set[EntityFetchKey],
    runtimeSources: Set[String],
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field],
    satisfiedRequirements: Set[(String, String)]
  )(implicit search: CandidateSearch): Either[PlanningFailure, List[PlannedField]] = {
    val parentType       = field.fieldType.innerType
    val typeName         = parentType.name.getOrElse("")
    val fieldParentType  = field.parentType.flatMap(_.name)
    val subgraphTypeName = fieldParentType
      .flatMap(graph.field(currentSubgraph, _, field.name))
      .flatMap(_._type.innerType.name)
      .getOrElse(typeName)
    val possibleTypes    = fieldParentType
      .map(graph.runtimeTypesForField(runtimeSources, currentSubgraph, _, field.name, subgraphTypeName))
      .getOrElse(graph.runtimeTypes(currentSubgraph, subgraphTypeName).toSet)
    val providedFields   = mergeFields(
      provided ::: fieldSetFields(
        graph.provided(currentSubgraph, fieldParentType.getOrElse(""), field.name),
        field.fieldType
      )
    )
    val selections       = selectedFields(field, parentType, typeName)
      .filter(graph.appliesOnSource(currentSubgraph, subgraphTypeName, _))
      .filter(child =>
        graph.isInterfaceObject(currentSubgraph, subgraphTypeName) ||
          child._condition.forall(condition => possibleTypes.isEmpty || condition.exists(possibleTypes))
      )
    val typenameField    =
      if (
        graph.isInterfaceObject(currentSubgraph, typeName) && selections.exists(_.targets.nonEmpty) &&
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
    val routed           = selections ::: typenameField.toList.map(_._2)

    val routedSelections = findFieldProviders(
      routed,
      currentSubgraph,
      parentType,
      typeName,
      possibleTypes,
      providedFields,
      satisfiedRequirements
    )

    for {
      _           <- routedSelections.find(_.providers.isEmpty) match {
                       case Some(value) =>
                         Left(PlanningFailure(s"No subgraph owns field '$typeName.${value.field.name}'."))
                       case None        => Right(())
                     }
      assignments <- providerAssignments(routedSelections)
      context      = EntityFetchContext(
                       field,
                       currentSubgraph,
                       path,
                       parentType,
                       typeName,
                       visitedFetches,
                       availableExternal,
                       providedFields
                     )
      planned     <- search.evaluate(assignments) { assignment =>
                       val sameSubgraphFields = mutable.ListBuffer.empty[(Field, List[Field])]
                       val entityFetchFields  =
                         mutable.LinkedHashMap.empty[(String, List[Selection]), mutable.ListBuffer[Field]]
                       assignment.foreach { case (selection, provider) =>
                         val requirements = graph.required(provider.subgraph, provider.typeName, selection.field.name)
                         if (
                           canResolveInSubgraph(
                             provider,
                             currentSubgraph,
                             selection.field.name,
                             requirements,
                             satisfiedRequirements
                           )
                         )
                           sameSubgraphFields += selection.field -> selection.supplied.toList.flatMap(_.fields)
                         else
                           entityFetchFields.getOrElseUpdate(
                             provider.subgraph -> requirements,
                             mutable.ListBuffer.empty
                           ) += selection.field
                       }
                       val pending            = entityFetchFields.iterator.map { case ((targetSubgraph, requirements), fields) =>
                         PendingFetch(targetSubgraph, fields.toList, requirements)
                       }.toList
                       for {
                         sameSubgraphPlans <- planSameSubgraphFields(
                                                currentSubgraph,
                                                path,
                                                visitedFetches,
                                                runtimeSources,
                                                sameSubgraphFields.toList
                                              )
                         values            <- search.evaluate(sameSubgraphPlans)(planEntityFetches(context, _, pending))
                       } yield values.flatten.map { value =>
                         val withTypename = addTypenameSelection(field, currentSubgraph, path, parentType, value)
                         typenameField.fold(withTypename) { case (alias, _) =>
                           withTypename.copy(
                             typenameSelections = TypenameSelection(path, alias) :: withTypename.typenameSelections
                           )
                         }
                       }
                     }
    } yield planned.flatten
  }

  private def findFieldProviders(
    fields: List[Field],
    currentSubgraph: String,
    parentType: __Type,
    typeName: String,
    possibleTypes: Set[String],
    providedFields: List[Field],
    satisfiedRequirements: Set[(String, String)]
  ): List[RoutedSelection] =
    fields.flatMap { child =>
      val childParent                                   = child.parentType.flatMap(_.name).getOrElse(typeName)
      val supplied                                      = providedFields.find(candidate => providedFieldCovers(candidate, child))
      def providers(owner: String): List[FieldProvider] = {
        val candidates =
          if (child.name == "__typename" && graph.isInterfaceObject(currentSubgraph, typeName))
            graph.runtimeTypeSource(typeName, currentSubgraph).toList.map(FieldProvider(_, typeName))
          else if (child.name == "__typename") List(FieldProvider(currentSubgraph, typeName))
          else
            supplied
              .map(_ => List(FieldProvider(currentSubgraph, owner)))
              .getOrElse {
                List(
                  graph.fieldSources(owner, child.name, currentSubgraph).map(FieldProvider(_, owner)),
                  if (owner == childParent) Nil
                  else graph.fieldSources(childParent, child.name, currentSubgraph).map(FieldProvider(_, childParent)),
                  if (childParent == typeName) Nil
                  else graph.fieldSources(typeName, child.name, currentSubgraph).map(FieldProvider(_, typeName))
                ).find(_.nonEmpty).getOrElse(Nil)
              }
        candidates.find { provider =>
          val requirements = graph.required(provider.subgraph, provider.typeName, child.name)
          canResolveInSubgraph(provider, currentSubgraph, child.name, requirements, satisfiedRequirements)
        }
          .fold(candidates)(List(_))
      }

      val directProviders      = providers(childParent)
      val conditionalProviders =
        if (
          child.name != "__typename" && !graph.isInterfaceObject(currentSubgraph, typeName) &&
          (childParent == typeName || directProviders.isEmpty) &&
          (parentType.kind match {
            case __TypeKind.INTERFACE | __TypeKind.UNION => true
            case _                                       => false
          })
        )
          child._condition
            .fold(possibleTypes)(condition =>
              if (possibleTypes.isEmpty) condition else condition intersect possibleTypes
            )
            .flatMap { condition =>
              val values = providers(condition)
              if (values.nonEmpty) Some(condition -> values) else None
            }
            .toList
            .sortBy(_._1)
        else Nil

      if (conditionalProviders.isEmpty) RoutedSelection(child, supplied, directProviders) :: Nil
      else
        conditionalProviders.map { case (condition, values) =>
          RoutedSelection(
            child.copy(_condition = Some(Set(condition)), targets = Some(Set(condition))),
            supplied,
            values
          )
        }
    }

  private def providerAssignments(
    selections: List[RoutedSelection]
  )(implicit search: CandidateSearch): Either[PlanningFailure, List[List[(RoutedSelection, FieldProvider)]]] =
    selections.foldLeft[Either[PlanningFailure, List[List[(RoutedSelection, FieldProvider)]]]](Right(List(Nil))) {
      case (result, selection) =>
        for {
          current  <- result
          combined <- search.combine(current, selection.providers) { case (values, provider) =>
                        values ::: List(selection -> provider)
                      }
        } yield combined
    }

  private def planSameSubgraphFields(
    currentSubgraph: String,
    path: Vector[String],
    visitedFetches: Set[EntityFetchKey],
    runtimeSources: Set[String],
    fields: List[(Field, List[Field])]
  )(implicit search: CandidateSearch): Either[PlanningFailure, List[PlannedSelections]] =
    fields
      .foldLeft[Either[PlanningFailure, List[PlannedSelections]]](
        Right(List(PlannedSelections(Nil, Nil, Nil, Nil)))
      ) { case (result, (child, provided)) =>
        for {
          current      <- result
          alternatives <-
            if (current.isEmpty) Left(PlanningFailure("No complete route candidate was found."))
            else
              planFieldCandidates(
                child,
                currentSubgraph,
                path :+ child.aliasedName,
                visitedFetches,
                graph.runtimeSources(
                  runtimeSources,
                  child.parentType.flatMap(_.name).getOrElse(""),
                  child.name
                ),
                availableKeys(currentSubgraph, child.fieldType.innerType),
                provided,
                Set.empty
              )
          combined     <- search.combine(current, alternatives) { case (values, planned) =>
                            PlannedSelections(
                              planned.downstream :: values.downstream,
                              values.entities ::: planned.entities,
                              values.pending ::: wrapPending(child, planned.pending),
                              values.typenameSelections ::: planned.typenameSelections
                            )
                          }
        } yield combined
      }
      .map(_.map(value => value.copy(downstream = value.downstream.reverse)))

  private def planEntityFetches(
    context: EntityFetchContext,
    selected: PlannedSelections,
    pending: List[PendingFetch]
  )(implicit search: CandidateSearch): Either[PlanningFailure, List[PlannedField]] =
    planPendingEntityFetches(
      context,
      EntityFetchState(selected.downstream.toVector, selected.entities, Nil, selected.typenameSelections),
      groupPending(pending ::: selected.pending)
    ).map { states =>
      states.map { state =>
        PlannedField(
          context.field.copy(fields = mergeFields(state.downstream.toList)),
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
  )(implicit search: CandidateSearch): Either[PlanningFailure, List[EntityFetchState]] =
    pending.foldLeft[Either[PlanningFailure, List[EntityFetchState]]](Right(List(initial))) {
      case (result, candidate) =>
        result.flatMap(states =>
          search.evaluate(states)(planEntityFetchCandidates(context, _, candidate)).map(_.flatten)
        )
    }

  private def planEntityFetchCandidates(
    context: EntityFetchContext,
    state: EntityFetchState,
    candidate: PendingFetch
  )(implicit search: CandidateSearch): Either[PlanningFailure, List[EntityFetchState]] = {
    val resolution = resolveEntityLookups(context, candidate)
    val concrete   = resolution.lookups.filter(value => graph.isObjectType(value.entityType))
    val types      = concrete.map(_.entityType).distinct
    if (
      resolution.lookups.headOption.forall(_.entityType != context.typeName) &&
      types.size > 1
    ) {
      val resolved         = types.foldLeft[Either[PlanningFailure, List[EntityFetchState]]](Right(List(state))) {
        case (result, entityType) =>
          result.flatMap(states =>
            search
              .evaluate(states) { current =>
                val fields = candidate.fields.filter(_._condition.forall(_.contains(entityType)))
                if (fields.isEmpty) Right(List(current))
                else {
                  val selected = candidate.copy(
                    fields = fields,
                    requirements =
                      fields.flatMap(child => graph.required(candidate.targetSubgraph, entityType, child.name)).distinct
                  )
                  planLookupCandidates(context, current, selected, concrete.filter(_.entityType == entityType))
                }
              }
              .map(_.flatten)
          )
      }
      val unresolvedFields = candidate.fields.flatMap { field =>
        field._condition.flatMap { condition =>
          val unresolved = condition -- types
          if (unresolved.isEmpty) None else Some(field.copy(_condition = Some(unresolved)))
        }
      }
      if (unresolvedFields.isEmpty) resolved
      else
        resolved.flatMap { states =>
          val pending  = candidate.copy(fields = unresolvedFields)
          val fetchKey = EntityFetchKey(
            context.currentSubgraph,
            candidate.targetSubgraph,
            context.typeName,
            flatten(unresolvedFields)
          )
          search
            .evaluate(states)(
              planIndirectEntityFetches(context, _, pending, resolution.lookupTypes, fetchKey)
            )
            .map(_.flatten)
        }
    } else {
      val entityType = resolution.lookups.headOption.map(_.entityType).getOrElse(context.typeName)
      val fetchKey   =
        EntityFetchKey(context.currentSubgraph, candidate.targetSubgraph, entityType, flatten(candidate.fields))
      if (context.visitedFetches.contains(fetchKey))
        Left(PlanningFailure(s"Entity routing cycle detected: ${fetchKey.render}."))
      else if (resolution.lookups.nonEmpty) planLookupCandidates(context, state, candidate, resolution.lookups)
      else planIndirectEntityFetches(context, state, candidate, resolution.lookupTypes, fetchKey)
    }
  }

  private def planLookupCandidates(
    context: EntityFetchContext,
    state: EntityFetchState,
    candidate: PendingFetch,
    lookups: List[ResolvedLookup]
  )(implicit search: CandidateSearch): Either[PlanningFailure, List[EntityFetchState]] =
    search
      .evaluate(lookups.sortBy(lookupSignature)) { lookup =>
        val fetchKey = EntityFetchKey(
          context.currentSubgraph,
          candidate.targetSubgraph,
          lookup.entityType,
          flatten(candidate.fields)
        )
        if (context.visitedFetches.contains(fetchKey))
          Left(PlanningFailure(s"Entity routing cycle detected: ${fetchKey.render}."))
        else planEntityFetch(context, state, candidate, lookup, fetchKey)
      }
      .map(_.flatten)

  private def resolveEntityLookups(
    context: EntityFetchContext,
    candidate: PendingFetch
  ): EntityLookupCandidates = {
    val subgraphType  = context.field.parentType
      .flatMap(_.name)
      .flatMap(graph.field(context.currentSubgraph, _, context.field.name))
      .flatMap(_._type.innerType.name)
      .getOrElse(context.typeName)
    val subgraphTypes = graph.runtimeTypes(context.currentSubgraph, subgraphType)
    val knownTypes    =
      if (subgraphTypes.nonEmpty) subgraphTypes else graph.runtimeTypes(candidate.targetSubgraph, context.typeName)
    val conditions    = candidate.fields.iterator
      .flatMap(_._condition)
      .flatMap(_.iterator)
      .filter(name => subgraphTypes.isEmpty || subgraphTypes.contains(name))
    val runtimeTypes  = (conditions ++ knownTypes).filter(graph.isObjectType).toList.distinct.sorted
    val lookupTypes   = ((context.typeName, context.parentType) :: runtimeTypes
      .flatMap(name => graph.rootType.types.get(name).map(name -> _))).distinct
    val lookups       = lookupTypes.flatMap { case (entityType, entityParent) =>
      val selected = selectLookups(
        entityParent,
        entityType,
        context.currentSubgraph,
        candidate.targetSubgraph,
        context.availableExternal
      ).map { case (value, fields) => LookupSelection.InjectedKeys(value, fields) }
      val lookups  =
        if (selected.nonEmpty) selected
        else lookupsUsingSelectedKeys(context.field, entityParent, entityType, candidate.targetSubgraph)
      lookups.map(ResolvedLookup(entityType, entityParent, _))
    }
    EntityLookupCandidates(lookupTypes, lookups)
  }

  private def planEntityFetch(
    context: EntityFetchContext,
    state: EntityFetchState,
    candidate: PendingFetch,
    resolved: ResolvedLookup,
    fetchKey: EntityFetchKey
  )(implicit search: CandidateSearch): Either[PlanningFailure, List[EntityFetchState]] = {
    val entityField                    = context.field.copy(fieldType = resolved.parentType)
    val (requiredFields, requirements) =
      injectRequirementFields(
        entityField,
        state.downstream,
        fieldSetFields(candidate.requirements, resolved.parentType)
      )
    for {
      requirementPlans <- planRequirementCandidates(
                            entityField,
                            context.currentSubgraph,
                            context.path,
                            context.visitedFetches + fetchKey,
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
                                           candidate.targetSubgraph,
                                           context.path,
                                           context.visitedFetches + fetchKey,
                                           Set(candidate.targetSubgraph),
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
                                           val withPrerequisiteFields       = mergeFields(
                                             state.downstream.toList ::: requirementPlan.downstream.fields
                                           ).toVector
                                           val (downstream, keys, typename) = injectKeyFields(
                                             entityField,
                                             resolved.parentType,
                                             withPrerequisiteFields,
                                             resolved.selection,
                                             entityTypeCondition(context.parentType, resolved.entityType)
                                           )
                                           val entity                       = PlannedEntity(
                                             candidate.targetSubgraph,
                                             context.currentSubgraph,
                                             context.path,
                                             resolved.entityType,
                                             keys,
                                             requirements,
                                             typename,
                                             resolved.selection.lookup,
                                             value.downstream.fields,
                                             value.entities,
                                             resolved.selection.mayNeedPrerequisiteFetches ||
                                               requirementPlan.entities.nonEmpty
                                           )
                                           val next                         = EntityFetchState(
                                             downstream,
                                             state.entities ::: requirementPlan.entities ::: (entity :: Nil),
                                             state.pending,
                                             state.typenameSelections ::: requirementPlan.typenameSelections :::
                                               value.typenameSelections
                                           )
                                           planPendingEntityFetches(
                                             context.copy(
                                               field = entityField,
                                               parentType = resolved.parentType,
                                               typeName = resolved.entityType,
                                               visitedFetches = context.visitedFetches + fetchKey
                                             ),
                                             next,
                                             value.pending
                                           )
                                         }
                            } yield values.flatten
                          }
    } yield completed.flatten
  }

  /**
   * Tries intermediate subgraphs that can supply a key accepted by the target subgraph.
   * For example, one subgraph can resolve a Product by id and supply the sku required by another.
   */
  private def planIndirectEntityFetches(
    context: EntityFetchContext,
    state: EntityFetchState,
    candidate: PendingFetch,
    lookupTypes: List[(String, __Type)],
    fetchKey: EntityFetchKey
  )(implicit search: CandidateSearch): Either[PlanningFailure, List[EntityFetchState]] = {
    val candidates = lookupTypes.flatMap { case (candidateTypeName, _) =>
      intermediateSubgraphs(candidateTypeName, context.currentSubgraph, candidate.targetSubgraph)
    }.distinct
    if (candidates.isEmpty) Right(List(state.copy(pending = groupPending(state.pending ::: (candidate :: Nil)))))
    else
      search.evaluate(candidates)(next =>
        planEntityFetchCandidates(
          context.copy(visitedFetches = context.visitedFetches + fetchKey),
          state,
          PendingFetch(next, candidate.fields, Nil)
        ).flatMap { values =>
          val completed = values.filter(_.pending == state.pending)
          Either.cond(
            completed.nonEmpty,
            completed,
            PlanningFailure(s"Intermediate subgraph '$next' did not complete the entity fetch.")
          )
        }
      ) match {
        case Right(values)                      => Right(values.flatten)
        case Left(failure) if failure.exhausted => Left(failure)
        case Left(_)                            =>
          Right(List(state.copy(pending = groupPending(state.pending ::: (candidate :: Nil)))))
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
      .lookups(targetSubgraph, typeName)
      .flatMap(lookup =>
        selectedKeySelections(fields, parentType, lookup.key)
          .map(selected => LookupSelection.SelectedKeys(lookup, selected))
      )
  }

  private def selectedKeySelections(
    fields: List[Field],
    parentType: __Type,
    keys: List[ComposedGraph.KeyField]
  ): Option[List[RequiredSelection]] =
    traverseOption(keys)(selectedKeySelection(fields, parentType, _))

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

  private def canResolveInSubgraph(
    provider: FieldProvider,
    currentSubgraph: String,
    fieldName: String,
    requirements: List[Selection],
    satisfiedRequirements: Set[(String, String)]
  ): Boolean =
    provider.subgraph == currentSubgraph &&
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
          val base     = requirementAliasBase(requirement)
          val existing = (selected.iterator ++ fields.iterator).find { field =>
            field.aliasedName.startsWith(base) &&
            field.copy(alias = None).toSelection == requirement.copy(alias = None).toSelection
          }
          existing match {
            case Some(field) =>
              val (_, selection) = requirementSelection(field)
              (fields, selection :: selections, names)
            case None        =>
              val alias                = privateAlias(base, names)
              val (aliased, selection) = requirementSelection(requirement.copy(alias = Some(alias)))
              (aliased :: fields, selection :: selections, names + alias)
          }
      } match {
      case (fields, selections, _) => fields.reverse -> selections.reverse
    }
  }

  private def planRequirementCandidates(
    field: Field,
    currentSubgraph: String,
    path: Vector[String],
    visitedFetches: Set[EntityFetchKey],
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field],
    requirements: List[Field]
  )(implicit search: CandidateSearch): Either[PlanningFailure, List[PlannedField]] =
    if (requirements.isEmpty) Right(List(PlannedField(field.copy(fields = Nil), Nil, Nil, Nil)))
    else
      planFieldCandidates(
        field.copy(fields = requirements),
        currentSubgraph,
        path,
        visitedFetches,
        Set(currentSubgraph),
        availableExternal,
        provided,
        Set.empty
      )

  private def addTypenameSelection(
    field: Field,
    currentSubgraph: String,
    path: Vector[String],
    parentType: __Type,
    planned: PlannedField
  ): PlannedField =
    parentType.kind match {
      case __TypeKind.INTERFACE | __TypeKind.UNION
          if !parentType.name.exists(graph.isInterfaceObject(currentSubgraph, _)) &&
            (planned.downstream.fields.isEmpty || planned.downstream.fields.exists(_.targets.nonEmpty)) =>
        val used              =
          field.fields.iterator.map(_.aliasedName).toSet ++ planned.downstream.fields.iterator.map(_.aliasedName)
        val (alias, typename) = privateTypename("_caliban_gateway_runtime_typename", parentType, used)
        planned.copy(
          downstream = planned.downstream.copy(
            fields = planned.downstream.fields ::: typename :: Nil
          ),
          typenameSelections = TypenameSelection(path, alias) :: planned.typenameSelections
        )
      case _ => planned
    }

  private def requirementSelection(field: Field): (Field, RequiredSelection) = {
    val prepared         = field.fields.map(requirementSelection)
    val children         = prepared.map(_._1)
    val selections       = prepared.map(_._2)
    val needsRuntimeType = selections.exists(_.conditions.nonEmpty)
    val typenameField    =
      if (needsRuntimeType) {
        Some(
          privateTypename(
            "_caliban_gateway_requirement_typename",
            field.fieldType.innerType,
            children.iterator.map(_.aliasedName).toSet
          )
        )
      } else None
    val downstream       = field.copy(fields = children ::: typenameField.toList.map(_._2))
    downstream -> RequiredSelection(
      field.name,
      field.aliasedName,
      selections,
      field._condition.orElse(field.targets),
      typenameField.map(_._1)
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

  private def wrapPending(field: Field, pending: List[PendingFetch]): List[PendingFetch] =
    pending.map(value => value.copy(fields = field.copy(fields = value.fields) :: Nil))

  private def injectKeyFields(
    field: Field,
    parentType: __Type,
    selected: Vector[Field],
    selection: LookupSelection,
    targets: Option[Set[String]]
  ): (Vector[Field], List[RequiredSelection], Option[RequiredSelection]) = {
    val (keyFields, keys)                 = selection match {
      case LookupSelection.InjectedKeys(_, fields) => fields                       -> List.empty[RequiredSelection]
      case LookupSelection.SelectedKeys(_, fields) => List.empty[RequiredKeyField] -> fields
    }
    val usedNames                         = field.fields.iterator.map(_.aliasedName).toSet ++ selected.iterator.map(_.aliasedName)
    val (injected, injectedFields, names) = keyFields.foldLeft(
      (List.empty[RequiredSelection], Vector.empty[Field], usedNames)
    ) { case ((selections, fields, names), keyField) =>
      val alias = privateAlias("_caliban_gateway_key", names)
      (
        requiredSelection(keyField, alias) :: selections,
        fields :+ requiredField(keyField, parentType, alias).copy(targets = targets),
        names + alias
      )
    }
    val injectedKeys                      = injected.reverse
    val selections                        = keys ::: injectedKeys
    val typenameField                     =
      if (selection.lookup.operation.requiresTypename || targets.nonEmpty)
        Some(privateTypename("_caliban_gateway_typename", parentType, names))
      else None
    val typename                          = typenameField.map { case (alias, _) => RequiredSelection("__typename", alias) }
    (selected ++ injectedFields ++ typenameField.map(_._2), selections, typename)
  }

  private def privateTypename(base: String, parentType: __Type, used: Set[String]): (String, Field) = {
    val alias = privateAlias(base, used)
    alias -> Field("__typename", Types.string, Some(parentType), alias = Some(alias))
  }

  private def entityTypeCondition(parentType: __Type, entityType: String): Option[Set[String]] =
    parentType.kind match {
      case __TypeKind.INTERFACE | __TypeKind.UNION if graph.isObjectType(entityType) => Some(Set(entityType))
      case _                                                                         => None
    }

  private def selectLookups(
    parentType: __Type,
    typeName: String,
    currentSubgraph: String,
    targetSubgraph: String,
    availableExternal: List[ComposedGraph.KeyField]
  ): List[(ComposedGraph.EntityLookup, List[RequiredKeyField])] =
    graph
      .lookups(targetSubgraph, typeName)
      .flatMap(lookup =>
        requiredKeyFields(parentType, currentSubgraph, lookup.key, availableExternal).toOption.map(lookup -> _)
      )
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
    currentSubgraph: String,
    keys: List[ComposedGraph.KeyField],
    availableExternal: List[ComposedGraph.KeyField]
  ): Either[PlanningFailure, List[RequiredKeyField]] =
    keys
      .foldLeft[Either[PlanningFailure, List[RequiredKeyField]]](Right(Nil)) { case (result, key) =>
        for {
          fields <- result
          field  <- requiredKeyField(parentType, currentSubgraph, key, availableExternal)
        } yield field :: fields
      }
      .map(_.reverse)

  private def requiredKeyField(
    parentType: __Type,
    currentSubgraph: String,
    key: ComposedGraph.KeyField,
    availableExternal: List[ComposedGraph.KeyField]
  ): Either[PlanningFailure, RequiredKeyField] =
    for {
      typeName <- parentType.name.toRight(PlanningFailure("Entity key parent type has no name."))
      field    <- graph
                    .field(currentSubgraph, typeName, key.name)
                    .toRight(
                      PlanningFailure(s"Subgraph '$currentSubgraph' does not provide key field '$typeName.${key.name}'.")
                    )
      carried   = availableExternal.find(_.name == key.name)
      owned     = graph.owns(currentSubgraph, typeName, key.name)
      _        <- Either.cond(
                    owned || carried.nonEmpty,
                    (),
                    PlanningFailure(s"Subgraph '$currentSubgraph' does not provide key field '$typeName.${key.name}'.")
                  )
      children <-
        requiredKeyFields(field._type.innerType, currentSubgraph, key.children, carried.toList.flatMap(_.children))
    } yield RequiredKeyField(key.name, field, children, owned)

  private def availableKeys(currentSubgraph: String, tpe: __Type): List[ComposedGraph.KeyField] =
    tpe.name.toList
      .flatMap(typeName =>
        graph.lookups(currentSubgraph, typeName).flatMap(_.key).filter(declaredKey(currentSubgraph, tpe, _))
      )
      .distinct

  private def declaredKey(currentSubgraph: String, parentType: __Type, key: ComposedGraph.KeyField): Boolean =
    parentType.name.exists { typeName =>
      graph.declares(currentSubgraph, typeName, key.name) && graph.field(currentSubgraph, typeName, key.name).exists {
        field =>
          key.children.forall(declaredKey(currentSubgraph, field._type.innerType, _))
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

  private def intermediateSubgraphs(
    typeName: String,
    currentSubgraph: String,
    targetSubgraph: String
  ): List[String] =
    graph
      .lookups(targetSubgraph, typeName)
      .iterator
      .flatMap(lookup => graph.sourcesForKey(typeName, lookup.key).iterator)
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

  private def entityDepth(fetches: List[EntityFetch]): Int = {
    val byId = fetches.iterator.map(fetch => fetch.id -> fetch).toMap

    def depth(fetch: EntityFetch, visiting: Set[FetchId]): Int =
      if (visiting.contains(fetch.id)) 0
      else
        1 + fetch.dependencies.iterator
          .flatMap(byId.get)
          .map(depth(_, visiting + fetch.id))
          .foldLeft(0)(math.max)

    fetches.foldLeft(0)((value, fetch) => math.max(value, depth(fetch, Set.empty)))
  }

  private def selectionCount(selection: RequiredSelection): Int =
    1 + selection.children.foldLeft(0)((count, child) => count + selectionCount(child))

  private def internalSelectionCount(entities: List[EntityFetch]): Int =
    entities.foldLeft(0) { (count, entity) =>
      count + entity.keys.foldLeft(0)((value, key) => value + selectionCount(key)) +
        entity.requirements.foldLeft(0)((value, requirement) => value + selectionCount(requirement)) +
        entity.typename.size
    }

  private def planCost(candidate: PlanCandidate): PlanCost =
    PlanCost(
      candidate.fetches.size + OperationPlan.logicalCallCount(candidate.entities),
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

  private def unsatisfiedMessage(pending: List[PendingFetch]): String = {
    val obligations =
      pending.map(value => s"'${value.targetSubgraph}:${flatten(value.fields).mkString(",")}'").mkString(", ")
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

  /**
   * Lexicographic cost: calls first, then dependency depth, then internal selections.
   */
  private final case class PlanCost(logicalCalls: Int, dependencyDepth: Int, internalSelections: Int)
      extends Ordered[PlanCost] {
    def compare(that: PlanCost): Int =
      Ordering[(Int, Int, Int)].compare(
        (logicalCalls, dependencyDepth, internalSelections),
        (that.logicalCalls, that.dependencyDepth, that.internalSelections)
      )
  }

  private sealed trait RootStrategy
  private object RootStrategy {
    final case class Single(source: String) extends RootStrategy
    case object Split                       extends RootStrategy
  }

  private final case class PlannedRootFetches(
    fetches: List[RootFetch],
    assignments: List[(PlannedRoot, FetchId)]
  )

  private final case class PlanCandidate(
    roots: List[PlannedRoot],
    fetches: List[RootFetch],
    entities: List[EntityFetch]
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

  private final case class FieldProvider(subgraph: String, typeName: String)

  private final case class RoutedSelection(
    field: Field,
    supplied: Option[Field],
    providers: List[FieldProvider]
  )

  private final case class EntityFetchKey(
    currentSubgraph: String,
    targetSubgraph: String,
    entityType: String,
    fields: List[String]
  ) {
    def render: String = s"$currentSubgraph -> $targetSubgraph for $entityType(${fields.mkString(",")})"
  }

  private final case class EntityFetchContext(
    field: Field,
    currentSubgraph: String,
    path: Vector[String],
    parentType: __Type,
    typeName: String,
    visitedFetches: Set[EntityFetchKey],
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field]
  )

  private final case class ResolvedLookup(
    entityType: String,
    parentType: __Type,
    selection: LookupSelection
  )

  private final case class EntityLookupCandidates(
    lookupTypes: List[(String, __Type)],
    lookups: List[ResolvedLookup]
  )

  private final case class PlannedSelections(
    downstream: List[Field],
    entities: List[PlannedEntity],
    pending: List[PendingFetch],
    typenameSelections: List[TypenameSelection]
  )

  private final case class EntityFetchState(
    downstream: Vector[Field],
    entities: List[PlannedEntity],
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

  private final case class PlannedField(
    downstream: Field,
    entities: List[PlannedEntity],
    pending: List[PendingFetch],
    typenameSelections: List[TypenameSelection]
  )

  private final case class PlannedRoot(
    source: String,
    client: Field,
    downstream: Field,
    entities: List[PlannedEntity],
    typenameSelections: List[TypenameSelection]
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
    mayNeedPrerequisiteFetches: Boolean
  ) {
    def toFetch(id: FetchId, root: FetchId, dependencies: Set[FetchId]): EntityFetch =
      EntityFetch(
        id,
        root,
        source,
        dependencies,
        dependencySource,
        mergePath,
        entityType,
        keys,
        requirements,
        typename,
        lookup,
        fields,
        mayNeedPrerequisiteFetches
      )
  }

}
