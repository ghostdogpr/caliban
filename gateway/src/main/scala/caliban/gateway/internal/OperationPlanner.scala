package caliban.gateway.internal

import caliban.InputValue
import caliban.Value.NullValue
import caliban.execution.{ isIntrospectionField, isMetaField, ExecutionRequest, Field, Fragment }
import caliban.gateway.traverseOption
import caliban.gateway.internal.OperationPlanner._
import caliban.introspection.adt.{ __Field, __Type, __TypeKind }
import caliban.parsing.SourceMapper
import caliban.parsing.adt.{ Directive, Document, OperationType, Selection }
import caliban.rendering.DocumentRenderer
import caliban.schema.Types
import zio.Duration

import scala.collection.immutable.ListMap
import scala.collection.mutable

private[gateway] final class OperationPlanner(
  graph: ComposedGraph,
  subgraphCount: Int,
  limits: OperationPlanner.Limits
) {

  def plan(document: Document, execution: ExecutionRequest): Either[PlanningFailure, OperationPlan] = {
    implicit val search: PlanningSearch = new PlanningSearch(limits)
    val rootName                        = operationRootName(execution.operationType)
    val fields                          = execution.field.collectFields(rootName)
    val (localFields, subgraphFields)   = fields.partition(isMetaField)

    for {
      _                  <- search.check
      candidate          <- planRoots(subgraphFields, execution.operationType)
      typenameSelections  = collectTypenameSelections(candidate.roots, candidate.entities)
      passthroughSubgraph =
        findPassthroughSubgraph(candidate.planned.routes, candidate.entities, typenameSelections, localFields)
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
      candidate.planned.routes,
      candidate.entities,
      typenameSelections,
      passthroughSubgraph
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
            val entities = addFetchDependencies(entityRoutes(planned.assignments, planned.routes.size))
            validateDependencies(entities).map(_ => PlannedCandidate(roots, planned, entities))
          }
          .map(_.minBy(routeCost))
      )

  private def planRootOptions(
    field: Field,
    operationType: OperationType
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[List[PlannedRoot]]] = {
    val subgraphs = graph.sources(operationType, field.name)
    for {
      _       <- Either.cond(subgraphs.nonEmpty, (), PlanningFailure(s"No subgraph owns root field '${field.name}'."))
      options <-
        if (subgraphs.size == 1)
          planRootsAtSubgraph(field, field, subgraphs.head, subgraphs, true).flatMap {
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
                planRootsAtSubgraph(field, field, subgraph, subgraphs, true).flatMap {
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
                        planned  <- planRootsAtSubgraph(field, selected, subgraph, subgraphs, false)
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

  private def rootRoutes(roots: List[PlannedRoot], operationType: OperationType): PlannedRoutes =
    if (operationType == OperationType.Mutation) {
      val routes = roots.zipWithIndex.map { case (root, index) =>
        RootRoute(RouteId(index), root.source, root.client :: Nil, root.downstream :: Nil)
      }
      PlannedRoutes(routes, roots.zip(routes).map { case (root, route) => root -> route.id })
    } else {
      val grouped  = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[PlannedRoot]]
      roots.foreach(root => grouped.getOrElseUpdate(root.source, mutable.ListBuffer.empty) += root)
      val routes   = grouped.iterator.zipWithIndex.map { case ((subgraph, planned), index) =>
        val selected = planned.toList
        RootRoute(RouteId(index), subgraph, selected.map(_.client), selected.map(_.downstream))
      }.toList
      val bySource = routes.iterator.map(route => route.source -> route.id).toMap
      PlannedRoutes(routes, roots.flatMap(root => bySource.get(root.source).map(root -> _)))
    }

  private def entityRoutes(assignments: List[(PlannedRoot, RouteId)], firstId: Int): List[EntityRoute] = {
    var nextRouteId = firstId

    def flatten(values: List[PlannedEntity], root: RouteId, dependencies: Set[RouteId]): List[EntityRoute] =
      values.flatMap { entity =>
        val id       = RouteId(nextRouteId)
        nextRouteId += 1
        val current  = entity.toRoute(id, root, dependencies)
        val children = flatten(entity.entities, root, Set(id))
        current :: children
      }

    assignments.flatMap { case (planned, root) => flatten(planned.entities, root, Set(root)) }
  }

  /**
   * Adds dependencies on fetches that supply entity keys or fields needed by @requires.
   */
  private def addFetchDependencies(routes: List[EntityRoute]): List[EntityRoute] = {
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
      if (!route.mayNeedPrerequisiteFetches) route
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

  private def collectTypenameSelections(
    roots: List[PlannedRoot],
    entities: List[EntityRoute]
  ): List[TypenameSelection] =
    (roots.flatMap(_.typenameSelections) ::: entities.flatMap(route =>
      route.typename
        .filter(_ => graph.isObjectType(route.entityType))
        .map(selection => TypenameSelection(route.mergePath, selection.responseName))
    )).distinct

  /**
   * Allows the original request to be forwarded unchanged when no gateway-side rewriting or merging is needed.
   */
  private def findPassthroughSubgraph(
    routes: List[RootRoute],
    entities: List[EntityRoute],
    typenameSelections: List[TypenameSelection],
    localFields: List[Field]
  ): Option[String] =
    routes match {
      case route :: Nil
          if subgraphCount == 1 && entities.isEmpty && typenameSelections.isEmpty && localFields.isEmpty &&
            graph.mapping(route.source).forall(!_.nonEmpty) =>
        Some(route.source)
      case _ => None
    }

  private def planRootsAtSubgraph(
    client: Field,
    selected: Field,
    currentSubgraph: String,
    rootSubgraphs: List[String],
    addTypenameFallback: Boolean
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[PlannedRoot]] =
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
    currentSubgraph: String,
    path: Vector[String],
    visitedFetches: Set[EntityFetchKey],
    runtimeSources: Set[String],
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field],
    satisfiedRequirements: Set[(String, String)]
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[PlannedField]] = {
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
                       for {
                         sameSubgraphPlans <- planSameSubgraphFields(
                                                currentSubgraph,
                                                path,
                                                visitedFetches,
                                                runtimeSources,
                                                sameSubgraphFields.toList
                                              )
                         values            <- search.evaluate(sameSubgraphPlans) { sameSubgraphPlan =>
                                                val candidates = groupPending(
                                                  entityFetchFields.iterator.map { case ((targetSubgraph, requirements), fields) =>
                                                    PendingFetch(targetSubgraph, fields.toList, requirements)
                                                  }.toList ::: sameSubgraphPlan.pending
                                                )
                                                planEntityFetches(
                                                  field,
                                                  currentSubgraph,
                                                  path,
                                                  parentType,
                                                  typeName,
                                                  visitedFetches,
                                                  availableExternal,
                                                  providedFields,
                                                  sameSubgraphPlan.downstream,
                                                  sameSubgraphPlan.entities,
                                                  sameSubgraphPlan.typenameSelections,
                                                  candidates
                                                )
                                              }
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

  private def planSameSubgraphFields(
    currentSubgraph: String,
    path: Vector[String],
    visitedFetches: Set[EntityFetchKey],
    runtimeSources: Set[String],
    fields: List[(Field, List[Field])]
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[PlannedSelections]] =
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
    field: Field,
    currentSubgraph: String,
    path: Vector[String],
    parentType: __Type,
    typeName: String,
    visitedFetches: Set[EntityFetchKey],
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field],
    selected: List[Field],
    nestedEntities: List[PlannedEntity],
    typenameSelections: List[TypenameSelection],
    pending: List[PendingFetch]
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[PlannedField]] = {
    val context = EntityFetchContext(
      field,
      currentSubgraph,
      path,
      parentType,
      typeName,
      visitedFetches,
      availableExternal,
      provided
    )
    planPendingEntityFetches(
      context,
      EntityFetchState(selected.toVector, nestedEntities, Nil, typenameSelections),
      pending
    ).map { states =>
      states.map { state =>
        PlannedField(
          field.copy(fields = mergeFields(state.downstream.toList)),
          state.entities,
          state.pending,
          state.typenameSelections
        )
      }
    }
  }

  private def planPendingEntityFetches(
    context: EntityFetchContext,
    initial: EntityFetchState,
    pending: List[PendingFetch]
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[EntityFetchState]] =
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
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[EntityFetchState]] = {
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
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[EntityFetchState]] =
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
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[EntityFetchState]] = {
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
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[EntityFetchState]] = {
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
  )(implicit search: PlanningSearch): Either[PlanningFailure, List[PlannedField]] =
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

  private def routeCost(candidate: PlannedCandidate): (Int, Int, Int) =
    (
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

    def combine[A, B, C](left: List[A], right: List[B])(combine: (A, B) => C): Either[PlanningFailure, List[C]] =
      if (left.isEmpty || right.isEmpty) check.map(_ => Nil)
      else if (left.tail.isEmpty && right.tail.isEmpty) check.map(_ => combine(left.head, right.head) :: Nil)
      else {
        val count = left.size.toLong * right.size.toLong
        capacity(count).map(_ => left.flatMap(a => right.map(combine(a, _))))
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
    def toRoute(id: RouteId, root: RouteId, dependencies: Set[RouteId]): EntityRoute =
      EntityRoute(
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

  final case class RootRoute(id: RouteId, source: String, client: List[Field], downstream: List[Field])

  /**
   * The response path and alias of an injected __typename field used during response completion.
   */
  final case class TypenameSelection(path: Vector[String], responseName: String)

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
    mayNeedPrerequisiteFetches: Boolean
  ) {

    lazy val selectionKey: String = canonicalSelectionKey(fields)
  }

  private[internal] def canonicalSelectionKey(fields: List[Field]): String = {
    def renderSelection(selection: Selection): String =
      DocumentRenderer.selectionsRenderer.renderCompact(selection :: Nil)

    def canonicalDirective(directive: Directive): Directive =
      directive.copy(arguments = ListMap(directive.arguments.toList.sortBy(_._1): _*), index = 0)

    def canonicalSelections(selections: List[Selection]): List[Selection] = {
      val canonical = selections.map(canonicalSelection)
      canonical match {
        case Nil | _ :: Nil => canonical
        case _              =>
          canonical
            .map(selection => renderSelection(selection) -> selection)
            .sortBy(_._1)
            .map(_._2)
      }
    }

    def canonicalSelection(selection: Selection): Selection =
      selection match {
        case field: Selection.Field             =>
          field.copy(
            arguments = ListMap(field.arguments.toList.sortBy(_._1): _*),
            directives = field.directives.map(canonicalDirective),
            selectionSet = canonicalSelections(field.selectionSet),
            index = 0
          )
        case fragment: Selection.InlineFragment =>
          fragment.copy(
            dirs = fragment.dirs.map(canonicalDirective),
            selectionSet = canonicalSelections(fragment.selectionSet)
          )
        case fragment: Selection.FragmentSpread =>
          fragment.copy(directives = fragment.directives.map(canonicalDirective))
      }

    val selections = canonicalSelections(fields.map(_.toSelection))
    DocumentRenderer.selectionsRenderer.renderCompact(selections)
  }

  final case class OperationPlan(
    operation: OperationType,
    rootName: String,
    fields: List[Field],
    localFields: List[Field],
    roots: List[RootRoute],
    entities: List[EntityRoute],
    typenameSelections: List[TypenameSelection],
    passthroughSubgraph: Option[String]
  ) {

    private[internal] lazy val cache: PlanExecutionCache = new PlanExecutionCache

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

      def bindRoot(route: RootRoute): RootRoute =
        if (route.client.exists(fieldReferences) || route.downstream.exists(fieldReferences))
          route.copy(client = route.client.map(bindField), downstream = route.downstream.map(bindField))
        else route

      def bindEntity(route: EntityRoute): EntityRoute =
        if (route.fields.exists(fieldReferences)) route.copy(fields = route.fields.map(bindField))
        else route

      plan.copy(
        fields = plan.fields.map(bindField),
        localFields = plan.localFields.map(bindField),
        roots = plan.roots.map(bindRoot),
        entities = plan.entities.map(bindEntity)
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
