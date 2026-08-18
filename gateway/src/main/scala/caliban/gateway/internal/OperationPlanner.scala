package caliban.gateway.internal

import caliban.execution.{ ExecutionRequest, Field }
import caliban.gateway.internal.OperationPlanner._
import caliban.introspection.adt.{ __Type, __TypeKind }
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
          planned <- sources.foldLeft[Either[PlanningFailure, List[PlannedRoot]]](Right(Nil)) { case (values, source) =>
                       for {
                         accumulated <- values
                         selected     = if (sources.size == 1) field else rootFieldForSource(field, source, sources)
                         plan        <- planField(
                                          selected,
                                          source,
                                          Vector(field.aliasedName),
                                          Set.empty,
                                          availableKeys(source, selected.fieldType.innerType),
                                          Nil,
                                          Set.empty
                                        )
                         _           <- Either.cond(
                                          plan.pending.isEmpty,
                                          (),
                                          PlanningFailure(unsatisfiedMessage(plan.pending))
                                        )
                       } yield
                         if (hasRootWork(plan))
                           PlannedRoot(source, field, plan.downstream, plan.entities) :: accumulated
                         else accumulated
                     }
        } yield planned ::: roots
      }
      .map(_.reverse)

    planned.flatMap { roots =>
      val grouped                                                                         = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[PlannedRoot]]
      roots.foreach(root => grouped.getOrElseUpdate(root.source, mutable.ListBuffer.empty) += root)
      val routes                                                                          = grouped.iterator.zipWithIndex.map { case ((source, values), index) =>
        val selected = values.toList
        RootRoute(RouteId(index), source, selected.map(_.client), selected.map(_.downstream))
      }.toList
      val routeBySource                                                                   = routes.iterator.map(route => route.source -> route.id).toMap
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
      val baseEntities                                                                    = roots.flatMap { planned =>
        routeBySource
          .get(planned.source)
          .toList
          .flatMap(root => flatten(planned.entities, root, Set(root)))
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
      val owners                                                                          = (routes.map(_.source) ::: entities.map(_.source)).distinct
      val passthrough                                                                     =
        if (sourceCount == 1 && routes.size == 1 && entities.isEmpty && localFields.isEmpty)
          routes.headOption.map(_.source)
        else None

      validateDependencies(entities).flatMap { _ =>
        if (execution.operationType == OperationType.Mutation && owners.size > 1)
          Left(PlanningFailure("Mutations spanning multiple subgraphs are not supported by this gateway."))
        else if (passthrough.isEmpty && hasCustomExecutableDirective(document, execution.operationName))
          Left(PlanningFailure("Custom executable directives are not supported by this gateway."))
        else
          Right(
            OperationPlan(execution.operationType, rootName, fields, localFields, routes, entities, passthrough)
          )
      }
    }
  }

  private def rootFieldForSource(field: Field, source: String, rootSources: List[String]): Field = {
    def filter(parent: __Type, fields: List[Field], candidates: List[String]): List[Field] = {
      val typeName = parent.name.getOrElse("")
      fields.flatMap { child =>
        val childParent = child.parentType.flatMap(_.name).getOrElse(typeName)
        val owners      = candidates.filter(graph.owns(_, childParent, child.name))
        val next        = if (owners.nonEmpty) owners else candidates
        val children    = filter(child.fieldType.innerType, child.fields, next)
        val include     =
          child.name == "__typename" && candidates.contains(source) ||
            (if (child.fields.nonEmpty) children.nonEmpty
             else owners.contains(source) || owners.isEmpty && candidates.headOption.contains(source))

        if (include) child.copy(fields = children) :: Nil else Nil
      }
    }

    field.copy(fields = filter(field.fieldType.innerType, field.fields, rootSources))
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
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field],
    satisfiedRequirements: Set[(String, String)]
  ): Either[PlanningFailure, PlannedField] = {
    val parentType = field.fieldType.innerType
    val typeName   = parentType.name.getOrElse("")
    val scoped     = mergeFields(
      provided ::: fieldSetFields(
        graph.provided(source, field.parentType.flatMap(_.name).getOrElse(""), field.name),
        field.fieldType
      )
    )
    val local      = mutable.ListBuffer.empty[(Field, List[Field])]
    val remote     = mutable.LinkedHashMap.empty[(String, List[Selection]), mutable.ListBuffer[Field]]
    var failure    = Option.empty[PlanningFailure]

    selectedFields(field, parentType, typeName).foreach { child =>
      val childParent = child.parentType.flatMap(_.name).getOrElse(typeName)
      val supplied    = scoped.find(candidate => sameField(candidate, child))
      val provider    =
        if (child.name == "__typename") Some(source)
        else supplied.map(_ => source).orElse(graph.source(childParent, child.name, source))

      provider match {
        case Some(`source`) =>
          val requirements = graph.required(source, childParent, child.name)
          if (requirements.isEmpty || satisfiedRequirements.contains(childParent -> child.name))
            local += child -> supplied.toList.flatMap(_.fields)
          else remote.getOrElseUpdate(source -> requirements, mutable.ListBuffer.empty) += child
        case Some(other)    =>
          val requirements = graph.required(other, childParent, child.name)
          remote.getOrElseUpdate(other -> requirements, mutable.ListBuffer.empty) += child
        case None           =>
          if (failure.isEmpty) failure = Some(PlanningFailure(s"No subgraph owns field '$typeName.${child.name}'."))
      }
    }

    failure match {
      case Some(value) => Left(value)
      case None        =>
        for {
          localPlan <- planLocalFields(source, path, trail, local.toList)
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
                         candidates
                       )
        } yield planned
    }
  }

  private def planLocalFields(
    source: String,
    path: Vector[String],
    trail: Set[TransitionKey],
    local: List[(Field, List[Field])]
  ): Either[PlanningFailure, PlannedSelections] =
    local
      .foldLeft[Either[PlanningFailure, PlannedSelections]](Right(PlannedSelections(Nil, Nil, Nil))) {
        case (result, (child, provided)) =>
          for {
            values  <- result
            planned <- planField(
                         child,
                         source,
                         path :+ child.aliasedName,
                         trail,
                         availableKeys(source, child.fieldType.innerType),
                         provided,
                         Set.empty
                       )
          } yield PlannedSelections(
            planned.downstream :: values.downstream,
            values.entities ::: planned.entities,
            values.pending ::: wrapPending(child, planned.pending)
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
    pending: List[PendingSelection]
  ): Either[PlanningFailure, PlannedField] =
    pending
      .foldLeft[Either[PlanningFailure, TransitionState]](
        Right(TransitionState(selected.toVector, nestedEntities, Nil))
      ) { case (result, candidate) =>
        for {
          current <- result
          next    <- planTransition(
                       field,
                       source,
                       path,
                       parentType,
                       typeName,
                       trail,
                       availableExternal,
                       provided,
                       current,
                       candidate
                     )
        } yield next
      }
      .map { state =>
        PlannedField(field.copy(fields = mergeFields(state.downstream.toList)), state.entities, state.pending)
      }

  private def planTransition(
    field: Field,
    source: String,
    path: Vector[String],
    parentType: __Type,
    typeName: String,
    trail: Set[TransitionKey],
    availableExternal: List[ComposedGraph.KeyField],
    provided: List[Field],
    state: TransitionState,
    candidate: PendingSelection
  ): Either[PlanningFailure, TransitionState] = {
    val target     = candidate.source
    val transition = TransitionKey(source, target, typeName, flatten(candidate.fields))
    if (trail.contains(transition))
      Left(PlanningFailure(s"Entity routing cycle detected: ${transition.render}."))
    else {
      val lookup = selectLookup(parentType, typeName, source, target, availableExternal) match {
        case Right((value, fields)) => Right(LookupSelection.Static(value, fields))
        case Left(failure)          => clientLookup(field, parentType, typeName, target).toRight(failure)
      }

      lookup match {
        case Right(selection) =>
          val requirementData                =
            injectRequirementFields(field, state.downstream, fieldSetFields(candidate.requirements, parentType))
          val (requiredFields, requirements) = requirementData
          for {
            requirementPlan             <- planRequirements(
                                             field,
                                             source,
                                             path,
                                             trail + transition,
                                             availableExternal,
                                             provided,
                                             requiredFields
                                           )
            _                           <- Either.cond(
                                             requirementPlan.pending.isEmpty,
                                             (),
                                             PlanningFailure(unsatisfiedMessage(requirementPlan.pending))
                                           )
            enrichedDownstream           = mergeFields(state.downstream.toList ::: requirementPlan.downstream.fields).toVector
            planned                     <- planField(
                                             field.copy(fields = candidate.fields),
                                             target,
                                             path,
                                             trail + transition,
                                             selection.lookup.key,
                                             Nil,
                                             candidate.fields.iterator
                                               .map(child => child.parentType.flatMap(_.name).getOrElse(typeName) -> child.name)
                                               .toSet
                                           )
            keyData                      = selection match {
                                             case LookupSelection.Static(value, fields) =>
                                               injectKeyFields(field, parentType, enrichedDownstream, fields, value)
                                             case LookupSelection.Client(value, fields) =>
                                               injectSelectedKeys(field, parentType, enrichedDownstream, fields, value)
                                           }
            (downstream, keys, typename) = keyData
            entity                       = PlannedEntity(
                                             target,
                                             source,
                                             path,
                                             typeName,
                                             keys,
                                             requirements,
                                             typename,
                                             selection.lookup,
                                             planned.downstream.fields,
                                             planned.entities,
                                             selection.requiresKeyEnrichment || requirementPlan.entities.nonEmpty
                                           )
            next                         = TransitionState(
                                             downstream,
                                             state.entities ::: requirementPlan.entities ::: (entity :: Nil),
                                             state.pending
                                           )
            resolved                    <- planned.pending.foldLeft[Either[PlanningFailure, TransitionState]](Right(next)) {
                                             case (result, pending) =>
                                               result.flatMap(current =>
                                                 planTransition(
                                                   field,
                                                   source,
                                                   path,
                                                   parentType,
                                                   typeName,
                                                   trail + transition,
                                                   availableExternal,
                                                   provided,
                                                   current,
                                                   pending
                                                 )
                                               )
                                           }
          } yield resolved
        case Left(_)          =>
          val candidates = bridgeSources(typeName, source, target)
          val attempts   = candidates.iterator.map(next =>
            planTransition(
              field,
              source,
              path,
              parentType,
              typeName,
              trail + transition,
              availableExternal,
              provided,
              state,
              PendingSelection(next, candidate.fields, Nil)
            )
          )
          attempts.collect { case Right(next) if next.pending == state.pending => next }.toList
            .sortBy(next => entityCount(next.entities) - entityCount(state.entities))
            .headOption
            .map(Right(_))
            .getOrElse(Right(state.copy(pending = groupPending(state.pending ::: (candidate :: Nil)))))
      }
    }
  }

  private def clientLookup(
    field: Field,
    parentType: __Type,
    typeName: String,
    target: String
  ): Option[LookupSelection.Client] =
    graph
      .lookups(target, typeName)
      .iterator
      .flatMap(lookup => clientKeySelections(field.collectFields(typeName), parentType, lookup.key).map(lookup -> _))
      .map { case (lookup, fields) => LookupSelection.Client(lookup, fields) }
      .toList
      .headOption

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
      case caliban.introspection.adt.__TypeKind.INTERFACE | caliban.introspection.adt.__TypeKind.UNION => field.fields
      case _                                                                                           => field.collectFields(typeName)
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
    if (requirements.isEmpty) Right(PlannedField(field.copy(fields = Nil), Nil, Nil))
    else planField(field.copy(fields = requirements), source, path, trail, availableExternal, provided, Set.empty)

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
    lookup: ComposedGraph.EntityLookup
  ): (Vector[Field], List[RequiredSelection], Option[RequiredSelection]) = {
    val usedNames = field.fields.iterator.map(_.aliasedName).toSet ++ selected.iterator.map(_.aliasedName)
    val keyData   = keyFields.foldLeft(
      (List.empty[RequiredSelection], Vector.empty[Field], usedNames)
    ) { case ((selections, fields, names), keyField) =>
      val alias = privateAlias("_caliban_gateway_key", names)
      (
        requiredSelection(keyField, alias) :: selections,
        fields :+ requiredField(keyField, parentType, alias),
        names + alias
      )
    }
    val keys      = keyData._1.reverse
    val typename  =
      if (lookup.operation.requiresTypename)
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
    lookup: ComposedGraph.EntityLookup
  ): (Vector[Field], List[RequiredSelection], Option[RequiredSelection]) = {
    val usedNames = field.fields.iterator.map(_.aliasedName).toSet ++ selected.iterator.map(_.aliasedName)
    val typename  =
      if (lookup.operation.requiresTypename)
        Some(RequiredSelection("__typename", privateAlias("_caliban_gateway_typename", usedNames)))
      else None
    val typeField = typename.map(selection =>
      Field(selection.field, Types.string, Some(parentType), alias = Some(selection.responseName))
    )
    (selected ++ typeField, keys, typename)
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
      field    <- Option(parentType.getFieldOrNull(key.name)).toRight(
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
    parentType.name.exists(graph.declares(source, _, key.name)) && Option(parentType.getFieldOrNull(key.name)).exists {
      field => key.children.forall(declaredKey(source, field._type.innerType, _))
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
    val grouped = mutable.LinkedHashMap.empty[(String, List[Selection]), mutable.ListBuffer[Field]]
    values.foreach(value =>
      grouped.getOrElseUpdate(value.source -> value.requirements, mutable.ListBuffer.empty) ++= value.fields
    )
    grouped.iterator.map { case ((source, requirements), fields) =>
      PendingSelection(source, mergeFields(fields.toList), requirements)
    }.toList
  }

  private def entityCount(entities: List[PlannedEntity]): Int =
    entities.foldLeft(0)((count, entity) => count + 1 + entityCount(entity.entities))

  private def mergeFields(fields: List[Field]): List[Field] = {
    val grouped = mutable.LinkedHashMap.empty[String, Field]
    fields.foreach { field =>
      grouped.get(field.aliasedName) match {
        case Some(existing) =>
          grouped.update(field.aliasedName, existing.copy(fields = mergeFields(existing.fields ::: field.fields)))
        case None           => grouped.put(field.aliasedName, field)
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
    field: caliban.introspection.adt.__Field,
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

  private final case class PlannedSelections(
    downstream: List[Field],
    entities: List[PlannedEntity],
    pending: List[PendingSelection]
  )

  private final case class TransitionState(
    downstream: Vector[Field],
    entities: List[PlannedEntity],
    pending: List[PendingSelection]
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
    pending: List[PendingSelection]
  )

  private final case class PlannedRoot(source: String, client: Field, downstream: Field, entities: List[PlannedEntity])

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
    passthrough: Option[String]
  )
}
