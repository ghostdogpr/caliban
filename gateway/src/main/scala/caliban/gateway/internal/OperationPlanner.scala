package caliban.gateway.internal

import caliban.execution.{ ExecutionRequest, Field }
import caliban.gateway.internal.OperationPlanner._
import caliban.introspection.adt.__Type
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
          roots  <- result
          source <- graph
                      .source(execution.operationType, field.name)
                      .toRight(PlanningFailure(s"No subgraph owns root field '${field.name}'."))
          plan   <- planField(field, source, Vector(source), Vector(field.aliasedName), transitions = 0)
        } yield PlannedRoot(source, field, plan.downstream, plan.entities) :: roots
      }
      .map(_.reverse)

    planned.flatMap { roots =>
      val grouped       = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[PlannedRoot]]
      roots.foreach(root => grouped.getOrElseUpdate(root.source, mutable.ListBuffer.empty) += root)
      val routes        = grouped.iterator.zipWithIndex.map { case ((source, values), index) =>
        val selected = values.toList
        RootRoute(RouteId(index), source, selected.map(_.client), selected.map(_.downstream))
      }.toList
      val routeBySource = routes.iterator.map(route => route.source -> route.id).toMap
      val entities      = roots.flatMap(root =>
        root.entities.map(entity =>
          EntityRoute(
            entity.source,
            routeBySource(root.source),
            root.source,
            entity.mergePath,
            entity.entityType,
            entity.keys,
            entity.typename,
            entity.lookup,
            entity.fields
          )
        )
      )
      val owners        = (routes.map(_.source) ::: entities.map(_.source)).distinct
      val passthrough   =
        if (sourceCount == 1 && routes.size == 1 && entities.isEmpty && localFields.isEmpty)
          routes.headOption.map(_.source)
        else None

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

  private def planField(
    field: Field,
    source: String,
    visitedSources: Vector[String],
    path: Vector[String],
    transitions: Int
  ): Either[PlanningFailure, PlannedField] = {
    val parentType = field.fieldType.innerType
    val typeName   = parentType.name.getOrElse("")
    val local      = mutable.ListBuffer.empty[Field]
    val remote     = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[Field]]
    var failure    = Option.empty[PlanningFailure]

    field.collectFields(typeName).foreach { child =>
      val provider =
        if (child.name == "__typename") Some(source)
        else graph.source(typeName, child.name, source)

      provider match {
        case Some(`source`) => local += child
        case Some(other)    => remote.getOrElseUpdate(other, mutable.ListBuffer.empty) += child
        case None           =>
          if (failure.isEmpty) failure = Some(PlanningFailure(s"No subgraph owns field '$typeName.${child.name}'."))
      }
    }

    failure match {
      case Some(value) => Left(value)
      case None        =>
        planLocalFields(field, source, visitedSources, path, transitions, local.toList).flatMap { selected =>
          planRemoteFields(field, source, visitedSources, path, transitions, parentType, typeName, selected, remote)
        }
    }
  }

  private def planLocalFields(
    field: Field,
    source: String,
    visitedSources: Vector[String],
    path: Vector[String],
    transitions: Int,
    local: List[Field]
  ): Either[PlanningFailure, List[Field]] =
    local
      .foldLeft[Either[PlanningFailure, List[Field]]](Right(Nil)) { case (result, child) =>
        for {
          values  <- result
          planned <- planField(child, source, visitedSources, path :+ child.aliasedName, transitions)
          _       <-
            if (planned.entities.isEmpty) Right(())
            else Left(PlanningFailure("Nested entity transitions are not supported by this gateway."))
        } yield planned.downstream :: values
      }
      .map(_.reverse)

  private def planRemoteFields(
    field: Field,
    source: String,
    visitedSources: Vector[String],
    path: Vector[String],
    transitions: Int,
    parentType: __Type,
    typeName: String,
    selected: List[Field],
    remote: mutable.LinkedHashMap[String, mutable.ListBuffer[Field]]
  ): Either[PlanningFailure, PlannedField] =
    remote.toList
      .foldLeft[Either[PlanningFailure, (Vector[Field], List[PlannedEntity])]](Right(selected.toVector -> Nil)) {
        case (result, (target, children)) =>
          for {
            current                             <- result
            child                               <- children.headOption.toRight(
                                                     PlanningFailure(
                                                       s"Cannot route '$typeName': no downstream field was selected."
                                                     )
                                                   )
            _                                   <- validateTransition(visitedSources, target, transitions)
            lookup                              <-
              graph
                .lookup(target, typeName)
                .toRight(
                  PlanningFailure(
                    s"Cannot route '$typeName.${child.name}': source '$target' has no resolvable entity lookup."
                  )
                )
            keyFields                           <- requiredKeyFields(parentType, typeName, source, child.name, lookup)
            plannedChildren                     <- planEntityFields(
                                                     children.toList,
                                                     target,
                                                     visitedSources,
                                                     path,
                                                     transitions
                                                   )
            (downstreamFields, entityRoutes)     = current
            usedNames                            = field.fields.iterator.map(_.aliasedName).toSet ++
                                                     downstreamFields.iterator.map(_.aliasedName)
            keyData                              = keyFields.foldLeft(
                                                     (Vector.empty[RequiredSelection], Vector.empty[Field], usedNames)
                                                   ) { case ((selections, fields, names), (key, keyField)) =>
                                                     val alias = privateAlias("_caliban_gateway_key", names)
                                                     (
                                                       selections :+ RequiredSelection(key, alias),
                                                       fields :+ Field(key, keyField._type, Some(parentType), alias = Some(alias)),
                                                       names + alias
                                                     )
                                                   }
            (keySelections, internalKeys, names) = keyData
            typenameSelection                    =
              if (lookup.operation.requiresTypename)
                Some(
                  RequiredSelection(
                    "__typename",
                    privateAlias("_caliban_gateway_typename", names)
                  )
                )
              else None
            internalTypename                     = typenameSelection.map(selection =>
                                                     Field(
                                                       selection.field,
                                                       Types.string,
                                                       Some(parentType),
                                                       alias = Some(selection.responseName)
                                                     )
                                                   )
            entity                               = PlannedEntity(
                                                     target,
                                                     path,
                                                     typeName,
                                                     keySelections.toList,
                                                     typenameSelection,
                                                     lookup,
                                                     plannedChildren
                                                   )
          } yield (downstreamFields ++ internalKeys ++ internalTypename) -> (entity :: entityRoutes)
      }
      .map { case (downstreamFields, entities) =>
        PlannedField(field.copy(fields = downstreamFields.toList), entities.reverse)
      }

  private def requiredKeyFields(
    parentType: __Type,
    typeName: String,
    source: String,
    childName: String,
    lookup: ComposedGraph.EntityLookup
  ): Either[PlanningFailure, List[(String, caliban.introspection.adt.__Field)]] =
    lookup.keyFields
      .foldLeft[Either[PlanningFailure, List[(String, caliban.introspection.adt.__Field)]]](Right(Nil)) {
        case (result, key) =>
          for {
            fields   <- result
            keyField <- Option(parentType.getFieldOrNull(key)).toRight(
                          PlanningFailure(
                            s"Cannot route '$typeName.$childName': source '$source' does not provide key field '$key'."
                          )
                        )
            _        <-
              if (graph.source(typeName, key, source).contains(source)) Right(())
              else
                Left(
                  PlanningFailure(
                    s"Cannot route '$typeName.$childName': source '$source' does not provide key field '$key'."
                  )
                )
          } yield (key -> keyField) :: fields
      }
      .map(_.reverse)

  private def validateTransition(
    visitedSources: Vector[String],
    target: String,
    transitions: Int
  ): Either[PlanningFailure, Unit] =
    if (visitedSources.contains(target))
      Left(PlanningFailure(s"Entity routing cycle detected: ${(visitedSources :+ target).mkString(" -> ")}."))
    else if (transitions > 0)
      Left(PlanningFailure("More than one entity transition is not supported by this gateway."))
    else Right(())

  private def planEntityFields(
    children: List[Field],
    target: String,
    visitedSources: Vector[String],
    path: Vector[String],
    transitions: Int
  ): Either[PlanningFailure, List[Field]] =
    children
      .foldLeft[Either[PlanningFailure, List[Field]]](Right(Nil)) { case (fields, child) =>
        for {
          values  <- fields
          planned <- planField(
                       child,
                       target,
                       visitedSources :+ target,
                       path :+ child.aliasedName,
                       transitions + 1
                     )
          _       <-
            if (planned.entities.isEmpty) Right(())
            else Left(PlanningFailure("More than one entity transition is not supported by this gateway."))
        } yield planned.downstream :: values
      }
      .map(_.reverse)

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

  final case class RequiredSelection(field: String, responseName: String)

  def privateAlias(base: String, used: Set[String]): String = {
    var candidate = base
    var suffix    = 2
    while (used.contains(candidate)) {
      candidate = s"${base}_$suffix"
      suffix += 1
    }
    candidate
  }

  final case class PlannedField(downstream: Field, entities: List[PlannedEntity])

  final case class PlannedRoot(source: String, client: Field, downstream: Field, entities: List[PlannedEntity])

  final case class PlannedEntity(
    source: String,
    mergePath: Vector[String],
    entityType: String,
    keys: List[RequiredSelection],
    typename: Option[RequiredSelection],
    lookup: ComposedGraph.EntityLookup,
    fields: List[Field]
  )

  final case class RootRoute(id: RouteId, source: String, client: List[Field], downstream: List[Field])

  final case class EntityRoute(
    source: String,
    dependency: RouteId,
    dependencySource: String,
    mergePath: Vector[String],
    entityType: String,
    keys: List[RequiredSelection],
    typename: Option[RequiredSelection],
    lookup: ComposedGraph.EntityLookup,
    fields: List[Field]
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
