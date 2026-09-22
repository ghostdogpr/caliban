package caliban.gateway.internal.planning

import caliban.InputValue
import caliban.execution.Field
import caliban.gateway.TypenameField
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.planning.CandidateSearch.PlanningFailure
import caliban.gateway.internal.planning.FetchGraphOptimizer._
import caliban.gateway.internal.planning.OperationPlan._
import caliban.introspection.adt.__TypeKind

import scala.collection.mutable

private[planning] final class FetchGraphOptimizer(graph: ComposedGraph) {
  def optimize(
    roots: List[RootFetch],
    fetches: List[EntityFetch]
  ): Either[PlanningFailure, (List[EntityFetch], Int)] = {
    val merged = mergeEquivalentFetches(addFetchDependencies(fetches, roots))
    dependencyDepth(merged).map(merged -> _)
  }

  /**
   * Adds dependencies on fetches that supply entity keys or fields needed by @requires.
   */
  private def addFetchDependencies(fetches: List[EntityFetch], roots: List[RootFetch]): List[EntityFetch] =
    if (!fetches.exists(_.mayNeedPrerequisiteFetches)) fetches
    else {
      val byId = fetches.iterator.map(fetch => fetch.id -> fetch).toMap

      def dependsOn(fetch: EntityFetch, dependency: FetchId, seen: Set[FetchId]): Boolean                      =
        fetch.dependencies.contains(dependency) || fetch.dependencies.exists { id =>
          !seen.contains(id) && byId.get(id).exists(dependsOn(_, dependency, seen + id))
        }
      def selectionPaths(selection: RequiredSelection): List[Vector[String]]                                   =
        if (selection.children.isEmpty) Vector(selection.responseName) :: Nil
        else selection.children.flatMap(selectionPaths).map(Vector(selection.responseName) ++ _)
      def targetedPaths(field: Field): List[(Vector[String], List[Set[String]])]                               = {
        val targets = field.targets.toList
        if (field.fields.isEmpty) (Vector(field.aliasedName), targets) :: Nil
        else
          field.fields.flatMap(targetedPaths).map { case (path, nested) =>
            (Vector(field.aliasedName) ++ path, targets ::: nested)
          }
      }
      def provides(paths: List[(Vector[String], List[Set[String]])], path: Vector[String], entityType: String) =
        paths.exists { case (candidate, targets) => candidate == path && targets.forall(_.contains(entityType)) }

      val providedPaths = fetches.iterator.map(fetch => fetch.id -> fetch.fields.flatMap(targetedPaths)).toMap
      val rootPaths     = roots.iterator.map(root => root.id -> root.downstream.flatMap(targetedPaths)).toMap

      fetches.map { fetch =>
        if (!fetch.mayNeedPrerequisiteFetches) fetch
        else {
          val required                                               =
            (fetch.keys ::: fetch.requirements).flatMap(selectionPaths).map(fetch.mergePath ++ _).toSet ++
              fetch.contextArguments.flatMap(argument => argument.projection.paths.map(argument.sourcePath ++ _))
          def provided(candidate: EntityFetch): List[Vector[String]] =
            providedPaths(candidate.id).map { case (path, _) => candidate.mergePath ++ path }.filter(required)
          def providedElsewhere(path: Vector[String]): Boolean       =
            provides(rootPaths.getOrElse(fetch.root, Nil), path, fetch.entityType) ||
              fetches.exists(other =>
                other.id != fetch.id && other.root == fetch.root && path.startsWith(other.mergePath) &&
                  provides(providedPaths(other.id), path.drop(other.mergePath.size), fetch.entityType)
              )
          val dependencies                                           = fetches.iterator
            .filter(_.root == fetch.root)
            .filterNot(candidate => dependsOn(candidate, fetch.id, Set.empty))
            .filter { candidate =>
              if (candidate.id == fetch.id) provided(candidate).exists(path => !providedElsewhere(path))
              else provided(candidate).nonEmpty
            }
            .map(_.id)
            .toSet
          fetch.copy(dependencies = fetch.dependencies ++ dependencies)
        }
      }
    }

  private def mergeEquivalentFetches(fetches: List[EntityFetch]): List[EntityFetch] = {
    val grouped  = mutable.LinkedHashMap.empty[EntityFetch, mutable.ListBuffer[EntityFetch]]
    val replaced = mutable.HashMap.empty[FetchId, FetchId]
    fetches.foreach { fetch =>
      val key    = fetch.copy(
        id = FetchId(0),
        dependencies = fetch.dependencies.map(id => replaced.getOrElse(id, id)),
        fields = Nil,
        mayNeedPrerequisiteFetches = false
      )
      val bucket = grouped.getOrElseUpdate(key, mutable.ListBuffer.empty)
      val index  =
        bucket.indexWhere(existing =>
          preservesFailureIsolation(fetch.source, fetch.entityType, existing.fields, fetch.fields)
        )
      if (index < 0) bucket += fetch
      else {
        val existing = bucket(index)
        replaced.update(fetch.id, existing.id)
        bucket.update(
          index,
          existing.copy(
            fields = mergeFields(existing.fields ::: fetch.fields),
            mayNeedPrerequisiteFetches = existing.mayNeedPrerequisiteFetches || fetch.mayNeedPrerequisiteFetches
          )
        )
      }
    }
    if (replaced.isEmpty) fetches
    else
      mergeEquivalentFetches(
        grouped.valuesIterator.flatten
          .map(fetch => fetch.copy(dependencies = fetch.dependencies.map(id => replaced.getOrElse(id, id))))
          .toList
      )
  }

  private def preservesFailureIsolation(
    source: String,
    entityType: String,
    left: List[Field],
    right: List[Field]
  ): Boolean = {
    def isolated(own: List[Field], other: List[Field]): Boolean = own.forall { field =>
      field.name == TypenameField || !declaredNonNull(source, entityType, field) || other.exists(covers(_, field))
    }
    isolated(left, right) && isolated(right, left)
  }

  private def declaredNonNull(source: String, entityType: String, field: Field): Boolean =
    field.targets.getOrElse(Set(entityType)).exists { owner =>
      (owner :: graph.possibleTypes(source, owner))
        .flatMap(graph.sourceField(source, _, field.name)) match {
        case Nil          => true
        case declarations => declarations.exists(_._type.kind == __TypeKind.NON_NULL)
      }
    }

  private def covers(candidate: Field, field: Field): Boolean =
    candidate.targets == field.targets && candidate.fragment.forall(_.directives.isEmpty) &&
      candidate.copy(alias = None).toSelection == field.copy(alias = None).toSelection

  private def dependencyDepth(fetches: List[EntityFetch]): Either[PlanningFailure, Int] = {
    val fetchById = fetches.iterator.map(fetch => fetch.id -> fetch).toMap
    val depths    = mutable.Map.empty[FetchId, Int]

    def maxDepth(ids: Iterable[FetchId], visiting: Set[FetchId]): Either[PlanningFailure, Int] =
      ids.foldLeft[Either[PlanningFailure, Int]](Right(0)) { (result, id) =>
        result.flatMap(current => visit(id, visiting).map(math.max(current, _)))
      }

    def visit(id: FetchId, visiting: Set[FetchId]): Either[PlanningFailure, Int] =
      depths.get(id) match {
        case Some(depth)                   => Right(depth)
        case None if visiting.contains(id) =>
          Left(PlanningFailure.Rejected("Entity routing dependency cycle detected."))
        case None                          =>
          fetchById.get(id) match {
            case None        => Right(0)
            case Some(fetch) =>
              maxDepth(fetch.dependencies, visiting + id).map { dependencyDepth =>
                val depth = dependencyDepth + 1
                depths.put(id, depth)
                depth
              }
          }
      }

    maxDepth(fetches.map(_.id), Set.empty)
  }
}

private[planning] object FetchGraphOptimizer {
  def mergeFields(fields: List[Field]): List[Field] = {
    val grouped = mutable.LinkedHashMap.empty[FieldKey, Field]
    fields.foreach { field =>
      val key = FieldKey(field.aliasedName, field.name, field.arguments, field.targets)
      grouped.get(key) match {
        case Some(existing) =>
          grouped.update(key, existing.copy(fields = mergeFields(existing.fields ::: field.fields)))
        case None           => grouped.put(key, field)
      }
    }
    grouped.values.toList
  }

  private final case class FieldKey(
    responseName: String,
    name: String,
    arguments: Map[String, InputValue],
    targets: Option[Set[String]]
  )
}
