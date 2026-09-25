package caliban.gateway.internal.planning

import caliban.InputValue
import caliban.execution.Field
import caliban.gateway.internal.planning.CandidateSearch.PlanningFailure
import caliban.gateway.internal.planning.OperationPlan._
import caliban.gateway.traverseEither

import scala.annotation.tailrec
import scala.collection.mutable

private[planning] object FetchGraphOptimizer {
  def optimize(roots: List[RootFetch], fetches: List[EntityFetch]): Either[PlanningFailure, List[EntityFetch]] =
    addFetchDependencies(fetches, roots).map(mergeEquivalentFetches)

  /**
   * Counts compatible entity batches by dependency wave. Dependencies outside this list are already satisfied.
   * Returns the dependency depth and the number of calls, or the cycle that prevents ordering the fetches.
   */
  def waves(fetches: List[EntityFetch]): Either[PlanningFailure, (Int, Int)] = {
    val fetchIds = fetches.iterator.map(_.id).toSet

    @tailrec
    def count(
      pending: List[EntityFetch],
      completed: Set[FetchId],
      depth: Int,
      calls: Int
    ): Either[PlanningFailure, (Int, Int)] =
      if (pending.isEmpty) Right(depth -> calls)
      else {
        val (ready, waiting) =
          pending.partition(fetch => fetch.dependencies.forall(id => completed.contains(id) || !fetchIds(id)))
        if (ready.isEmpty) Left(DependencyCycle)
        else
          count(
            waiting,
            completed ++ ready.iterator.map(_.id),
            depth + 1,
            calls + ready.iterator.map(entityGroupKey).toSet.size
          )
      }

    count(fetches, Set.empty, 0, 0)
  }

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

  /**
   * Adds dependencies on fetches that supply entity keys or fields needed by @requires.
   */
  private def addFetchDependencies(
    fetches: List[EntityFetch],
    roots: List[RootFetch]
  ): Either[PlanningFailure, List[EntityFetch]] =
    if (!fetches.exists(_.mayNeedPrerequisiteFetches)) Right(fetches)
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

      traverseEither(fetches) { fetch =>
        if (!fetch.mayNeedPrerequisiteFetches) Right(fetch)
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
          if (provided(fetch).exists(path => !providedElsewhere(path))) Left(DependencyCycle)
          else {
            val dependencies = fetches.iterator
              .filter(candidate => candidate.id != fetch.id && candidate.root == fetch.root)
              .filterNot(candidate => dependsOn(candidate, fetch.id, Set.empty))
              .filter(provided(_).nonEmpty)
              .map(_.id)
              .toSet
            Right(fetch.copy(dependencies = fetch.dependencies ++ dependencies))
          }
        }
      }
    }

  private def mergeEquivalentFetches(fetches: List[EntityFetch]): List[EntityFetch] = {
    val grouped  = mutable.LinkedHashMap.empty[EntityFetch, EntityFetch]
    val replaced = mutable.HashMap.empty[FetchId, FetchId]
    fetches.foreach { fetch =>
      val key = fetch.copy(
        id = FetchId(0),
        dependencies = fetch.dependencies.map(id => replaced.getOrElse(id, id)),
        fields = Nil,
        mayNeedPrerequisiteFetches = false
      )
      grouped.get(key) match {
        case None           => grouped.put(key, fetch)
        case Some(existing) =>
          replaced.update(fetch.id, existing.id)
          grouped.update(
            key,
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
        grouped.valuesIterator
          .map(fetch => fetch.copy(dependencies = fetch.dependencies.map(id => replaced.getOrElse(id, id))))
          .toList
      )
  }

  private val DependencyCycle: PlanningFailure = PlanningFailure.Rejected("Entity routing dependency cycle detected.")

  private final case class FieldKey(
    responseName: String,
    name: String,
    arguments: Map[String, InputValue],
    targets: Option[Set[String]]
  )
}
