package caliban.gateway.internal.planning

import caliban.InputValue
import caliban.execution.Field
import caliban.gateway.internal.planning.CandidateSearch.PlanningFailure
import caliban.gateway.internal.planning.OperationPlan._

import scala.annotation.tailrec
import scala.collection.mutable

private[planning] object FetchGraphOptimizer {
  def optimize(
    roots: List[RootFetch],
    fetches: List[EntityFetch],
    needPrerequisites: Set[FetchId]
  ): List[EntityFetch] =
    mergeEquivalentFetches(addFetchDependencies(fetches, roots, needPrerequisites))

  /**
   * Orders entity fetches into dependency waves, starting with the root fetches completed.
   * Returns the waves, or the cycle that prevents ordering the fetches.
   */
  def waves(roots: List[RootFetch], fetches: List[EntityFetch]): Either[PlanningFailure, List[List[EntityFetch]]] = {
    @tailrec
    def order(
      pending: List[EntityFetch],
      completed: Set[FetchId],
      waves: List[List[EntityFetch]]
    ): Either[PlanningFailure, List[List[EntityFetch]]] =
      if (pending.isEmpty) Right(waves.reverse)
      else {
        val (ready, waiting) = pending.partition(_.dependencies.forall(completed))
        if (ready.isEmpty) Left(DependencyCycle)
        else order(waiting, completed ++ ready.iterator.map(_.id), ready :: waves)
      }

    order(fetches, roots.iterator.map(_.id).toSet, Nil)
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
    roots: List[RootFetch],
    needPrerequisites: Set[FetchId]
  ): List[EntityFetch] =
    if (needPrerequisites.isEmpty) fetches
    else {
      val byId = fetches.iterator.map(fetch => fetch.id -> fetch).toMap

      def dependsOn(fetch: EntityFetch, dependency: FetchId): Boolean                                          =
        fetch.dependencies.exists(id => id == dependency || byId.get(id).exists(dependsOn(_, dependency)))
      def selectionPaths(prefix: Vector[String])(selection: RequiredSelection): List[Vector[String]]           = {
        val path = prefix :+ selection.responseName
        if (selection.children.isEmpty) path :: Nil
        else selection.children.flatMap(selectionPaths(path))
      }
      def targetedPaths(prefix: Vector[String])(field: Field): List[(Vector[String], List[Set[String]])]       = {
        val path    = prefix :+ field.aliasedName
        val targets = field.targets.toList
        if (field.fields.isEmpty) (path, targets) :: Nil
        else
          field.fields.flatMap(targetedPaths(path)).map { case (nested, conditions) =>
            (nested, targets ::: conditions)
          }
      }
      def provides(paths: List[(Vector[String], List[Set[String]])], path: Vector[String], entityType: String) =
        paths.exists { case (candidate, targets) => candidate == path && targets.forall(_.contains(entityType)) }

      val providers = fetches.map(fetch => fetch -> fetch.fields.flatMap(targetedPaths(fetch.mergePath)))
      val rootPaths = roots.iterator.map(root => root.id -> root.downstream.flatMap(targetedPaths(Vector.empty))).toMap

      providers.map { case (fetch, own) =>
        if (!needPrerequisites(fetch.id)) fetch
        else {
          val required                                         =
            (fetch.keys ::: fetch.requirements).flatMap(selectionPaths(fetch.mergePath)).toSet ++
              fetch.contextArguments.flatMap(argument => argument.projection.paths.map(argument.sourcePath ++ _))
          val siblings                                         =
            providers.filter { case (other, _) => other.id != fetch.id && other.root == fetch.root }
          def providedElsewhere(path: Vector[String]): Boolean =
            provides(rootPaths.getOrElse(fetch.root, Nil), path, fetch.entityType) ||
              siblings.exists { case (_, paths) => provides(paths, path, fetch.entityType) }
          // A fetch that needs its own output depends on itself, which waves rejects as a cycle.
          val needsItself                                      = own.exists { case (path, _) => required(path) && !providedElsewhere(path) }
          val dependencies                                     = siblings.iterator.collect {
            case (candidate, paths) if !dependsOn(candidate, fetch.id) && paths.exists { case (path, _) =>
                  required(path)
                } =>
              candidate.id
          }.toSet
          fetch.copy(dependencies = fetch.dependencies ++ dependencies ++ (if (needsItself) Set(fetch.id) else Nil))
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
        fields = Nil
      )
      grouped.get(key) match {
        case None           => grouped.put(key, fetch)
        case Some(existing) =>
          replaced.update(fetch.id, existing.id)
          grouped.update(key, existing.copy(fields = mergeFields(existing.fields ::: fetch.fields)))
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
