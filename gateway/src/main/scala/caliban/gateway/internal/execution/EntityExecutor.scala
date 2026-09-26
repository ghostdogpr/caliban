package caliban.gateway.internal.execution

import caliban.{ CalibanError, InputValue, PathValue, ResponseValue }
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.execution.EntityExecutor._
import caliban.gateway.internal.execution.ResponseMerge.{ Fill, Patch }
import caliban.gateway.internal.planning.OperationPlan._
import caliban.gateway.traverseOption
import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue }
import caliban.parsing.adt.OperationType
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Scala3Annotations.threadUnsafe
import caliban.Value.{ EnumValue, FloatValue, IntValue, NullValue, StringValue }
import zio.{ Trace, URIO, ZIO }

import scala.collection.mutable

/**
 * Groups compatible entity fetches, batches source representations, and executes the resulting lookup calls.
 */
private[gateway] final class EntityExecutor[-R](
  graph: ComposedGraph,
  subgraphExecutors: Map[String, SubgraphExecutor[R]]
) {
  def execute(
    fetches: List[EntityFetch],
    roots: Map[FetchId, ResponseValue],
    blocked: List[EntityLocation],
    cache: PlanExecutionCache
  )(implicit trace: Trace): URIO[R, EntityResult] = {
    val grouped                = mutable.LinkedHashMap.empty[EntityGroupKey, mutable.ListBuffer[EntityFetch]]
    fetches.foreach { fetch =>
      grouped.getOrElseUpdate(fetch.groupKey, mutable.ListBuffer.empty) += fetch
    }
    val candidates             = new Candidates(roots)
    val groups                 = grouped.values.toList.zipWithIndex.map { case (group, index) =>
      index -> prepareGroup(group.toList, blocked, candidates)
    }
    val batches                = groups.flatMap { case (index, group) => group.batches.map(index -> _) }
    val (combinable, separate) = batches.partition { case (_, batch) => canCombine(batch.fetch) }
    val tasks                  =
      (separate.map(_ :: Nil) ::: combinable.groupBy { case (_, batch) => batch.fetch.source }.values.toList).map {
        case (index, batch) :: Nil => executeBatch(batch, cache).map(result => List(index -> result))
        case parts                 => executeParts(parts, cache)
      }
    ZIO.collectAllPar(tasks).map { results =>
      val prepared = groups.map { case (index, group) => index -> group.result }
      EntityResult.concat((prepared ::: results.flatten).sortBy(_._1).map(_._2))
    }
  }

  private val lookups = new EntityLookup(graph)

  private def canCombine(fetch: EntityFetch): Boolean =
    fetch.lookup.operation == ComposedGraph.LookupOperation.FederationEntities

  private def executeParts[K](batches: List[(K, EntityBatch)], cache: PlanExecutionCache)(implicit
    trace: Trace
  ): URIO[R, List[(K, EntityResult)]] = {
    val parts    = batches.zipWithIndex.map { case ((key, batch), slot) => key -> lookups.preparePart(batch, cache, slot) }
    val aliases  = parts.iterator.map { case (_, (part, _)) => part.alias }.toSet
    val executor = subgraphExecutors(batches.head._2.fetch.source)
    executor
      .execute(EntityLookup.combine(parts.map { case (_, (part, _)) => part }), OperationType.Query)
      .map { response =>
        parts.map { case (key, (part, call)) =>
          key -> call.complete(EntityLookup.partResponse(response, part, aliases), executor.errorPolicy)
        }
      }
      .catchAll {
        case SubgraphExecutor.RequestTooLarge =>
          ZIO.foreachPar(batches) { case (key, batch) => executeBatch(batch, cache).map(key -> _) }
        case _                                =>
          ZIO.succeed(batches.map { case (key, batch) => key -> failure(batch) })
      }
  }

  private def executeBatch(batch: EntityBatch, cache: PlanExecutionCache)(implicit
    trace: Trace
  ): URIO[R, EntityResult] =
    lookups.prepare(batch, cache) match {
      case Some((request, call)) =>
        val executor = subgraphExecutors(batch.fetch.source)
        executor
          .execute(request, OperationType.Query)
          .map(call.complete(_, executor.errorPolicy))
          .catchAll(_ => ZIO.succeed(failure(batch)))
      case None                  => ZIO.succeed(failure(batch))
    }

  private def prepareGroup(
    fetches: List[EntityFetch],
    blocked: List[EntityLocation],
    candidates: Candidates
  ): PreparedGroup = {
    val batches   =
      mutable.LinkedHashMap.empty[
        Map[ContextualArgument, InputValue],
        mutable.LinkedHashMap[Representation, mutable.ListBuffer[EntityLocation]]
      ]
    val errors    = mutable.ListBuffer.empty[CalibanError]
    val blockedAt = mutable.ListBuffer.empty[EntityLocation]
    val patches   = mutable.ListBuffer.empty[(FetchId, Patch)]

    def unreadable(location: EntityLocation): Unit = {
      errors += missingRepresentation(location.fetch, location.path)
      blockedAt += location
    }

    fetches.foreach { fetch =>
      val blockedPaths = PathIndex(blocked.iterator.collect {
        case location if fetch.dependencies(location.fetch.id) => location.path
      })
      candidates.at(fetch.root, fetch.mergePath).foreach { case (path, value) =>
        val location = EntityLocation(fetch, path)
        if (blockedPaths.containsPrefixOf(path)) blockedAt += location
        else
          value match {
            case obj: ObjectValue =>
              val fields      = IndexedFields(obj)
              val runtimeType = fetch.typename.fold(Option(fetch.entityType))(selection =>
                fields.get(selection.responseName).collect { case StringValue(name) => name }
              )
              runtimeType match {
                case None                                                            => unreadable(location)
                case Some(name) if !graph.acceptsRuntimeType(fetch.entityType, name) => patches += location.nullPatch
                case Some(name)                                                      =>
                  sourceRepresentation(fetch, path, name, fields, candidates) match {
                    case Some((contexts, representation)) =>
                      batches
                        .getOrElseUpdate(contexts, mutable.LinkedHashMap.empty)
                        .getOrElseUpdate(representation, mutable.ListBuffer.empty) += location
                    case None                             => unreadable(location)
                  }
              }
            case _                => unreadable(location)
          }
      }
    }

    PreparedGroup(
      EntityResult(patches.toList, errors.toList, blockedAt.toList),
      batches.iterator.map { case (contexts, representations) =>
        val entries = representations.iterator.map { case (representation, locations) =>
          EntityBatchEntry(representation.identity, representation.requirements, locations.toList)
        }.toVector
        EntityBatch(fetches.filter(fetch => entries.exists(_.locations.exists(_.fetch eq fetch))), contexts, entries)
      }.toList
    )
  }

  private def sourceRepresentation(
    fetch: EntityFetch,
    path: List[PathValue],
    runtimeType: String,
    fields: IndexedFields,
    candidates: Candidates
  ): Option[(Map[ContextualArgument, InputValue], Representation)] =
    for {
      identity     <- readIdentity(runtimeType, fetch.keys, fields)
      requirements <- readRequirements(fetch.requirements, runtimeType, fields)
      contexts     <- readContextArguments(fetch, path, candidates)
    } yield contexts.toMap -> Representation(identity, requirements)

  private def readContextArguments(
    fetch: EntityFetch,
    entityPath: List[PathValue],
    candidates: Candidates
  ): Option[List[(ContextualArgument, InputValue)]] =
    traverseOption(fetch.contextArguments) { argument =>
      // A nested context shadows its ancestors; keep the last match when depths are equal.
      val source = candidates
        .at(fetch.root, argument.sourcePath)
        .foldLeft(Option.empty[(List[PathValue], ObjectValue)]) {
          case (nearest, (path, value: ObjectValue)) if entityPath.startsWith(path) =>
            if (nearest.forall(_._1.size <= path.size)) Some(path -> value) else nearest
          case (nearest, _)                                                         => nearest
        }
        .map(_._2)
      source.flatMap { value =>
        val fields = IndexedFields(value)
        val names  = argument.projection match {
          case ContextProjection.Path(names)                        => names
          case ContextProjection.ByType(typenameAlias, pathsByType) =>
            val runtimeType = fields.get(typenameAlias) match {
              case Some(StringValue(name)) => name
              case _                       => argument.sourceType
            }
            pathsByType.getOrElse(runtimeType, Nil)
        }
        projectContextInput(names, fields)
      }.map(argument -> _)
    }

  private def projectContextInput(names: List[String], fields: IndexedFields): Option[InputValue] =
    names match {
      case name :: rest => fields.get(name).flatMap(projectContextValue(rest, _))
      case Nil          => None
    }

  private def projectContextValue(names: List[String], value: ResponseValue): Option[InputValue] =
    if (value == NullValue) Some(NullValue)
    else if (names.isEmpty) responseInput(value)
    else
      value match {
        case obj: ObjectValue  => projectContextInput(names, IndexedFields(obj))
        case ListValue(values) => traverseOption(values)(projectContextValue(names, _)).map(InputListValue.apply)
        case _                 => None
      }

  private def readRequirements(
    requirements: List[RequiredSelection],
    runtimeType: String,
    value: IndexedFields
  ): Option[List[(String, InputValue)]] =
    if (requirements.isEmpty) Some(Nil)
    else readSelections(requirements, value, allowNull = true)(appliesTo(_, runtimeType))

  private def failure(batch: EntityBatch): EntityResult =
    EntityResult(
      Nil,
      batch.mergePaths.map(RemoteError.at),
      batch.entries.iterator.flatMap(_.locations).toList
    )

  private def missingRepresentation(fetch: EntityFetch, path: List[PathValue]): CalibanError.ExecutionError =
    CalibanError.ExecutionError(s"Entity key '${entityKey(fetch)}' was missing from the source result.", path = path)
}

private[gateway] object EntityExecutor {

  /**
   * Blocked locations are filled with null and block dependent fetches.
   */
  final case class EntityResult(
    patches: List[(FetchId, Patch)],
    errors: List[CalibanError],
    blocked: List[EntityLocation]
  )

  object EntityResult {
    def concat(results: List[EntityResult]): EntityResult =
      EntityResult(
        results.flatMap(_.patches),
        results.flatMap(_.errors),
        results.flatMap(_.blocked)
      )
  }

  private[execution] def readIdentity(
    runtimeType: String,
    keys: List[RequiredSelection],
    fields: IndexedFields
  ): Option[EntityIdentity] =
    traverseOption(keys)(key => fields.get(key.responseName).flatMap(selectedInput(key, _)).map(key.field -> _))
      .map(EntityIdentity(runtimeType, _))

  private[execution] def entityKey(fetch: EntityFetch): String =
    s"${fetch.entityType}(${fetch.keys.map(_.field).mkString(", ")})"

  private[execution] def fetchPath(fetch: EntityFetch): List[PathValue] =
    fetch.mergePath.iterator.map(PathValue.Key(_)).toList

  private[execution] final case class EntityIdentity(typename: String, keys: List[(String, InputValue)]) {
    @transient @threadUnsafe
    final override lazy val hashCode: Int = namedValuesHash(typename.hashCode, keys)

    final override def equals(other: Any): Boolean =
      other match {
        case that: EntityIdentity => (this eq that) || (typename == that.typename && namedValuesEqual(keys, that.keys))
        case _                    => false
      }
  }

  private[execution] final case class EntityLocation(fetch: EntityFetch, path: List[PathValue]) {
    def nullPatch: (FetchId, Patch) = fetch.root -> (path -> Fill(RemoteError.nullObject(fetch.fields)))
  }

  private[execution] final case class EntityBatchEntry(
    identity: EntityIdentity,
    requirements: List[(String, InputValue)],
    locations: List[EntityLocation]
  )

  private[execution] final case class EntityBatch(
    fetches: List[EntityFetch],
    contexts: Map[ContextualArgument, InputValue],
    entries: Vector[EntityBatchEntry]
  ) {
    def fetch: EntityFetch = fetches.head

    lazy val mergePaths: List[List[PathValue]] = fetches.map(fetchPath).distinct
  }

  private final case class CandidatePath(root: FetchId, path: Vector[String])

  private def collectEntities(
    value: ResponseValue,
    fields: List[String],
    reversedPath: List[PathValue],
    collected: mutable.ListBuffer[(List[PathValue], ResponseValue)]
  ): Unit =
    value match {
      case obj: ObjectValue if fields ne Nil                =>
        val nested = obj.getOrNull(fields.head)
        if (nested ne null) collectEntities(nested, fields.tail, PathValue.Key(fields.head) :: reversedPath, collected)
      case ListValue(values)                                =>
        var index     = 0
        var remaining = values
        while (remaining ne Nil) {
          collectEntities(remaining.head, fields, PathValue.Index(index) :: reversedPath, collected)
          index += 1
          remaining = remaining.tail
        }
      case other if (fields eq Nil) && (other ne NullValue) => collected += (reversedPath.reverse -> other)
      case _                                                => ()
    }

  private final class Candidates(roots: Map[FetchId, ResponseValue]) {
    private val collected = mutable.HashMap.empty[CandidatePath, List[(List[PathValue], ResponseValue)]]

    def at(root: FetchId, path: Vector[String]): List[(List[PathValue], ResponseValue)] =
      collected.getOrElseUpdate(
        CandidatePath(root, path), {
          val buffer = new mutable.ListBuffer[(List[PathValue], ResponseValue)]
          collectEntities(roots.getOrElse(root, NullValue), path.toList, Nil, buffer)
          buffer.toList
        }
      )
  }

  private final case class PreparedGroup(result: EntityResult, batches: List[EntityBatch])

  private final case class Representation(identity: EntityIdentity, requirements: List[(String, InputValue)]) {
    @transient @threadUnsafe
    final override lazy val hashCode: Int = namedValuesHash(identity.hashCode, requirements)

    final override def equals(other: Any): Boolean =
      other match {
        case that: Representation =>
          (this eq that) || (identity == that.identity && namedValuesEqual(requirements, that.requirements))
        case _                    => false
      }
  }

  private def namedValuesHash(seed: Int, values: List[(String, InputValue)]): Int = {
    var hash      = seed
    var remaining = values
    while (remaining ne Nil) {
      val head = remaining.head
      hash = hash * 31 + head._1.hashCode
      hash = hash * 31 + comparableValue(head._2).hashCode
      remaining = remaining.tail
    }
    hash
  }

  private def namedValuesEqual(left: List[(String, InputValue)], right: List[(String, InputValue)]): Boolean = {
    var remainingLeft  = left
    var remainingRight = right
    while ((remainingLeft ne Nil) && (remainingRight ne Nil)) {
      val leftHead  = remainingLeft.head
      val rightHead = remainingRight.head
      if (
        leftHead._1 != rightHead._1 ||
        (leftHead._2 != rightHead._2 && comparableValue(leftHead._2) != comparableValue(rightHead._2))
      ) return false
      remainingLeft = remainingLeft.tail
      remainingRight = remainingRight.tail
    }
    (remainingLeft eq Nil) && (remainingRight eq Nil)
  }

  // Local and remote subgraphs can encode the same key as Long vs Int, 1 vs 1.0 or enum vs string.
  private def comparableValue(value: InputValue): InputValue =
    value match {
      case int: IntValue            => IntValue(int.toBigInt)
      case float: FloatValue        =>
        val decimal = float.toBigDecimal
        if (decimal.isWhole) IntValue(decimal.toBigInt) else FloatValue(decimal)
      case EnumValue(name)          => StringValue(name)
      case InputListValue(values)   => InputListValue(values.map(comparableValue))
      case InputObjectValue(fields) =>
        InputObjectValue(fields.map { case (name, nested) => name -> comparableValue(nested) })
      case other                    => other
    }

  private def selectedInput(
    selection: RequiredSelection,
    value: ResponseValue,
    allowNull: Boolean = false
  ): Option[InputValue] =
    if (value == NullValue) if (allowNull) Some(NullValue) else None
    else if (selection.children.isEmpty) responseInput(value)
    else
      value match {
        case obj: ObjectValue  =>
          selectedObject(selection.children, selection.typenameAlias, obj, allowNull)
        case ListValue(values) =>
          traverseOption(values)(selectedInput(selection, _, allowNull)).map(InputListValue.apply)
        case _                 => None
      }

  private def responseInput(value: ResponseValue): Option[InputValue] =
    value match {
      case input: InputValue   => Some(input)
      case ObjectValue(fields) =>
        traverseOption(fields) { case (name, nested) => responseInput(nested).map(name -> _) }
          .map(values => InputObjectValue(values.toMap))
      case ListValue(values)   => traverseOption(values)(responseInput).map(InputListValue.apply)
      case _                   => None
    }

  private def selectedObject(
    selections: List[RequiredSelection],
    typenameAlias: Option[String],
    value: ObjectValue,
    allowNull: Boolean
  ): Option[InputObjectValue] = {
    val fields     = IndexedFields(value)
    val applicable = typenameAlias match {
      case None        => Some((_: RequiredSelection) => true)
      case Some(alias) =>
        fields.get(alias).collect { case StringValue(runtimeType) => appliesTo(_: RequiredSelection, runtimeType) }
    }
    applicable
      .flatMap(readSelections(selections, fields, allowNull)(_))
      .map(values => InputObjectValue(values.toMap))
  }

  private def readSelections(selections: List[RequiredSelection], fields: IndexedFields, allowNull: Boolean)(
    applicable: RequiredSelection => Boolean
  ): Option[List[(String, InputValue)]] = {
    val collected = List.newBuilder[(String, InputValue)]
    var remaining = selections
    while (remaining ne Nil) {
      val selection = remaining.head
      if (applicable(selection)) {
        fields.get(selection.responseName).flatMap(selectedInput(selection, _, allowNull)) match {
          case Some(result) => collected += (selection.field -> result)
          case None         => return None
        }
      }
      remaining = remaining.tail
    }
    Some(collected.result())
  }

  private def appliesTo(selection: RequiredSelection, runtimeType: String): Boolean =
    selection.conditions.forall(_.contains(runtimeType))

}
