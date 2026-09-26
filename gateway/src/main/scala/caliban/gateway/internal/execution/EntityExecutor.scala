package caliban.gateway.internal.execution

import caliban.{ CalibanError, InputValue, PathValue, ResponseValue }
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.execution.EntityExecutor._
import caliban.gateway.internal.planning.OperationPlan._
import caliban.gateway.traverseOption
import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue }
import caliban.parsing.adt.OperationType
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Scala3Annotations.threadUnsafe
import caliban.Value.{ EnumValue, FloatValue, IntValue, NullValue, StringValue }
import zio.{ Trace, URIO, ZIO }

import scala.collection.compat._
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
    blocked: FetchPaths,
    cache: PlanExecutionCache
  )(implicit trace: Trace): URIO[R, List[EntityResult]] = {
    val grouped = mutable.LinkedHashMap.empty[EntityGroupKey, mutable.ListBuffer[EntityFetch]]
    fetches.foreach { fetch =>
      grouped.getOrElseUpdate(fetch.groupKey, mutable.ListBuffer.empty) += fetch
    }
    val wave    = Wave(blocked, collectCandidates(fetches, roots), cache)
    grouped.values.map(_.toList).toList match {
      case group :: Nil => executeGroup(group, wave).map(_ :: Nil)
      case groups       =>
        val (combined, separate) = groups.zipWithIndex.partition { case (group, _) => canCombine(group.head) }
        val tasks                =
          separate.map { case (group, index) =>
            executeGroup(group, wave).map(result => List(index -> result))
          } ::: combined.groupBy { case (group, _) => group.head.source }.toList.map { case (source, sourceGroups) =>
            executeCombined(source, sourceGroups, wave)
          }
        ZIO.collectAllPar(tasks).map(_.flatten.sortBy(_._1).map(_._2))
    }
  }

  private val lookups = new EntityLookup(graph)

  private def collectCandidates(fetches: List[EntityFetch], roots: Map[FetchId, ResponseValue]): Candidates = {
    val candidates = mutable.HashMap.empty[CandidatePath, List[(List[PathValue], ResponseValue)]]
    fetches.foreach { fetch =>
      (fetch.mergePath :: fetch.contextArguments.map(_.sourcePath)).foreach { path =>
        val key = CandidatePath(fetch.root, path)
        if (!candidates.contains(key)) {
          val collected = new mutable.ListBuffer[(List[PathValue], ResponseValue)]
          collectEntities(roots.getOrElse(fetch.root, NullValue), path.toList, Nil, collected)
          candidates.put(key, collected.toList)
        }
      }
    }
    candidates
  }

  private def canCombine(fetch: EntityFetch): Boolean =
    fetch.contextArguments.isEmpty && fetch.lookup.operation == ComposedGraph.LookupOperation.FederationEntities

  private def executeCombined(source: String, groups: List[(List[EntityFetch], Int)], wave: Wave)(implicit
    trace: Trace
  ): URIO[R, List[(Int, EntityResult)]] = {
    val prepared         = groups.map { case (fetches, index) => PreparedGroup(index, fetches, prepareBatch(fetches, wave)) }
    val (pending, empty) = prepared.partition(_.batch.entries.nonEmpty)
    val emptyResults     = empty.map(group => group.index -> unfetchedResult(group.batch))
    val executed         = pending match {
      case Nil          => ZIO.succeed(Nil)
      case group :: Nil =>
        executeBatch(group.fetches, group.batch, wave).map(result => List(group.index -> result))
      case _            => executeParts(source, pending, wave)
    }
    executed.map(_ ::: emptyResults)
  }

  private def executeParts(source: String, groups: List[PreparedGroup], wave: Wave)(implicit
    trace: Trace
  ): URIO[R, List[(Int, EntityResult)]] = {
    val parts    = groups.zipWithIndex.map { case (group, slot) =>
      val (part, call) = lookups.preparePart(group.fetches.head, group.batch, wave.cache, slot)
      PreparedPart(group, call, part)
    }
    val executor = subgraphExecutors(source)
    executor
      .execute(EntityLookup.combine(parts.map(_.part)), OperationType.Query)
      .map { response =>
        parts.map { case PreparedPart(group, call, part) =>
          group.index -> call.complete(EntityLookup.partResponse(response, part), executor.errorPolicy)
        }
      }
      .catchAll {
        case SubgraphExecutor.RequestTooLarge =>
          ZIO.foreachPar(groups) { group =>
            executeBatch(group.fetches, group.batch, wave).map(group.index -> _)
          }
        case _                                =>
          ZIO.succeed(groups.map(group => group.index -> failure(group.fetches, group.batch)))
      }
  }

  private def executeGroup(fetches: List[EntityFetch], wave: Wave)(implicit trace: Trace): URIO[R, EntityResult] = {
    val batch = prepareBatch(fetches, wave)

    if (batch.entries.isEmpty) ZIO.succeed(unfetchedResult(batch))
    else if (fetches.forall(_.contextArguments.isEmpty))
      executeBatch(fetches, batch, wave)
    else {
      val batches =
        batch.entries.groupBy(_.contextArguments).values.toList.map(EntityBatch(_, Nil, Map.empty, Map.empty))
      ZIO.foreachPar(batches)(executeBatch(fetches, _, wave)).map { results =>
        EntityResult(
          results.flatMap(_.patches),
          batch.errors ::: results.flatMap(_.errors),
          unionBlocked(batch.blocked, results.flatMap(_.blocked)),
          unionBlocked(batch.unmatched, results.flatMap(_.unmatched))
        )
      }
    }
  }

  private def executeBatch(fetches: List[EntityFetch], batch: EntityBatch, wave: Wave)(implicit
    trace: Trace
  ): URIO[R, EntityResult] =
    lookups.prepare(fetches.head, batch, wave.cache) match {
      case Some((request, call)) =>
        val executor = subgraphExecutors(fetches.head.source)
        executor
          .execute(request, OperationType.Query)
          .map(call.complete(_, executor.errorPolicy))
          .catchAll(_ => ZIO.succeed(failure(fetches, batch)))
      case None                  => ZIO.succeed(failure(fetches, batch))
    }

  private def prepareBatch(fetches: List[EntityFetch], wave: Wave): EntityBatch = {
    val candidates = wave.candidates
    val entries    = mutable.LinkedHashMap.empty[Representation, mutable.ListBuffer[EntityLocation]]
    val errors     = mutable.ListBuffer.empty[CalibanError]
    val blocked    = mutable.ListBuffer.empty[EntityLocation]
    val unmatched  = mutable.ListBuffer.empty[EntityLocation]

    def block(fetch: EntityFetch, path: List[PathValue]): Unit = blocked += EntityLocation(fetch, path)

    fetches.foreach { fetch =>
      val identitySelections = IdentitySelections(fetch.keys.map(key => CorrelationKey(key.field, key)), fetch.typename)
      val blockedPaths       = PathIndex(
        fetch.dependencies.iterator.flatMap(dependency => wave.blocked.getOrElse(dependency, Set.empty).iterator)
      )
      candidates.getOrElse(CandidatePath(fetch.root, fetch.mergePath), Nil).foreach {
        case (_, NullValue)           => ()
        case (path, obj: ObjectValue) =>
          if (blockedPaths.containsPrefixOf(path))
            block(fetch, path)
          else if (hasUnmatchedType(fetch, obj))
            unmatched += EntityLocation(fetch, path)
          else
            sourceRepresentation(fetch, path, obj, candidates, identitySelections) match {
              case Some(representation) =>
                entries.getOrElseUpdate(representation, mutable.ListBuffer.empty) += EntityLocation(fetch, path)
              case None                 =>
                errors += missingRepresentation(fetch, path)
                block(fetch, path)
            }
        case (path, _)                =>
          errors += missingRepresentation(fetch, path)
          block(fetch, path)
      }
    }

    EntityBatch(
      entries.iterator.map { case (representation, locations) =>
        EntityBatchEntry(
          representation.identity,
          representation.requirements.toMap,
          representation.contextArguments.toMap,
          locations.toList
        )
      }.toVector,
      errors.toList,
      blockLocations(Map.empty, blocked),
      blockLocations(Map.empty, unmatched)
    )
  }

  private def hasUnmatchedType(fetch: EntityFetch, value: ObjectValue): Boolean =
    fetch.typename.exists { selection =>
      value.getOrNull(selection.responseName) match {
        case StringValue(runtimeType) => !graph.acceptsRuntimeType(fetch.entityType, runtimeType)
        case _                        => false
      }
    }

  private def sourceRepresentation(
    fetch: EntityFetch,
    path: List[PathValue],
    value: ObjectValue,
    candidates: Candidates,
    identitySelections: IdentitySelections
  ): Option[Representation] = {
    val fields = IndexedFields(value)
    for {
      identity     <- identitySelections.read(fetch.entityType, fields)
      requirements <- readRequirements(fetch.requirements, identity.typename, fields)
      contexts     <- readContextArguments(fetch, path, candidates)
    } yield Representation(identity, requirements, contexts)
  }

  private def readContextArguments(
    fetch: EntityFetch,
    entityPath: List[PathValue],
    candidates: Candidates
  ): Option[List[(ContextualArgument, InputValue)]] =
    traverseOption(fetch.contextArguments) { argument =>
      // A nested context shadows its ancestors; keep the last match when depths are equal.
      val source = candidates
        .getOrElse(CandidatePath(fetch.root, argument.sourcePath), Nil)
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

  private def collectEntities(
    value: ResponseValue,
    fields: List[String],
    reversedPath: List[PathValue],
    collected: mutable.ListBuffer[(List[PathValue], ResponseValue)]
  ): Unit =
    value match {
      case obj: ObjectValue if fields ne Nil =>
        val nested = obj.getOrNull(fields.head)
        if (nested ne null) collectEntities(nested, fields.tail, PathValue.Key(fields.head) :: reversedPath, collected)
      case ListValue(values)                 =>
        var index     = 0
        var remaining = values
        while (remaining ne Nil) {
          collectEntities(remaining.head, fields, PathValue.Index(index) :: reversedPath, collected)
          index += 1
          remaining = remaining.tail
        }
      case NullValue                         => ()
      case other                             => collected += (reversedPath.reverse -> other)
    }

  private def unfetchedResult(batch: EntityBatch): EntityResult =
    EntityResult(Nil, batch.errors, batch.blocked, batch.unmatched)

  private def failure(fetches: List[EntityFetch], batch: EntityBatch): EntityResult =
    EntityResult(
      Nil,
      batch.errors ::: fetches.map(fetch => RemoteError.at(fetchPath(fetch))),
      blockEntries(batch.blocked, batch.entries),
      batch.unmatched
    )

  private def missingRepresentation(fetch: EntityFetch, path: List[PathValue]): CalibanError.ExecutionError =
    CalibanError.ExecutionError(s"Entity key '${entityKey(fetch)}' was missing from the source result.", path = path)
}

private[gateway] object EntityExecutor {
  final case class EntityPatch(fetch: EntityFetch, path: List[PathValue], value: ResponseValue)

  type FetchPaths = Map[FetchId, Set[List[PathValue]]]

  final case class EntityResult(
    patches: List[EntityPatch],
    errors: List[CalibanError],
    blocked: FetchPaths,
    unmatched: FetchPaths
  )

  private[execution] final case class CorrelationKey(keyField: String, selection: RequiredSelection)

  private[execution] final case class IdentitySelections(
    keys: List[CorrelationKey],
    typename: Option[RequiredSelection]
  ) {
    def read(entityType: String, fields: IndexedFields): Option[EntityIdentity] = {
      val runtimeType = typename match {
        case Some(selection) => fields.get(selection.responseName).collect { case StringValue(value) => value }
        case None            => Some(entityType)
      }
      runtimeType.flatMap(runtimeType =>
        traverseOption(keys)(key =>
          fields.get(key.selection.responseName).flatMap(selectedInput(key.selection, _)).map(key.keyField -> _)
        ).map(EntityIdentity(runtimeType, _))
      )
    }
  }

  private[execution] def blockEntries(blocked: FetchPaths, entries: Iterable[EntityBatchEntry]): FetchPaths =
    blockLocations(blocked, entries.flatMap(_.locations))

  private def blockLocations(blocked: FetchPaths, locations: Iterable[EntityLocation]): FetchPaths =
    unionBlocked(blocked, locations.groupMap(_.fetch.id)(_.path))

  private[execution] def unionBlocked(
    blocked: FetchPaths,
    additions: Iterable[(FetchId, Iterable[List[PathValue]])]
  ): FetchPaths =
    additions.foldLeft(blocked) { case (values, (fetchId, paths)) =>
      values.updated(fetchId, values.getOrElse(fetchId, Set.empty) ++ paths)
    }

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

  private[execution] final case class EntityLocation(fetch: EntityFetch, path: List[PathValue])

  private[execution] final case class EntityBatchEntry(
    identity: EntityIdentity,
    requirements: Map[String, InputValue],
    contextArguments: Map[ContextualArgument, InputValue],
    locations: List[EntityLocation]
  )

  private[execution] final case class EntityBatch(
    entries: Vector[EntityBatchEntry],
    errors: List[CalibanError],
    blocked: FetchPaths,
    unmatched: FetchPaths
  )

  private final case class Wave(blocked: FetchPaths, candidates: Candidates, cache: PlanExecutionCache)

  private final case class CandidatePath(root: FetchId, path: Vector[String])

  private type Candidates = collection.Map[CandidatePath, List[(List[PathValue], ResponseValue)]]

  private final case class PreparedGroup(index: Int, fetches: List[EntityFetch], batch: EntityBatch)

  private final case class PreparedPart(group: PreparedGroup, call: EntityLookup#Call, part: EntityLookup.CallPart)

  private final case class Representation(
    identity: EntityIdentity,
    requirements: List[(String, InputValue)],
    contextArguments: List[(ContextualArgument, InputValue)]
  ) {
    @transient @threadUnsafe
    final override lazy val hashCode: Int =
      31 * namedValuesHash(identity.hashCode, requirements) + contextArguments.hashCode

    final override def equals(other: Any): Boolean =
      other match {
        case that: Representation =>
          (this eq that) ||
          (identity == that.identity && namedValuesEqual(requirements, that.requirements) &&
            contextArguments == that.contextArguments)
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

  // Local and remote subgraphs can encode the same key as Long vs Int or enum vs string.
  private def comparableValue(value: InputValue): InputValue =
    value match {
      case int: IntValue            => IntValue(int.toBigInt)
      case float: FloatValue        => FloatValue(float.toBigDecimal)
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
