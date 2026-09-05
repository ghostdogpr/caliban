package caliban.gateway.internal.execution

import caliban.{ CalibanError, GraphQLRequest, InputValue, PathValue, ResponseValue }
import caliban.gateway.internal.composition.ComposedGraph
import caliban.gateway.internal.execution.EntityExecutor._
import caliban.gateway.internal.planning.OperationPlan._
import caliban.gateway.traverseOption
import caliban.InputValue.{ ListValue => InputListValue, ObjectValue => InputObjectValue }
import caliban.parsing.adt.OperationType
import caliban.ResponseValue.{ ListValue, ObjectValue }
import caliban.Scala3Annotations.threadUnsafe
import caliban.Value.{ NullValue, StringValue }
import zio.{ Trace, URIO, ZIO }

import scala.collection.mutable

/**
 * Groups compatible entity fetches, batches source representations, and executes the resulting lookup calls.
 */
private[gateway] final class EntityExecutor[-R](
  graph: ComposedGraph,
  subgraphExecutors: Map[String, SubgraphExecutor[R]]
) {
  private val lookups = new EntityLookup(graph)

  private def cachedGroupKey(fetch: EntityFetch, cache: PlanExecutionCache): EntityGroupKey =
    PlanExecutionCache.memoize(cache.groupKeys, fetch.id)(entityGroupKey(fetch))

  def execute(
    fetches: List[EntityFetch],
    roots: Map[FetchId, ResponseValue],
    blocked: Map[FetchId, Set[List[PathValue]]],
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  )(implicit trace: Trace): URIO[R, List[EntityResult]] = {
    val grouped    = mutable.LinkedHashMap.empty[EntityGroupKey, mutable.ListBuffer[EntityFetch]]
    fetches.foreach { fetch =>
      val key = cachedGroupKey(fetch, cache)
      grouped.get(key) match {
        case Some(group) => group += fetch
        case None        => grouped.put(key, mutable.ListBuffer(fetch))
      }
    }
    val candidates = mutable.HashMap.empty[(FetchId, Vector[String]), List[(List[PathValue], ResponseValue)]]
    fetches.foreach { fetch =>
      (fetch.mergePath :: fetch.contextArguments.map(_.sourcePath)).foreach { path =>
        val key = fetch.root -> path
        if (!candidates.contains(key)) {
          val collected = new mutable.ListBuffer[(List[PathValue], ResponseValue)]
          entityCandidates(roots.getOrElse(fetch.root, NullValue), path.toList, Nil, collected)
          candidates.put(key, collected.toList)
        }
      }
    }
    grouped.values.toList match {
      case group :: Nil => executeGroup(group, blocked, candidates, resolvedRequest, cache).map(_ :: Nil)
      case groups       =>
        ZIO.foreachPar(groups)(group => executeGroup(group, blocked, candidates, resolvedRequest, cache))
    }
  }

  private def executeGroup(
    group: mutable.ListBuffer[EntityFetch],
    blocked: Map[FetchId, Set[List[PathValue]]],
    candidates: mutable.HashMap[(FetchId, Vector[String]), List[(List[PathValue], ResponseValue)]],
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  )(implicit trace: Trace): URIO[R, EntityResult] = {
    val fetch   = group.head
    val fetches = group.toList
    val batch   = prepareBatch(fetches, candidates, blocked)

    if (batch.entries.isEmpty) ZIO.succeed(EntityResult(Nil, batch.errors, batch.blocked))
    else if (fetches.forall(_.contextArguments.isEmpty))
      executeBatch(fetch, fetches, batch, resolvedRequest, cache)
    else {
      val grouped = batch.entries.groupBy(_.contextArguments).values.toList
      val batches = grouped.zipWithIndex.map { case (entries, index) =>
        batch.copy(
          entries = entries,
          errors = if (index == 0) batch.errors else Nil,
          blocked = if (index == 0) batch.blocked else Map.empty
        )
      }
      ZIO.foreachPar(batches)(executeBatch(fetch, fetches, _, resolvedRequest, cache)).map { results =>
        EntityResult(
          results.flatMap(_.patches),
          results.flatMap(_.errors),
          unionBlocked(Map.empty, results.flatMap(_.blocked))
        )
      }
    }
  }

  private def executeBatch(
    fetch: EntityFetch,
    fetches: List[EntityFetch],
    batch: EntityBatch,
    resolvedRequest: GraphQLRequest,
    cache: PlanExecutionCache
  )(implicit trace: Trace): URIO[R, EntityResult] = {
    def failure = EntityResult(
      Nil,
      batch.errors ::: fetches.map(fetch => RemoteError.at(fetchPath(fetch))),
      blockAll(batch)
    )

    lookups.prepare(fetch, batch, resolvedRequest, cache) match {
      case Some(lookup) =>
        subgraphExecutors.get(fetch.source) match {
          case Some(executor) =>
            executor
              .execute(lookup.request, OperationType.Query)
              .map(response => lookup.complete(response, executor.errorPolicy))
              .catchAll(_ => ZIO.succeed(failure))
          case None           => ZIO.succeed(failure)
        }
      case None         => ZIO.succeed(failure)
    }
  }

  private def prepareBatch(
    fetches: List[EntityFetch],
    candidates: mutable.HashMap[(FetchId, Vector[String]), List[(List[PathValue], ResponseValue)]],
    blocked: Map[FetchId, Set[List[PathValue]]]
  ): EntityBatch = {
    val entries                                               =
      mutable.LinkedHashMap.empty[Representation, mutable.ListBuffer[EntityLocation]]
    val errors                                                = mutable.ListBuffer.empty[CalibanError]
    val skipped                                               = mutable.Map.empty[FetchId, mutable.Set[List[PathValue]]]
    def skip(fetch: EntityFetch, path: List[PathValue]): Unit =
      skipped.getOrElseUpdate(fetch.id, mutable.Set.empty) += path

    fetches.foreach { fetch =>
      val identitySelections = IdentitySelections(fetch.keys.map(key => CorrelationKey(key.field, key)), fetch.typename)
      val blockedPaths       = PathIndex(
        fetch.dependencies.iterator.flatMap(dependency => blocked.getOrElse(dependency, Set.empty).iterator)
      )
      val objectType         = graph.isObjectType(fetch.entityType)
      candidates.getOrElse((fetch.root, fetch.mergePath), Nil).foreach {
        case (_, NullValue)           => ()
        case (path, obj: ObjectValue) =>
          if (blockedPaths.containsPrefixOf(path))
            skip(fetch, path)
          else if (
            objectType && fetch.typename.exists { selection =>
              obj.getOrNull(selection.responseName) match {
                case StringValue(runtimeType) => runtimeType != fetch.entityType
                case _                        => false
              }
            }
          )
            skip(fetch, path)
          else
            sourceRepresentation(fetch, path, obj, candidates, identitySelections) match {
              case Some(representation) =>
                entries.get(representation) match {
                  case Some(locations) => locations += EntityLocation(fetch, path)
                  case None            =>
                    entries.put(representation, mutable.ListBuffer(EntityLocation(fetch, path)))
                }
              case None                 =>
                errors += missingRepresentation(fetch, path)
                skip(fetch, path)
            }
        case (path, _)                =>
          errors += missingRepresentation(fetch, path)
          skip(fetch, path)
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
      skipped.iterator.map { case (fetchId, paths) => fetchId -> paths.toSet }.toMap
    )
  }

  private def sourceRepresentation(
    fetch: EntityFetch,
    path: List[PathValue],
    value: ObjectValue,
    candidates: mutable.HashMap[(FetchId, Vector[String]), List[(List[PathValue], ResponseValue)]],
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
    candidates: mutable.HashMap[(FetchId, Vector[String]), List[(List[PathValue], ResponseValue)]]
  ): Option[List[(ContextualArgument, InputValue)]] =
    traverseOption(fetch.contextArguments) { argument =>
      val source = candidates
        .getOrElse(fetch.root -> argument.sourcePath, Nil)
        .collect {
          case (path, value: ObjectValue) if entityPath.startsWith(path) =>
            path -> value
        }
        .sortBy(_._1.size)
        .lastOption
        .map(_._2)
      source.flatMap { value =>
        val fields      = IndexedFields(value)
        val runtimeType = argument.typename
          .flatMap(selection => fields.get(selection.responseName).collect { case StringValue(name) => name })
          .getOrElse(argument.sourceType)
        readContextInput(argument.selections, runtimeType, fields)
      }.map(argument -> _)
    }

  private def readContextInput(
    selections: List[RequiredSelection],
    runtimeType: String,
    value: IndexedFields
  ): Option[InputValue] =
    selections.filter(appliesTo(_, runtimeType)) match {
      case selection :: Nil => value.get(selection.responseName).flatMap(selectedContextInput(selection, _))
      case _                => None
    }

  private def selectedContextInput(selection: RequiredSelection, value: ResponseValue): Option[InputValue] =
    if (value == NullValue) Some(NullValue)
    else if (selection.children.isEmpty) responseInput(value)
    else
      value match {
        case obj: ObjectValue  =>
          val fields = IndexedFields(obj)
          selection.children match {
            case child :: Nil => fields.get(child.responseName).flatMap(selectedContextInput(child, _))
            case _            => None
          }
        case ListValue(values) =>
          traverseOption(values)(selectedContextInput(selection, _)).map(InputListValue.apply)
        case _                 => None
      }

  private def readRequirements(
    requirements: List[RequiredSelection],
    runtimeType: String,
    value: IndexedFields
  ): Option[List[(String, InputValue)]] =
    if (requirements.isEmpty) Some(Nil)
    else readSelections(requirements, value, allowNull = true)(appliesTo(_, runtimeType))

  private def entityCandidates(
    value: ResponseValue,
    fields: List[String],
    reversedPath: List[PathValue],
    collected: mutable.ListBuffer[(List[PathValue], ResponseValue)]
  ): Unit =
    value match {
      case ObjectValue(values) if fields ne Nil =>
        val head      = fields.head
        val tail      = fields.tail
        var remaining = values
        var found     = false
        while (!found && (remaining ne Nil)) {
          val field = remaining.head
          if (field._1 == head) {
            found = true
            entityCandidates(field._2, tail, PathValue.Key(head) :: reversedPath, collected)
          }
          remaining = remaining.tail
        }
      case ListValue(values)                    =>
        var index     = 0
        var remaining = values
        while (remaining ne Nil) {
          entityCandidates(remaining.head, fields, PathValue.Index(index) :: reversedPath, collected)
          index += 1
          remaining = remaining.tail
        }
      case NullValue                            => ()
      case other                                => collected += (reversedPath.reverse -> other)
    }

  private def blockAll(batch: EntityBatch): Map[FetchId, Set[List[PathValue]]] =
    blockEntries(batch.blocked, batch.entries)

  private def missingRepresentation(fetch: EntityFetch, path: List[PathValue]): CalibanError.ExecutionError =
    CalibanError.ExecutionError(
      s"Entity key '${entityKey(fetch)}' was missing from the source result.",
      path = path
    )
}

private[gateway] object EntityExecutor {
  final case class EntityPatch(fetch: EntityFetch, path: List[PathValue], value: ResponseValue)

  final case class EntityResult(
    patches: List[EntityPatch],
    errors: List[CalibanError],
    blocked: Map[FetchId, Set[List[PathValue]]]
  )

  private[internal] final case class CorrelationKey(keyField: String, selection: RequiredSelection)

  private[internal] final case class IdentitySelections(
    keys: List[CorrelationKey],
    typename: Option[RequiredSelection]
  ) {
    def read(entityType: String, value: IndexedFields): Option[EntityIdentity] = {
      val runtimeType = typename match {
        case Some(selection) =>
          value.get(selection.responseName) match {
            case Some(StringValue(value)) => value
            case _                        => null
          }
        case None            => entityType
      }
      if (runtimeType eq null) None
      else
        keys match {
          case key :: Nil =>
            value.get(key.selection.responseName).flatMap(selectedInput(key.selection, _)) match {
              case Some(input) => Some(EntityIdentity(runtimeType, (key.keyField -> input) :: Nil))
              case None        => None
            }
          case selections =>
            traverseOption(selections)(key =>
              value
                .get(key.selection.responseName)
                .flatMap(selectedInput(key.selection, _))
                .map(key.keyField -> _)
            )
              .map(EntityIdentity(runtimeType, _))
        }
    }
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
          selectedObject(selection.children, selection.runtimeTypeAlias, obj, allowNull)
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
    runtimeTypeAlias: Option[String],
    value: ObjectValue,
    allowNull: Boolean
  ): Option[InputObjectValue] = {
    val fields     = IndexedFields(value)
    val applicable = runtimeTypeAlias match {
      case None        => Some(selections)
      case Some(alias) =>
        fields.get(alias).collect { case StringValue(runtimeType) =>
          selections.filter(appliesTo(_, runtimeType))
        }
    }
    applicable.flatMap(values =>
      readSelections(values, fields, allowNull)(_ => true).map(values => InputObjectValue(values.toMap))
    )
  }

  private def readSelections(
    selections: List[RequiredSelection],
    fields: IndexedFields,
    allowNull: Boolean
  )(applicable: RequiredSelection => Boolean): Option[List[(String, InputValue)]] = {
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

  private[execution] def blockEntries(
    blocked: Map[FetchId, Set[List[PathValue]]],
    entries: Iterable[EntityBatchEntry]
  ): Map[FetchId, Set[List[PathValue]]] = {
    val additions = mutable.Map.empty[FetchId, mutable.Set[List[PathValue]]]
    entries.foreach(
      _.locations.foreach(location => additions.getOrElseUpdate(location.fetch.id, mutable.Set.empty) += location.path)
    )
    if (additions.isEmpty) blocked
    else
      additions.foldLeft(blocked) { case (result, (fetchId, paths)) =>
        result.updated(fetchId, result.getOrElse(fetchId, Set.empty) ++ paths)
      }
  }

  private[execution] def unionBlocked(
    blocked: Map[FetchId, Set[List[PathValue]]],
    additions: Iterable[(FetchId, Set[List[PathValue]])]
  ): Map[FetchId, Set[List[PathValue]]] =
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
      hash = hash * 31 + head._2.hashCode
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
      if (leftHead._1 != rightHead._1 || leftHead._2 != rightHead._2) return false
      remainingLeft = remainingLeft.tail
      remainingRight = remainingRight.tail
    }
    (remainingLeft eq Nil) && (remainingRight eq Nil)
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
    blocked: Map[FetchId, Set[List[PathValue]]]
  )
}
