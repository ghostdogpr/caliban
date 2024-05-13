package caliban.execution

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue._
import caliban.Value._
import caliban._
import caliban.execution.Fragment.IsDeferred
import caliban.parsing.adt._
import caliban.schema.ReducedStep.DeferStep
import caliban.schema.Step.{ PureStep => _, _ }
import caliban.schema.{ PureStep, ReducedStep, Step, Types }
import caliban.transformers.Transformer
import caliban.wrappers.Wrapper.FieldWrapper
import zio._
import zio.query._
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.stream.ZStream

import scala.annotation.{ switch, tailrec }
import scala.collection.compat.{ BuildFrom => _, _ }
import scala.collection.immutable.VectorBuilder
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

object Executor {

  /**
   * Executes the given query against a schema. It returns either an [[caliban.CalibanError.ExecutionError]] or a [[ResponseValue]].
   * @param request a request object containing all information needed
   * @param plan an execution plan
   * @param fieldWrappers a list of field wrappers
   * @param queryExecution a strategy for executing queries in parallel or not
   * @param makeCache effect used to create a new cache for the query execution
   */
  def executeRequest[R](
    request: ExecutionRequest,
    plan: Step[R],
    fieldWrappers: List[FieldWrapper[R]] = Nil,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    featureSet: Set[Feature] = Set.empty,
    makeCache: UIO[Cache] = Cache.empty(Trace.empty),
    transformer: Transformer[R] = Transformer.empty[R]
  )(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] = {
    val wrapPureValues      = fieldWrappers.exists(_.wrapPureValues)
    val stepReducer         =
      new StepReducer[R](
        transformer,
        request.operationType eq OperationType.Subscription,
        featureSet(Feature.Defer),
        wrapPureValues
      )
    val reducedStepExecutor =
      new ReducedStepExecutor[R](
        queryExecution,
        request.operationType eq OperationType.Mutation,
        fieldWrappers,
        wrapPureValues
      )

    def runQuery(step: ReducedStep[R], cache: Cache) =
      for {
        env          <- ZIO.environment[R]
        deferred     <- Ref.make(List.empty[Deferred[R]])
        errors       <- Ref.make(List.empty[CalibanError])
        result       <- reducedStepExecutor.makeQuery(step, errors, deferred).runCache(cache)
        resultErrors <- errors.get
        defers       <- deferred.get
      } yield
        if (defers.nonEmpty) {
          val stream = (makeDeferStream(defers, cache)
            .mapChunks(chunk => Chunk.single(GraphQLIncrementalResponse(chunk.toList, hasNext = true))) ++ ZStream
            .succeed(GraphQLIncrementalResponse.empty))
            .map(_.toResponseValue)
            .provideEnvironment(env)

          GraphQLResponse(
            StreamValue(ZStream.succeed(result) ++ stream),
            resultErrors.reverse,
            hasNext = Some(true)
          )
        } else GraphQLResponse(result, resultErrors.reverse, hasNext = None)

    def makeDeferStream(
      defers: List[Deferred[R]],
      cache: Cache
    ): ZStream[R, Nothing, Incremental[CalibanError]] = {
      def run(d: Deferred[R]) =
        ZStream.unwrap(runIncrementalQuery(d.step, cache, d.path, d.label).map {
          case (resp, Nil)  =>
            ZStream.succeed(resp)
          case (resp, more) =>
            ZStream.succeed(resp) ++ makeDeferStream(more, cache)
        })

      ZStream.mergeAllUnbounded()(defers.map(run): _*)
    }

    def runIncrementalQuery(
      step: ReducedStep[R],
      cache: Cache,
      path: List[PathValue],
      label: Option[String]
    ) =
      for {
        deferred     <- Ref.make(List.empty[Deferred[R]])
        errors       <- Ref.make(List.empty[CalibanError])
        result       <- reducedStepExecutor.makeQuery(step, errors, deferred).runCache(cache)
        resultErrors <- errors.get
        defers       <- deferred.get
      } yield (Incremental.Defer(
        result,
        errors = resultErrors.reverse,
        path = ListValue(path.reverse),
        label = label
      )
        -> defers)

    for {
      cache    <- makeCache
      response <- runQuery(stepReducer.reduceStep(plan, request.field, Map.empty, Nil), cache)
    } yield response
  }

  private[caliban] def fail(error: CalibanError)(implicit trace: Trace): UIO[GraphQLResponse[CalibanError]] =
    ZIO.succeed(GraphQLResponse(NullValue, List(error)))

  private final class StepReducer[R](
    transformer: Transformer[R],
    isSubscription: Boolean,
    isDeferredEnabled: Boolean,
    wrapPureValues: Boolean
  )(implicit trace: Trace) {

    def reduceStep(
      step: Step[R],
      currentField: Field,
      arguments: Map[String, InputValue],
      path: List[PathValue]
    ): ReducedStep[R] = {

      def reduceObjectStep(objectName: String, getFieldStep: String => Step[R]): ReducedStep[R] = {
        def reduceField(f: Field): (String, ReducedStep[R], FieldInfo) = {
          val aliasedName = f.aliasedName
          val fname       = f.name
          val field       = {
            if (fname == "__typename") PureStep(StringValue(objectName))
            else reduceStep(getFieldStep(fname), f, f.arguments, PathValue.Key(aliasedName) :: path)
          }
          (aliasedName, field, fieldInfo(f, aliasedName, path, f.directives))
        }

        val filteredFields    = mergeFields(currentField, objectName)
        val (deferred, eager) = {
          if (isDeferredEnabled) {
            filteredFields.partitionMap { f =>
              val entry = reduceField(f)
              f.fragment match {
                // The defer spec provides some latitude on how we handle responses. Since it is more performant to return
                // pure fields rather than spin up the defer machinery we return pure fields immediately to the caller.
                case Some(IsDeferred(label)) if !entry._2.isPure => Left((label, entry))
                case _                                           => Right(entry)
              }
            }
          } else (Nil, filteredFields.map(reduceField))
        }

        val eagerReduced = reduceObject(eager)
        deferred match {
          case Nil => eagerReduced
          case d   =>
            DeferStep(
              eagerReduced,
              d.groupBy(_._1).toList.map { case (label, labelAndFields) =>
                val (_, fields) = labelAndFields.unzip
                reduceObject(fields) -> label
              },
              path
            )
        }
      }

      def reduceListStep(steps: List[Step[R]]): ReducedStep[R] = {

        def reduceToListStep(head: ReducedStep[R], remaining0: List[Step[R]]): ReducedStep[R] = {
          var i         = 1
          val nil       = Nil
          val lb        = ListBuffer.empty[ReducedStep[R]]
          var isPure    = wrapPureValues && head.isPure
          lb addOne head
          var remaining = remaining0
          while (remaining ne nil) {
            val step = reduceStep(remaining.head, currentField, arguments, PathValue.Index(i) :: path)
            if (isPure && !step.isPure) isPure = false
            lb addOne step
            i += 1
            remaining = remaining.tail
          }
          ReducedStep.ListStep(
            lb.result(),
            Types.listOf(currentField.fieldType) match {
              case Some(tpe) => tpe.isNullable
              case None      => false
            },
            isPure
          )
        }

        def reduceToPureStep(head: PureStep, remaining0: List[Step[R]]): ReducedStep[R] = {
          var i         = 1
          val nil       = Nil
          val lb        = ListBuffer.empty[ResponseValue]
          var remaining = remaining0
          lb addOne head.value
          while (remaining ne nil) {
            val step = reduceStep(remaining.head, currentField, arguments, PathValue.Index(i) :: path)
            lb addOne step.asInstanceOf[PureStep].value
            i += 1
            remaining = remaining.tail
          }
          PureStep(ListValue(lb.result()))
        }

        if (steps.isEmpty) PureStep(ListValue(Nil))
        else {
          reduceStep(steps.head, currentField, arguments, PathValue.Index(0) :: path) match {
            // In 99.99% of the cases, if the head is pure, all the other elements will be pure as well but we catch that error just in case
            // NOTE: Our entire test suite passes without catching the error
            case step: PureStep =>
              try reduceToPureStep(step, steps.tail)
              catch { case _: ClassCastException => reduceToListStep(step, steps.tail) }
            case step           => reduceToListStep(step, steps.tail)
          }
        }
      }

      def reduceQuery(query: ZQuery[R, Throwable, Step[R]]) =
        ReducedStep.QueryStep(
          query.foldCauseQuery(
            e => ZQuery.failCause(effectfulExecutionError(path, Some(currentField.locationInfo), e)),
            a => ZQuery.succeed(reduceStep(a, currentField, arguments, path))
          )
        )

      def reduceStream(stream: ZStream[R, Throwable, Step[R]]) =
        if (isSubscription) {
          ReducedStep.StreamStep(
            stream
              .mapErrorCause(effectfulExecutionError(path, Some(currentField.locationInfo), _))
              .map(reduceStep(_, currentField, arguments, path))
          )
        } else {
          reduceStep(
            QueryStep(ZQuery.fromZIONow(stream.runCollect.map(chunk => ListStep(chunk.toList)))),
            currentField,
            arguments,
            path
          )
        }

      def wrapFn[A](step: A => Step[R], input: A): Step[R] =
        try step(input)
        catch { case NonFatal(e) => Step.fail(e) }

      step match {
        case s: PureStep                => s
        case s: QueryStep[R]            => reduceQuery(s.query)
        case s: ObjectStep[R]           => val t = transformer(s, currentField); reduceObjectStep(t.name, t.fields)
        case s: FunctionStep[R]         => reduceStep(wrapFn(s.step, arguments), currentField, Map.empty, path)
        case s: MetadataFunctionStep[R] => reduceStep(wrapFn(s.step, currentField), currentField, arguments, path)
        case s: ListStep[R]             => reduceListStep(s.steps)
        case s: StreamStep[R]           => reduceStream(s.inner)
      }
    }

    private def mergeFields(field: Field, typeName: String): List[Field] = {
      def matchesTypename(f: Field): Boolean =
        f._condition.isEmpty || f._condition.get.contains(typeName)

      def mergeFields(fields: List[Field]) = {
        val map       = new java.util.LinkedHashMap[String, Field](calculateMapCapacity(fields.size))
        val nil       = Nil
        var remaining = fields
        while (remaining ne nil) {
          val h = remaining.head
          if (matchesTypename(h)) {
            map.compute(
              h.aliasedName,
              (_, f) =>
                if (f eq null) h
                else f.copy(fields = f.fields ::: h.fields)
            )
          }
          remaining = remaining.tail
        }
        map.values().asScala.toList
      }

      val fields = field.fields
      if (field.allFieldsUniqueNameAndCondition) {
        if (fields.isEmpty || !matchesTypename(fields.head)) Nil
        else fields
      } else mergeFields(fields)
    }

    /**
     * The behaviour of mutable Maps (both Java and Scala) is to resize once the number of entries exceeds
     * the capacity * loadFactor (default of 0.75d) threshold in order to prevent hash collisions.
     *
     * This method is a helper method to estimate the initial map size depending on the number of elements the Map is
     * expected to hold
     *
     * NOTE: This method is the same as java.util.HashMap.calculateHashMapCapacity on JDK19+
     */
    private def calculateMapCapacity(nMappings: Int): Int =
      Math.ceil(nMappings / 0.75d).toInt

    private def effectfulExecutionError(
      path: List[PathValue],
      locationInfo: Option[LocationInfo],
      cause: Cause[Throwable]
    ): Cause[ExecutionError] =
      cause.failureOption orElse cause.defects.headOption match {
        case Some(e: ExecutionError) => Cause.fail(e.copy(path = path.reverse, locationInfo = locationInfo))
        case other                   => Cause.fail(ExecutionError("Effect failure", path.reverse, locationInfo, other))
      }

    private def fieldInfo(
      field: Field,
      aliasedName: String,
      path: List[PathValue],
      fieldDirectives: List[Directive]
    ): FieldInfo =
      FieldInfo(aliasedName, field, path, fieldDirectives, field.parentType)

    private def reduceObject(items: List[(String, ReducedStep[R], FieldInfo)]): ReducedStep[R] = {
      var hasPures   = false
      var hasQueries = false
      val nil        = Nil
      var remaining  = items
      while ((remaining ne nil) && !(hasPures && hasQueries)) {
        val isPure = remaining.head._2.isPure
        if (isPure && !hasPures) hasPures = true
        else if (!isPure && !hasQueries) hasQueries = true
        else ()
        remaining = remaining.tail
      }

      if (hasQueries || wrapPureValues) ReducedStep.ObjectStep(items, hasPures, !hasQueries)
      else
        PureStep(
          ObjectValue(items.asInstanceOf[List[(String, PureStep, FieldInfo)]].map { case (k, v, _) => (k, v.value) })
        )
    }
  }

  private final class ReducedStepExecutor[R](
    queryExecution: QueryExecution,
    isMutation: Boolean,
    fieldWrappers: List[FieldWrapper[R]],
    wrapPureValues: Boolean
  )(implicit trace: Trace) {
    private type ExecutionQuery[+A] = ZQuery[R, ExecutionError, A]

    private val queryExecutionTag = queryExecution.tag

    private def collectAll[E, A, B, Coll[+V] <: Iterable[V]](
      in: Coll[A],
      isTopLevelField: Boolean
    )(
      as: A => ZQuery[R, E, B]
    )(implicit bf: BuildFrom[Coll[A], B, Coll[B]]): ZQuery[R, E, Coll[B]] =
      if (in.sizeCompare(1) == 0) as(in.head).map(v => bf.fromSpecific(in)(v :: Nil))
      else if (isMutation && isTopLevelField) ZQuery.foreach(in)(as)
      else
        (queryExecutionTag: @switch) match {
          case QueryExecution.Sequential.tag => ZQuery.foreach(in)(as)
          case QueryExecution.Parallel.tag   => ZQuery.foreachPar(in)(as)
          case QueryExecution.Batched.tag    => ZQuery.foreachBatched(in)(as)
          case QueryExecution.Mixed.tag      =>
            if (isTopLevelField) ZQuery.foreachPar(in)(as) else ZQuery.foreachBatched(in)(as)
        }

    def makeQuery(
      step: ReducedStep[R],
      errors: Ref[List[CalibanError]],
      deferred: Ref[List[Deferred[R]]]
    ): URQuery[R, ResponseValue] = {

      def handleError(error: ExecutionError): UQuery[ResponseValue] =
        ZQuery.fromZIONow(errors.update(error :: _).as(NullValue))

      def wrap(query: ExecutionQuery[ResponseValue], isPure: Boolean, fieldInfo: FieldInfo) = {
        @tailrec
        def loop(query: ExecutionQuery[ResponseValue], wrappers: List[FieldWrapper[R]]): ExecutionQuery[ResponseValue] =
          wrappers match {
            case Nil             => query
            case wrapper :: tail =>
              val q = if (isPure && !wrapper.wrapPureValues) query else wrapper.wrap(query, fieldInfo)
              loop(q, tail)
          }
        loop(query, fieldWrappers)
      }

      def objectFieldQuery(step: ReducedStep[R], info: FieldInfo, isPure: Boolean = false) = {
        val q = wrap(loop(step), isPure, info)
        if (info.details.fieldType.isNullable) q.catchAll(handleError) else q
      }

      def makeObjectQuery(
        steps: List[(String, ReducedStep[R], FieldInfo)],
        hasPureFields: Boolean,
        isTopLevelField: Boolean
      ) = {

        def collectAllQueries() = {
          def combineQueryResults(results: List[ResponseValue]) = {
            val builder = ListBuffer.empty[(String, ResponseValue)]
            val nil     = Nil
            var resps   = results
            var names   = steps
            while (resps ne nil) {
              val (name, _, _) = names.head
              builder addOne ((name, resps.head))
              resps = resps.tail
              names = names.tail
            }
            ObjectValue(builder.result())
          }
          collectAll(steps, isTopLevelField) { case (_, step, info) =>
            // Only way we could have ended with pure fields here is if we wrap pure values, so we check that first as it's cheaper
            objectFieldQuery(step, info, wrapPureValues && step.isPure)
          }.map(combineQueryResults)
        }

        def combineResults(names: List[String], resolved: List[ResponseValue])(fromQueries: List[ResponseValue]) = {
          val nil                                    = Nil
          var results: List[(String, ResponseValue)] = nil
          var remainingQueries                       = fromQueries
          var remainingResponses                     = resolved
          var remainingNames                         = names
          while (remainingResponses ne nil) {
            val name = remainingNames.head
            val resp = remainingResponses.head match {
              case null =>
                val v = remainingQueries.head
                remainingQueries = remainingQueries.tail
                v
              case v    => v
            }
            results = (name, resp) :: results
            remainingResponses = remainingResponses.tail
            remainingNames = remainingNames.tail
          }
          ObjectValue(results)
        }

        def collectMixed() = {
          val nil                                                = Nil // Bring into stack memory to avoid fetching from heap on each iteration
          var queries: List[(String, ReducedStep[R], FieldInfo)] = nil
          var names: List[String]                                = nil
          var resolved: List[ResponseValue]                      = nil
          var remaining                                          = steps
          while (remaining ne nil) {
            val t @ (name, step, _) = remaining.head
            val value               = step match {
              case PureStep(value) => value
              case _               =>
                queries = t :: queries
                null
            }
            resolved = value :: resolved
            names = name :: names
            remaining = remaining.tail
          }
          collectAll(queries, isTopLevelField) { case (_, s, i) => objectFieldQuery(s, i) }
            .map(combineResults(names, resolved))
        }

        if (hasPureFields && !wrapPureValues) collectMixed() else collectAllQueries()
      }

      def makeListQuery(steps: List[ReducedStep[R]], areItemsNullable: Boolean): ExecutionQuery[ResponseValue] =
        collectAll(steps, isTopLevelField = false)(if (areItemsNullable) loop(_).catchAll(handleError) else loop(_))
          .map(ListValue.apply)

      def loop(step: ReducedStep[R], isTopLevelField: Boolean = false): ExecutionQuery[ResponseValue] =
        step match {
          case PureStep(value)                                  => ZQuery.succeed(value)
          case ReducedStep.QueryStep(step)                      => step.flatMap(loop(_))
          case ReducedStep.ObjectStep(steps, hasPureFields, _)  => makeObjectQuery(steps, hasPureFields, isTopLevelField)
          case ReducedStep.ListStep(steps, areItemsNullable, _) => makeListQuery(steps, areItemsNullable)
          case ReducedStep.StreamStep(stream)                   =>
            ZQuery
              .environmentWith[R](env =>
                ResponseValue.StreamValue(
                  stream.mapChunksZIO { chunk =>
                    collectAll(chunk, isTopLevelField)(loop(_).catchAll(_ => ZQuery.succeed(NullValue))).run
                  }.provideEnvironment(env)
                )
              )
          case ReducedStep.DeferStep(obj, nextSteps, path)      =>
            val deferredSteps = nextSteps.map { case (step, label) =>
              Deferred(path, step, label)
            }
            ZQuery.fromZIONow(deferred.update(deferredSteps ::: _)) *> loop(obj)
        }

      loop(step, isTopLevelField = true).catchAll(handleError)
    }
  }

  // The implicit classes below are for methods that don't exist in Scala 2.12 so we add them as syntax methods instead
  private implicit class EnrichedListOps[+A](private val list: List[A]) extends AnyVal {
    def partitionMap[A1, A2](f: A => Either[A1, A2]): (List[A1], List[A2]) = {
      val l = List.newBuilder[A1]
      val r = List.newBuilder[A2]
      list.foreach { x =>
        f(x) match {
          case Left(x1)  => l += x1
          case Right(x2) => r += x2
        }
      }
      (l.result(), r.result())
    }
  }

  private implicit class EnrichedListBufferOps[A](private val lb: ListBuffer[A]) extends AnyVal {
    def mapInPlace[B](f: A => B): ListBuffer[B] = lb.map(f)
    def addOne(value: A): ListBuffer[A]         = lb += value
  }

  private implicit class EnrichedVectorBuilderOps[A](private val lb: VectorBuilder[A]) extends AnyVal {
    def addOne(value: A): VectorBuilder[A] = lb += value
  }
}
