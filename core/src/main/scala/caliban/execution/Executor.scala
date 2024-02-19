package caliban.execution

import caliban.CalibanError.ExecutionError
import caliban.ResponseValue._
import caliban.Value._
import caliban._
import caliban.execution.Fragment.IsDeferred
import caliban.parsing.adt._
import caliban.schema.ReducedStep.DeferStep
import caliban.schema.Step._
import caliban.schema.{ ReducedStep, Step, Types }
import caliban.transformers.Transformer
import caliban.wrappers.Wrapper.FieldWrapper
import zio._
import zio.query._
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.stream.ZStream

import scala.annotation.tailrec
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
   */
  def executeRequest[R](
    request: ExecutionRequest,
    plan: Step[R],
    fieldWrappers: List[FieldWrapper[R]] = Nil,
    queryExecution: QueryExecution = QueryExecution.Parallel,
    featureSet: Set[Feature] = Set.empty,
    transformer: Transformer[R] = Transformer.empty
  )(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] = {
    val wrapPureValues     = fieldWrappers.exists(_.wrapPureValues)
    val isDeferredEnabled  = featureSet(Feature.Defer)
    val isMutation         = request.operationType == OperationType.Mutation
    val isSubscription     = request.operationType == OperationType.Subscription
    val isEmptyTransformer = transformer.isEmpty

    type ExecutionQuery[+A] = ZQuery[R, ExecutionError, A]

    def collectAll[E, A, B, Coll[+V] <: Iterable[V]](
      in: Coll[A],
      isTopLevelField: Boolean
    )(
      as: A => ZQuery[R, E, B]
    )(implicit bf: BuildFrom[Coll[A], B, Coll[B]]): ZQuery[R, E, Coll[B]] = {
      val sc = in.sizeCompare(1)
      queryExecution match {
        case _ if sc == 0                       => as(in.head).map(bf.newBuilder(in).+=(_).result())
        case _ if isTopLevelField && isMutation => ZQuery.foreach(in)(as)
        case QueryExecution.Batched             => ZQuery.foreachBatched(in)(as)
        case QueryExecution.Parallel            => ZQuery.foreachPar(in)(as)
        case QueryExecution.Sequential          => ZQuery.foreach(in)(as)
      }
    }

    def reduceStep(
      step: Step[R],
      currentField: Field,
      arguments: Map[String, InputValue],
      path: List[PathValue]
    ): ReducedStep[R] = {

      def reduceObjectStep(objectName: String, getFieldStep: String => Step[R]): ReducedStep[R] = {
        def reduceField(f: Field): (String, ReducedStep[R], FieldInfo) = {
          val field =
            if (f.name == "__typename") PureStep(StringValue(objectName))
            else reduceStep(getFieldStep(f.name), f, f.arguments, PathValue.Key(f.aliasedName) :: path)
          (f.aliasedName, field, fieldInfo(f, path, f.directives))
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

        val eagerReduced = reduceObject(eager, wrapPureValues)
        deferred match {
          case Nil => eagerReduced
          case d   =>
            DeferStep(
              eagerReduced,
              d.groupBy(_._1).toList.map { case (label, labelAndFields) =>
                val (_, fields) = labelAndFields.unzip
                reduceObject(fields, wrapPureValues) -> label
              },
              path
            )
        }
      }

      def reduceListStep(steps: List[Step[R]]) = {
        var i         = 0
        val lb        = ListBuffer.empty[ReducedStep[R]]
        val nil       = Nil
        var remaining = steps
        while (remaining ne nil) {
          lb addOne reduceStep(remaining.head, currentField, arguments, PathValue.Index(i) :: path)
          i += 1
          remaining = remaining.tail
        }
        reduceList(
          lb.result(),
          Types.listOf(currentField.fieldType) match {
            case Some(tpe) => tpe.isNullable
            case None      => false
          }
        )
      }

      def reduceQuery(query: ZQuery[R, Throwable, Step[R]]) =
        ReducedStep.QueryStep(
          query.foldCauseQuery(
            e => ZQuery.failCause(effectfulExecutionError(path, Some(currentField.locationInfo), e)),
            a => ZQuery.succeed(reduceStep(a, currentField, arguments, path))
          )
        )

      def handleError(step: => Step[R]): Step[R] =
        try step
        catch { case NonFatal(e) => Step.fail(e) }

      val step0 =
        if (isEmptyTransformer) step
        else transformer.transformStep.lift((step, currentField)).getOrElse(step)

      step0 match {
        case s @ PureStep(EnumValue(v))     =>
          // special case of an hybrid union containing case objects, those should return an object instead of a string
          currentField.fields.view.filter(_._condition.forall(_.contains(v))).collectFirst {
            case f if f.name == "__typename" =>
              ObjectValue(List(f.aliasedName -> StringValue(v)))
            case f if f.name == "_"          =>
              NullValue
          } match {
            case Some(v) => PureStep(v)
            case None    => s
          }
        case s: PureStep                    => s
        case QueryStep(inner)               => reduceQuery(inner)
        case ObjectStep(objectName, fields) => reduceObjectStep(objectName, fields)
        case FunctionStep(step)             => reduceStep(handleError(step(arguments)), currentField, Map.empty, path)
        case MetadataFunctionStep(step)     => reduceStep(handleError(step(currentField)), currentField, arguments, path)
        case ListStep(steps)                => reduceListStep(steps)
        case StreamStep(stream)             =>
          if (isSubscription) {
            ReducedStep.StreamStep(
              stream
                .mapErrorCause(effectfulExecutionError(path, Some(currentField.locationInfo), _))
                .map(reduceStep(_, currentField, arguments, path))
            )
          } else {
            reduceStep(
              QueryStep(ZQuery.fromZIO(stream.runCollect.map(chunk => ListStep(chunk.toList)))),
              currentField,
              arguments,
              path
            )
          }
      }
    }

    def makeQuery(
      step: ReducedStep[R],
      errors: Ref[List[CalibanError]],
      deferred: Ref[List[Deferred[R]]]
    ): URQuery[R, ResponseValue] = {

      def handleError(error: ExecutionError): UQuery[ResponseValue] =
        ZQuery.fromZIO(errors.update(error :: _).as(NullValue))

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

        def combineResults(names: List[String], resolved: List[ResponseValue])(fromQueries: Vector[ResponseValue]) = {
          val nil                                    = Nil
          var results: List[(String, ResponseValue)] = nil
          var i                                      = fromQueries.length
          var remainingResponses                     = resolved
          var remainingNames                         = names
          while (remainingResponses ne nil) {
            val name = remainingNames.head
            var resp = remainingResponses.head
            if (resp eq null) {
              i -= 1
              resp = fromQueries(i)
            }
            results = (name, resp) :: results
            remainingResponses = remainingResponses.tail
            remainingNames = remainingNames.tail
          }
          ObjectValue(results)
        }

        def collectMixed() = {
          val queries                       = new VectorBuilder[(String, ReducedStep[R], FieldInfo)]
          val nil                           = Nil // Bring into stack memory to avoid fetching from heap on each iteration
          var names: List[String]           = nil
          var resolved: List[ResponseValue] = nil
          var remaining                     = steps
          while (remaining ne nil) {
            val (name, step, _) = remaining.head
            val value           = step match {
              case PureStep(value) => value
              case _               => queries.addOne(remaining.head); null
            }
            resolved = value :: resolved
            names = name :: names
            remaining = remaining.tail
          }
          collectAll(queries.result(), isTopLevelField) { case (_, s, i) => objectFieldQuery(s, i) }
            .map(combineResults(names, resolved))
        }

        if (hasPureFields && !wrapPureValues) collectMixed() else collectAllQueries()
      }

      def makeListQuery(steps: List[ReducedStep[R]], areItemsNullable: Boolean): ExecutionQuery[ResponseValue] =
        collectAll(steps, isTopLevelField = false)(if (areItemsNullable) loop(_).catchAll(handleError) else loop(_))
          .map(ListValue.apply)

      def loop(step: ReducedStep[R], isTopLevelField: Boolean = false): ExecutionQuery[ResponseValue] =
        step match {
          case PureStep(value)                               => ZQuery.succeed(value)
          case ReducedStep.QueryStep(step)                   => step.flatMap(loop(_))
          case ReducedStep.ObjectStep(steps, hasPureFields)  => makeObjectQuery(steps, hasPureFields, isTopLevelField)
          case ReducedStep.ListStep(steps, areItemsNullable) => makeListQuery(steps, areItemsNullable)
          case ReducedStep.StreamStep(stream)                =>
            ZQuery
              .environmentWith[R](env =>
                ResponseValue.StreamValue(
                  stream.mapChunksZIO { chunk =>
                    collectAll(chunk, isTopLevelField)(loop(_).catchAll(_ => ZQuery.succeed(NullValue))).run
                  }.provideEnvironment(env)
                )
              )
          case ReducedStep.DeferStep(obj, nextSteps, path)   =>
            val deferredSteps = nextSteps.map { case (step, label) =>
              Deferred(path, step, label)
            }
            ZQuery.fromZIO(deferred.update(deferredSteps ::: _)) *> loop(obj)
        }
      loop(step, isTopLevelField = true).catchAll(handleError)
    }

    def runQuery(step: ReducedStep[R], cache: Cache) =
      for {
        env          <- ZIO.environment[R]
        deferred     <- Ref.make(List.empty[Deferred[R]])
        errors       <- Ref.make(List.empty[CalibanError])
        query         = makeQuery(step, errors, deferred)
        result       <- query.runCache(cache)
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
        query         = makeQuery(step, errors, deferred)
        result       <- query.runCache(cache)
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
      cache    <- Cache.empty
      reduced   = reduceStep(plan, request.field, Map.empty, Nil)
      response <- runQuery(reduced, cache)
    } yield response
  }

  private[caliban] def fail(error: CalibanError)(implicit trace: Trace): UIO[GraphQLResponse[CalibanError]] =
    ZIO.succeed(GraphQLResponse(NullValue, List(error)))

  private[caliban] def mergeFields(field: Field, typeName: String): List[Field] = {
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

  private def fieldInfo(field: Field, path: List[PathValue], fieldDirectives: List[Directive]): FieldInfo =
    FieldInfo(field.aliasedName, field, path, fieldDirectives, field.parentType)

  // In 99.99% of the cases, if the head is pure, all the other elements will be pure as well but we catch that error just in case
  // NOTE: Our entire test suite passes without catching the error
  private def reduceList[R](list: List[ReducedStep[R]], areItemsNullable: Boolean): ReducedStep[R] =
    if (list.isEmpty || list.head.isPure)
      try PureStep(ListValue(list.asInstanceOf[List[PureStep]].map(_.value)))
      catch { case _: ClassCastException => ReducedStep.ListStep(list, areItemsNullable) }
    else ReducedStep.ListStep(list, areItemsNullable)

  private def reduceObject[R](
    items: List[(String, ReducedStep[R], FieldInfo)],
    wrapPureValues: Boolean
  ): ReducedStep[R] = {
    var hasPures   = false
    val nil        = Nil
    var hasQueries = wrapPureValues
    var remaining  = items
    while ((remaining ne nil) && !(hasPures && hasQueries)) {
      if (remaining.head._2.isPure) hasPures = true
      else hasQueries = true
      remaining = remaining.tail
    }

    if (hasQueries) ReducedStep.ObjectStep(items, hasPures)
    else
      PureStep(
        ObjectValue(items.asInstanceOf[List[(String, PureStep, FieldInfo)]].map { case (k, v, _) => (k, v.value) })
      )
  }

  private def effectfulExecutionError(
    path: List[PathValue],
    locationInfo: Option[LocationInfo],
    cause: Cause[Throwable]
  ): Cause[ExecutionError] =
    cause.failureOption orElse cause.defects.headOption match {
      case Some(e: ExecutionError) => Cause.fail(e.copy(path = path.reverse, locationInfo = locationInfo))
      case other                   => Cause.fail(ExecutionError("Effect failure", path.reverse, locationInfo, other))
    }

  private implicit class EnrichedListOps[+A](val list: List[A]) extends AnyVal {
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

  private implicit class EnrichedListBufferOps[A](val lb: ListBuffer[A]) extends AnyVal {
    // This method doesn't exist in Scala 2.12 so we just use `.map` for it instead
    def mapInPlace[B](f: A => B): ListBuffer[B] = lb.map(f)
    def addOne(value: A): ListBuffer[A]         = lb += value
  }

  private implicit class EnrichedVectorBuilderOps[A](private val lb: VectorBuilder[A]) extends AnyVal {
    def addOne(value: A): VectorBuilder[A] = lb += value
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

}
