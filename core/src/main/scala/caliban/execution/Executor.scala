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
import caliban.wrappers.Wrapper.FieldWrapper
import zio._
import zio.query.{ Cache, UQuery, URQuery, ZQuery }
import zio.stacktracer.TracingImplicits.disableAutoTrace
import zio.stream.ZStream

import scala.annotation.tailrec
import scala.collection.mutable
import scala.jdk.CollectionConverters._

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
    featureSet: Set[Feature] = Set.empty
  )(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] = {
    val wrapPureValues = fieldWrappers.exists(_.wrapPureValues)
    type ExecutionQuery[+A] = ZQuery[R, ExecutionError, A]

    val execution                                                          = request.operationType match {
      case OperationType.Query        => queryExecution
      case OperationType.Mutation     => QueryExecution.Sequential
      case OperationType.Subscription => QueryExecution.Sequential
    }
    def collectAll[E, A](as: List[ZQuery[R, E, A]]): ZQuery[R, E, List[A]] =
      execution match {
        case QueryExecution.Sequential => ZQuery.collectAll(as)
        case QueryExecution.Parallel   => ZQuery.collectAllPar(as)
        case QueryExecution.Batched    => ZQuery.collectAllBatched(as)
      }

    def reduceStep(
      step: Step[R],
      currentField: Field,
      arguments: Map[String, InputValue],
      path: List[Either[String, Int]]
    ): ReducedStep[R] = {

      def reduceObjectStep(objectName: String, fields: Map[String, Step[R]]) = {
        val filteredFields    = mergeFields(currentField, objectName)
        val (deferred, eager) = filteredFields.partitionMap {
          case f @ Field("__typename", _, _, _, _, _, _, directives, _, _, _)   =>
            Right((f.aliasedName, PureStep(StringValue(objectName)), fieldInfo(f, path, directives)))
          case f @ Field(name, _, _, _, _, _, args, directives, _, _, fragment) =>
            val aliasedName = f.aliasedName
            val field       = fields
              .get(name)
              .fold(NullStep: ReducedStep[R])(reduceStep(_, f, args, Left(aliasedName) :: path))

            val info = fieldInfo(f, path, directives)

            fragment.collectFirst {
              // The defer spec provides some latitude on how we handle responses. Since it is more performant to return
              // pure fields rather than spin up the defer machinery we return pure fields immediately to the caller.
              case IsDeferred(label) if featureSet(Feature.Defer) && !field.isPure =>
                (label, (aliasedName, field, info))
            }.toLeft((aliasedName, field, info))
        }

        deferred match {
          case Nil => reduceObject(eager, wrapPureValues)
          case d   =>
            DeferStep(
              reduceObject(eager, wrapPureValues),
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
        val lb        = List.newBuilder[ReducedStep[R]]
        var remaining = steps
        while (!remaining.isEmpty) {
          lb += reduceStep(remaining.head, currentField, arguments, Right(i) :: path)
          i += 1
          remaining = remaining.tail
        }
        reduceList(lb.result(), Types.listOf(currentField.fieldType).fold(false)(_.isNullable))
      }

      step match {
        case s @ PureStep(EnumValue(v))     =>
          // special case of an hybrid union containing case objects, those should return an object instead of a string
          val obj = currentField.fields.view.filter(_._condition.forall(_.contains(v))).collectFirst {
            case f if f.name == "__typename" =>
              ObjectValue(List(f.aliasedName -> StringValue(v)))
            case f if f.name == "_"          =>
              NullValue
          }
          obj.fold(s)(PureStep(_))
        case s: PureStep                    => s
        case FunctionStep(step)             => reduceStep(step(arguments), currentField, Map.empty, path)
        case MetadataFunctionStep(step)     => reduceStep(step(currentField), currentField, arguments, path)
        case QueryStep(inner)               =>
          ReducedStep.QueryStep(
            inner.foldCauseQuery(
              e => ZQuery.failCause(effectfulExecutionError(path, Some(currentField.locationInfo), e)),
              a => ZQuery.succeed(reduceStep(a, currentField, arguments, path))
            )
          )
        case ObjectStep(objectName, fields) => reduceObjectStep(objectName, fields)
        case ListStep(steps)                => reduceListStep(steps)
        case StreamStep(stream)             =>
          if (request.operationType == OperationType.Subscription) {
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
        ZQuery.fromZIO(errors.update(error :: _)).as(NullValue)

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

      def objectFieldQuery(name: String, step: ReducedStep[R], info: FieldInfo) = {
        val q = wrap(loop(step), step.isPure, info)

        if (info.details.fieldType.isNullable) q.catchAll(handleError).map((name, _))
        else q.map((name, _))
      }

      def makeObjectQuery(steps: List[(String, ReducedStep[R], FieldInfo)]) = {
        var resolved: mutable.HashMap[String, ResponseValue] = null

        val queries =
          if (wrapPureValues) steps.map((objectFieldQuery _).tupled)
          else {
            val queries   = List.newBuilder[ExecutionQuery[(String, ResponseValue)]]
            var remaining = steps

            while (!remaining.isEmpty) {
              remaining.head match {
                case (name, PureStep(value), _) =>
                  if (null == resolved) resolved = new mutable.HashMap[String, ResponseValue]()
                  resolved.update(name, value)
                case (name, step, info)         =>
                  queries += objectFieldQuery(name, step, info)
              }
              remaining = remaining.tail
            }
            queries.result()
          }

        if (null == resolved) collectAll(queries).map(ObjectValue.apply)
        else
          collectAll(queries).map { results =>
            results.foreach(kv => resolved.update(kv._1, kv._2))
            ObjectValue(steps.map { case (name, _, _) => name -> resolved(name) })
          }
      }

      def makeListQuery(steps: List[ReducedStep[R]], areItemsNullable: Boolean) =
        collectAll(
          if (areItemsNullable) steps.map(loop(_).catchAll(handleError))
          else steps.map(loop)
        ).map(ListValue.apply)

      def loop(step: ReducedStep[R]): ExecutionQuery[ResponseValue] =
        step match {
          case PureStep(value)                               => ZQuery.succeed(value)
          case ReducedStep.QueryStep(step)                   => step.flatMap(loop)
          case ReducedStep.ObjectStep(steps)                 => makeObjectQuery(steps)
          case ReducedStep.ListStep(steps, areItemsNullable) => makeListQuery(steps, areItemsNullable)
          case ReducedStep.StreamStep(stream)                =>
            ZQuery
              .environment[R]
              .map(env =>
                ResponseValue.StreamValue(
                  stream
                    .mapZIO(loop(_).catchAll(_ => ZQuery.succeed(NullValue)).run)
                    .provideEnvironment(env)
                )
              )
          case ReducedStep.DeferStep(obj, nextSteps, path)   =>
            val deferredSteps = nextSteps.map { case (step, label) =>
              Deferred(path, step, label)
            }
            ZQuery.fromZIO(deferred.update(deferredSteps ::: _)) *> loop(obj)
        }
      loop(step).catchAll(handleError)
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

      ZStream.mergeAll(1)(defers.map(run): _*)
    }

    def runIncrementalQuery(
      step: ReducedStep[R],
      cache: Cache,
      path: List[Either[String, Int]],
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
        path = ListValue(path.map {
          case Left(s)  => StringValue(s)
          case Right(i) => IntValue(i)
        }.reverse),
        label = label
      )
        -> defers)

    for {
      cache    <- Cache.empty
      reduced   = reduceStep(plan, request.field, Map(), Nil)
      response <- runQuery(reduced, cache)
    } yield response
  }

  private[caliban] def fail(error: CalibanError)(implicit trace: Trace): UIO[GraphQLResponse[CalibanError]] =
    ZIO.succeed(GraphQLResponse(NullValue, List(error)))

  private[caliban] def mergeFields(field: Field, typeName: String): List[Field] = {
    val map      = new java.util.LinkedHashMap[String, Field]()
    var modified = false

    field.fields.foreach { field =>
      if (field._condition.forall(_.contains(typeName))) {
        map.compute(
          field.aliasedName,
          (_, f) =>
            if (f == null) field
            else {
              modified = true
              f.copy(fields = f.fields ::: field.fields)
            }
        )
      } else {
        modified = true
      }
    }

    // Avoid conversions if no modification took place
    if (modified) map.values().asScala.toList else field.fields
  }

  private def fieldInfo(field: Field, path: List[Either[String, Int]], fieldDirectives: List[Directive]): FieldInfo =
    FieldInfo(field.aliasedName, field, path, fieldDirectives, field.parentType)

  private def reduceList[R](list: List[ReducedStep[R]], areItemsNullable: Boolean): ReducedStep[R] =
    if (list.forall(_.isInstanceOf[PureStep]))
      PureStep(ListValue(list.asInstanceOf[List[PureStep]].map(_.value)))
    else ReducedStep.ListStep(list, areItemsNullable)

  private def reduceObject[R](
    items: List[(String, ReducedStep[R], FieldInfo)],
    wrapPureValues: Boolean
  ): ReducedStep[R] =
    if (!wrapPureValues && items.forall(_._2.isPure))
      PureStep(ObjectValue(items.asInstanceOf[List[(String, PureStep, FieldInfo)]].map { case (k, v, _) =>
        (k, v.value)
      }))
    else ReducedStep.ObjectStep(items)

  private def effectfulExecutionError(
    path: List[Either[String, Int]],
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
}
