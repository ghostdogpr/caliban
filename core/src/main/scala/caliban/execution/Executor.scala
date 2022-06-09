package caliban.execution

import scala.annotation.tailrec
import caliban.CalibanError.ExecutionError
import caliban.ResponseValue._
import caliban.Value._
import caliban._
import caliban.execution.Executor.{ effectfulExecutionError, fieldInfo, mergeFields, reduceList, reduceObject }
import caliban.execution.Fragment.IsDeferred
import caliban.parsing.adt._
import caliban.schema.ReducedStep.DeferStep
import caliban.schema.Step._
import caliban.schema.{ ReducedStep, Step, Types }
import caliban.wrappers.Wrapper.FieldWrapper
import zio._
import zio.query.{ Cache, Described, ZQuery }
import zio.stream.ZStream

import scala.collection.mutable.ArrayBuffer

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
    queryExecution: QueryExecution = QueryExecution.Parallel
  ): URIO[R, GraphQLResponse[CalibanError]] = {

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
    ): ReducedStep[R] =
      step match {
        case s @ PureStep(value)            =>
          value match {
            case EnumValue(v) =>
              // special case of an hybrid union containing case objects, those should return an object instead of a string
              val obj = mergeFields(currentField, v).collectFirst {
                case f: Field if f.name == "__typename" =>
                  ObjectValue(List(f.alias.getOrElse(f.name) -> StringValue(v)))
                case f: Field if f.name == "_"          =>
                  NullValue
              }
              obj.fold(s)(PureStep(_))
            case _            => s
          }
        case FunctionStep(step)             => reduceStep(step(arguments), currentField, Map(), path)
        case MetadataFunctionStep(step)     => reduceStep(step(currentField), currentField, arguments, path)
        case ListStep(steps)                =>
          reduceList(
            steps.zipWithIndex.map { case (step, i) =>
              reduceStep(step, currentField, arguments, Right(i) :: path)
            },
            Types.listOf(currentField.fieldType).fold(false)(_.isNullable)
          )
        case ObjectStep(objectName, fields) =>
          val filteredFields    = mergeFields(currentField, objectName)
          val (deferred, eager) = filteredFields.partitionMap {
            case f @ Field(name @ "__typename", _, _, alias, _, _, _, directives, _, _, _) =>
              Right((alias.getOrElse(name), PureStep(StringValue(objectName)), fieldInfo(f, path, directives)))
            case f @ Field(name, _, _, alias, _, _, args, directives, _, _, fragment)      =>
              val aliasedName = alias.getOrElse(name)
              val field       = fields
                .get(name)
                .fold(NullStep: ReducedStep[R])(reduceStep(_, f, args, Left(alias.getOrElse(name)) :: path))

              val info = fieldInfo(f, path, directives)

              fragment.collectFirst {
                // The defer spec provides some latitude on how we handle responses. Since it is more performant to return
                // pure fields rather than spin up the defer machinery we return pure fields immediately to the caller.
                case IsDeferred(label) if !field.isPure =>
                  (label, (aliasedName, field, info))
              }.toLeft((aliasedName, field, info))
          }

          deferred match {
            case Nil => reduceObject(eager, fieldWrappers)
            case d   =>
              DeferStep(
                reduceObject(eager, fieldWrappers),
                d.groupBy(_._1).toList.map { case (label, labelAndFields) =>
                  val (_, fields) = labelAndFields.unzip
                  reduceObject(fields, fieldWrappers) -> label
                },
                path
              )
          }
        case QueryStep(inner)               =>
          ReducedStep.QueryStep(
            inner.foldCauseM(
              e => ZQuery.halt(effectfulExecutionError(path, Some(currentField.locationInfo), e)),
              a => ZQuery.succeed(reduceStep(a, currentField, arguments, path))
            )
          )
        case StreamStep(stream)             =>
          if (request.operationType == OperationType.Subscription) {
            ReducedStep.StreamStep(
              stream
                .mapErrorCause(effectfulExecutionError(path, Some(currentField.locationInfo), _))
                .map(reduceStep(_, currentField, arguments, path))
            )
          } else {
            reduceStep(
              QueryStep(ZQuery.fromEffect(stream.runCollect.map(chunk => ListStep(chunk.toList)))),
              currentField,
              arguments,
              path
            )
          }
      }

    def makeQuery(
      step: ReducedStep[R],
      errors: Ref[List[CalibanError]],
      deferred: Ref[List[Deferred[R]]]
    ): ZQuery[R, Nothing, ResponseValue] = {

      def handleError(error: ExecutionError, isNullable: Boolean): ZQuery[Any, ExecutionError, ResponseValue] =
        if (isNullable) ZQuery.fromEffect(errors.update(error :: _)).as(NullValue)
        else ZQuery.fail(error)

      @tailrec
      def wrap(query: ZQuery[R, ExecutionError, ResponseValue], isPure: Boolean)(
        wrappers: List[FieldWrapper[R]],
        fieldInfo: FieldInfo
      ): ZQuery[R, ExecutionError, ResponseValue] =
        wrappers match {
          case Nil             => query
          case wrapper :: tail =>
            val q = if (isPure && !wrapper.wrapPureValues) query else wrapper.wrap(query, fieldInfo)
            wrap(q, isPure)(tail, fieldInfo)
        }

      def loop(step: ReducedStep[R]): ZQuery[R, Nothing, Either[ExecutionError, ResponseValue]] =
        step match {
          case PureStep(value)                               => ZQuery.succeed(Right(value))
          case ReducedStep.ListStep(steps, areItemsNullable) =>
            val queries = steps.map(loop(_).flatMap(_.fold(handleError(_, areItemsNullable), ZQuery.succeed(_))))
            collectAll(queries).map(s => ListValue(s)).either
          case ReducedStep.ObjectStep(steps)                 =>
            val queries = steps.map { case (name, step, info) =>
              wrap(loop(step).flatMap(_.fold(ZQuery.fail(_), ZQuery.succeed(_))), step.isPure)(fieldWrappers, info)
                .foldM(handleError(_, info.details.fieldType.isNullable), ZQuery.succeed(_))
                .map(name -> _)
            }
            collectAll(queries).map(f => ObjectValue(f)).either
          case ReducedStep.QueryStep(step)                   =>
            step.foldM(
              error => ZQuery.succeed(Left(error)),
              query => loop(query)
            )
          case ReducedStep.StreamStep(stream)                =>
            ZQuery
              .environment[R]
              .map(env =>
                Right(
                  ResponseValue.StreamValue(
                    stream
                      .mapM(loop(_).flatMap(_.fold(_ => ZQuery.succeed(NullValue), ZQuery.succeed(_))).run)
                      .provide(env)
                  )
                )
              )
          case ReducedStep.DeferStep(obj, nextSteps, path)   =>
            val deferredSteps = nextSteps.map { case (step, label) =>
              Deferred(path, step, label)
            }
            ZQuery.fromEffect(deferred.update(deferredSteps ::: _)) *> loop(obj)
        }
      loop(step).flatMap(_.fold(error => ZQuery.fromEffect(errors.update(error :: _)).as(NullValue), ZQuery.succeed(_)))
    }

    def runQuery(step: ReducedStep[R], cache: Cache, path: Option[List[Either[String, Int]]], label: Option[String]) =
      for {
        deferred     <- Ref.make(List.empty[Deferred[R]])
        errors       <- Ref.make(List.empty[CalibanError])
        query         = makeQuery(step, errors, deferred)
        result       <- query.runCache(cache)
        resultErrors <- errors.get
        defers       <- deferred.get
      } yield (GraphQLResponse(
        result,
        resultErrors.reverse,
        extensions = None,
        label = label,
        path = path.map(path =>
          ListValue(path.map {
            case Left(value)  => StringValue(value)
            case Right(value) => IntValue(value)
          }.reverse)
        ),
        hasNext = if (defers.nonEmpty) Some(true) else None
      ) -> defers)

    def makeDeferStream(
      defers: List[Deferred[R]],
      remaining: Ref[Int],
      cache: Cache
    ): ZStream[R, Nothing, ResponseValue] = {
      def run(d: Deferred[R]) =
        ZStream.unwrap(runQuery(d.step, cache, Some(d.path), d.label).map {
          case (resp, Nil)  =>
            ZStream.fromEffect(
              remaining.updateAndGet(_ - 1).map(more => resp.copy(hasNext = Some(more > 0))).map(_.toResponseValue)
            )
          case (resp, more) =>
            ZStream.fromEffect(
              remaining.updateAndGet(_ - 1 + more.size).as(resp.copy(hasNext = Some(true)).toResponseValue)
            ) ++
              makeDeferStream(more, remaining, cache)
        })

      ZStream.mergeAllUnbounded()(defers.map(run): _*)
    }

    for {
      env               <- ZIO.environment[R]
      cache             <- Cache.empty
      reduced            = reduceStep(plan, request.field, Map(), Nil)
      responseAndDefers <- runQuery(reduced, cache, None, None)
      (response, defers) = responseAndDefers
      remaining         <- Ref.make(defers.size)
    } yield
      if (defers.nonEmpty)
        response.withExtension("__defer", StreamValue(makeDeferStream(defers, remaining, cache).provide(env)))
      else response
  }

  private[caliban] def fail(error: CalibanError): UIO[GraphQLResponse[CalibanError]] =
    IO.succeed(GraphQLResponse(NullValue, List(error)))

  private[caliban] def mergeFields(field: Field, typeName: String): List[Field] = {
    // ugly mutable code but it's worth it for the speed ;)
    val array = ArrayBuffer.empty[Field]
    val map   = collection.mutable.Map.empty[String, Int]
    var index = 0

    field.fields.foreach { field =>
      if (field._condition.forall(_.contains(typeName))) {
        val name = field.alias.getOrElse(field.name)
        map.get(name) match {
          case None        =>
            // first time we see this field, add it to the array
            array += field
            map.update(name, index)
            index = index + 1
          case Some(index) =>
            // field already existed, merge it
            val f = array(index)
            array(index) = f.copy(fields = f.fields ::: field.fields)
        }
      }
    }

    array.toList
  }

  private def fieldInfo(field: Field, path: List[Either[String, Int]], fieldDirectives: List[Directive]): FieldInfo =
    FieldInfo(field.alias.getOrElse(field.name), field, path, fieldDirectives)

  private def reduceList[R](list: List[ReducedStep[R]], areItemsNullable: Boolean): ReducedStep[R] =
    if (list.forall(_.isInstanceOf[PureStep]))
      PureStep(ListValue(list.asInstanceOf[List[PureStep]].map(_.value)))
    else ReducedStep.ListStep(list, areItemsNullable)

  private def reduceObject[R](
    items: List[(String, ReducedStep[R], FieldInfo)],
    fieldWrappers: List[FieldWrapper[R]]
  ): ReducedStep[R] =
    if (!fieldWrappers.exists(_.wrapPureValues) && items.map(_._2).forall(_.isInstanceOf[PureStep]))
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

object Reducer {}
