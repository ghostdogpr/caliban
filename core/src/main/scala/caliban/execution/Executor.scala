package caliban.execution

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import caliban.CalibanError.ExecutionError
import caliban.ResponseValue._
import caliban.Value._
import caliban._
import caliban.parsing.adt._
import caliban.schema.Step._
import caliban.schema.{ ReducedStep, Step, Types }
import caliban.wrappers.Wrapper.FieldWrapper
import zio._
import zio.query.ZQuery

object Executor {

  /**
   * Executes the given query against a schema. It returns either an [[caliban.CalibanError.ExecutionError]] or a [[ResponseValue]].
   * @param request a request object containing all information needed
   * @param plan an execution plan
   * @param variables a list of variables
   * @param fieldWrappers a list of field wrappers
   * @param queryExecution a strategy for executing queries in parallel or not
   */
  def executeRequest[R](
    request: ExecutionRequest,
    plan: Step[R],
    variables: Map[String, InputValue] = Map(),
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
          val mergedFields = mergeFields(currentField, objectName)
          val items        = mergedFields.map {
            case f @ Field(name @ "__typename", _, _, alias, _, _, _, _, directives) =>
              (alias.getOrElse(name), PureStep(StringValue(objectName)), fieldInfo(f, path, directives))
            case f @ Field(name, _, _, alias, _, _, args, _, directives)             =>
              val arguments = resolveVariables(args, request.variableDefinitions, variables)
              (
                alias.getOrElse(name),
                fields
                  .get(name)
                  .fold(NullStep: ReducedStep[R])(reduceStep(_, f, arguments, Left(alias.getOrElse(name)) :: path)),
                fieldInfo(f, path, directives)
              )
          }
          reduceObject(items, fieldWrappers)
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

    def makeQuery(step: ReducedStep[R], errors: Ref[List[CalibanError]]): ZQuery[R, Nothing, ResponseValue] = {

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
            val q = if (isPure && !wrapper.wrapPureValues) query else wrapper.f(query, fieldInfo)
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
              .fromEffect(ZIO.environment[R])
              .map(env =>
                Right(
                  ResponseValue.StreamValue(
                    stream
                      .mapM(loop(_).flatMap(_.fold(_ => ZQuery.succeed(NullValue), ZQuery.succeed(_))).run)
                      .provide(env)
                  )
                )
              )
        }
      loop(step).flatMap(_.fold(error => ZQuery.fromEffect(errors.update(error :: _)).as(NullValue), ZQuery.succeed(_)))
    }

    for {
      errors       <- Ref.make(List.empty[CalibanError])
      reduced       = reduceStep(plan, request.field, Map(), Nil)
      query         = makeQuery(reduced, errors)
      result       <- query.run
      resultErrors <- errors.get
    } yield GraphQLResponse(result, resultErrors.reverse)
  }

  private[caliban] def fail(error: CalibanError): UIO[GraphQLResponse[CalibanError]] =
    IO.succeed(GraphQLResponse(NullValue, List(error)))

  private def resolveVariables(
    arguments: Map[String, InputValue],
    variableDefinitions: List[VariableDefinition],
    variableValues: Map[String, InputValue]
  ): Map[String, InputValue] = {
    def resolveVariable(value: InputValue): InputValue =
      value match {
        case InputValue.ListValue(values)   => InputValue.ListValue(values.map(resolveVariable))
        case InputValue.ObjectValue(fields) =>
          InputValue.ObjectValue(fields.map({ case (k, v) => k -> resolveVariable(v) }))
        case InputValue.VariableValue(name) =>
          lazy val defaultInputValue = (for {
            definition <- variableDefinitions.find(_.name == name)
            inputValue <- definition.defaultValue
          } yield inputValue) getOrElse NullValue
          variableValues.getOrElse(name, defaultInputValue)
        case value: Value                   => value
      }
    arguments.map({ case (k, v) => k -> resolveVariable(v) })
  }

  private[caliban] def mergeFields(field: Field, typeName: String): List[Field] = {
    // ugly mutable code but it's worth it for the speed ;)
    val array = ArrayBuffer.empty[Field]
    val map   = collection.mutable.Map.empty[String, Int]

    field.fields.foreach { field =>
      if (field.condition.forall(_.contains(typeName))) {
        val name = field.alias.getOrElse(field.name)
        map.get(name) match {
          case None        =>
            // first time we see this field, add it to the array
            array += field
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
}
