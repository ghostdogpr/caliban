package caliban.execution

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import caliban.CalibanError.ExecutionError
import caliban.ResponseValue._
import caliban.Value._
import caliban.parsing.adt._
import caliban.schema.Step._
import caliban.schema.{ ReducedStep, Step }
import caliban.wrappers.Wrapper.FieldWrapper
import caliban.{ CalibanError, GraphQLResponse, InputValue, ResponseValue, Value }
import zio._
import zquery.ZQuery

object Executor {

  /**
   * Executes the given query against a schema. It returns either an [[caliban.CalibanError.ExecutionError]] or a [[ResponseValue]].
   * @param request a request object containing all information needed
   * @param plan an execution plan
   * @param variables a list of variables
   * @param fieldWrappers a list of field wrappers
   */
  def executeRequest[R](
    request: ExecutionRequest,
    plan: Step[R],
    variables: Map[String, InputValue] = Map(),
    fieldWrappers: List[FieldWrapper[R]] = Nil
  ): URIO[R, GraphQLResponse[CalibanError]] = {

    val allowParallelism = request.operationType match {
      case OperationType.Query        => true
      case OperationType.Mutation     => false
      case OperationType.Subscription => false
    }

    def reduceStep(
      step: Step[R],
      currentField: Field,
      arguments: Map[String, InputValue],
      path: List[Either[String, Int]]
    ): ReducedStep[R] =
      step match {
        case s @ PureStep(value) =>
          value match {
            case EnumValue(v) =>
              // special case of an hybrid union containing case objects, those should return an object instead of a string
              val obj = mergeFields(currentField, v).collectFirst {
                case f: Field if f.name == "__typename" =>
                  ObjectValue(List(f.alias.getOrElse(f.name) -> StringValue(v)))
              }
              obj.fold(s)(PureStep(_))
            case _ => s
          }
        case FunctionStep(step) => reduceStep(step(arguments), currentField, Map(), path)
        case ListStep(steps) =>
          reduceList(steps.zipWithIndex.map {
            case (step, i) => reduceStep(step, currentField, arguments, Right(i) :: path)
          })
        case ObjectStep(objectName, fields) =>
          val mergedFields = mergeFields(currentField, objectName)
          val items = mergedFields.map {
            case f @ Field(name @ "__typename", _, _, alias, _, _, _, _, directives) =>
              (alias.getOrElse(name), PureStep(StringValue(objectName)), fieldInfo(f, path, directives))
            case f @ Field(name, _, _, alias, _, _, args, _, directives) =>
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
        case QueryStep(inner) =>
          ReducedStep.QueryStep(
            inner.bimap(
              effectfulExecutionError(path, Some(currentField.locationInfo), _),
              reduceStep(_, currentField, arguments, path)
            )
          )
        case StreamStep(stream) =>
          if (request.operationType == OperationType.Subscription) {
            ReducedStep.StreamStep(
              stream.bimap(
                effectfulExecutionError(path, Some(currentField.locationInfo), _),
                reduceStep(_, currentField, arguments, path)
              )
            )
          } else {
            reduceStep(QueryStep(ZQuery.fromEffect(stream.runCollect.map(ListStep(_)))), currentField, arguments, path)
          }
      }

    def makeQuery(step: ReducedStep[R], errors: Ref[List[CalibanError]]): ZQuery[R, Nothing, ResponseValue] = {

      @tailrec
      def wrap(query: ZQuery[R, Nothing, ResponseValue])(
        wrappers: List[FieldWrapper[R]],
        fieldInfo: FieldInfo
      ): ZQuery[R, Nothing, ResponseValue] =
        wrappers match {
          case Nil => query
          case wrapper :: tail =>
            wrap(
              wrapper
                .f(query, fieldInfo)
                .foldM(error => ZQuery.fromEffect(errors.update(error :: _)).map(_ => NullValue), ZQuery.succeed(_))
            )(tail, fieldInfo)

        }

      def loop(step: ReducedStep[R]): ZQuery[R, Nothing, ResponseValue] =
        step match {
          case PureStep(value) => ZQuery.succeed(value)
          case ReducedStep.ListStep(steps) =>
            val queries = steps.map(loop)
            (if (allowParallelism) ZQuery.collectAllPar(queries) else ZQuery.collectAll(queries)).map(ListValue)
          case ReducedStep.ObjectStep(steps) =>
            val queries = steps.map { case (name, step, info) => wrap(loop(step))(fieldWrappers, info).map(name -> _) }
            (if (allowParallelism) ZQuery.collectAllPar(queries) else ZQuery.collectAll(queries)).map(ObjectValue)
          case ReducedStep.QueryStep(step) =>
            step.foldM(
              error => ZQuery.fromEffect(errors.update(error :: _)).map(_ => NullValue),
              query => loop(query)
            )
          case ReducedStep.StreamStep(stream) =>
            ZQuery
              .fromEffect(ZIO.environment[R])
              .map(env => ResponseValue.StreamValue(stream.mapM(loop(_).run).provide(env)))
        }
      loop(step)
    }

    for {
      errors       <- Ref.make(List.empty[CalibanError])
      reduced      = reduceStep(plan, request.field, Map(), Nil)
      query        = makeQuery(reduced, errors)
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
        case InputValue.ListValue(values) => InputValue.ListValue(values.map(resolveVariable))
        case InputValue.ObjectValue(fields) =>
          InputValue.ObjectValue(fields.map({ case (k, v) => k -> resolveVariable(v) }))
        case InputValue.VariableValue(name) =>
          lazy val defaultInputValue = (for {
            definition <- variableDefinitions.find(_.name == name)
            inputValue <- definition.defaultValue
          } yield inputValue) getOrElse value
          variableValues.getOrElse(name, defaultInputValue)
        case value: Value => value
      }
    arguments.map({ case (k, v) => k -> resolveVariable(v) })
  }

  private[caliban] def mergeFields(field: Field, typeName: String): List[Field] = {
    val allFields = field.fields ++ field.conditionalFields.getOrElse(typeName, Nil)
    allFields
      .foldLeft(ListMap.empty[String, Field]) {
        case (result, field) =>
          val name = field.alias.getOrElse(field.name)
          result.updated(
            name,
            result
              .get(name)
              .fold(field)(f => f.copy(fields = f.fields ++ field.fields))
          )
      }
      .values
      .toList
  }

  private def fieldInfo(field: Field, path: List[Either[String, Int]], fieldDirectives: List[Directive]): FieldInfo =
    FieldInfo(field.alias.getOrElse(field.name), path, field.parentType, field.fieldType, fieldDirectives)

  private def reduceList[R](list: List[ReducedStep[R]]): ReducedStep[R] =
    if (list.forall(_.isInstanceOf[PureStep]))
      PureStep(ListValue(list.asInstanceOf[List[PureStep]].map(_.value)))
    else ReducedStep.ListStep(list)

  private def reduceObject[R](
    items: List[(String, ReducedStep[R], FieldInfo)],
    fieldWrappers: List[FieldWrapper[R]]
  ): ReducedStep[R] =
    if (!fieldWrappers.exists(_.wrapPureValues) && items.map(_._2).forall(_.isInstanceOf[PureStep]))
      PureStep(ObjectValue(items.asInstanceOf[List[(String, PureStep, FieldInfo)]].map {
        case (k, v, _) => (k, v.value)
      }))
    else ReducedStep.ObjectStep(items)

  private def effectfulExecutionError(
    path: List[Either[String, Int]],
    locationInfo: Option[LocationInfo],
    e: Throwable
  ): ExecutionError =
    e match {
      case e: ExecutionError => e
      case other             => ExecutionError("Effect failure", path.reverse, locationInfo, Some(other))
    }

}
