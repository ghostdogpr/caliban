package caliban.execution

import scala.collection.immutable.ListMap
import caliban.ResponseValue._
import caliban.Value._
import caliban.parsing.adt._
import caliban.schema.Step._
import caliban.schema.{ GenericSchema, ReducedStep, Step }
import caliban.wrappers.Wrapper.FieldWrapper
import caliban.{ CalibanError, GraphQLResponse, InputValue, ResponseValue }
import zio._
import zquery.{ Described, ZQuery }

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

    executePlan(plan, request.field, request.variableDefinitions, variables, allowParallelism, fieldWrappers)
  }

  private[caliban] def fail(error: CalibanError): UIO[GraphQLResponse[CalibanError]] =
    IO.succeed(GraphQLResponse(NullValue, List(error)))

  private def executePlan[R](
    plan: Step[R],
    root: Field,
    variableDefinitions: List[VariableDefinition],
    variableValues: Map[String, InputValue],
    allowParallelism: Boolean,
    fieldWrappers: List[FieldWrapper[R]]
  ): URIO[R, GraphQLResponse[CalibanError]] = {

    def reduceStep(
      step: Step[R],
      currentField: Field,
      arguments: Map[String, InputValue],
      path: List[Either[String, Int]]
    ): ReducedStep[R] =
      step match {
        case s @ PureStep(value) =>
          value match {
            case EnumValue(v) if mergeFields(currentField, v).collectFirst {
                  case Field("__typename", _, _, _, _, _, _) => true
                }.nonEmpty =>
              // special case of an hybrid union containing case objects, those should return an object instead of a string
              val obj = mergeFields(currentField, v).collectFirst {
                case Field(name @ "__typename", _, _, alias, _, _, _) =>
                  ObjectValue(List(alias.getOrElse(name) -> StringValue(v)))
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
            case f @ Field(name @ "__typename", _, _, alias, _, _, _) =>
              (alias.getOrElse(name), PureStep(StringValue(objectName)), fieldInfo(f, path))
            case f @ Field(name, _, _, alias, _, _, args) =>
              val arguments = resolveVariables(args, variableDefinitions, variableValues)
              (
                alias.getOrElse(name),
                fields
                  .get(name)
                  .fold(NullStep: ReducedStep[R])(reduceStep(_, f, arguments, Left(alias.getOrElse(name)) :: path)),
                fieldInfo(f, path)
              )
          }
          reduceObject(items, fieldWrappers)
        case QueryStep(inner) =>
          ReducedStep.QueryStep(
            inner.bimap(GenericSchema.effectfulExecutionError(path, _), reduceStep(_, currentField, arguments, path))
          )
        case StreamStep(stream) =>
          ReducedStep.StreamStep(
            stream.bimap(GenericSchema.effectfulExecutionError(path, _), reduceStep(_, currentField, arguments, path))
          )
      }

    def makeQuery(step: ReducedStep[R], errors: Ref[List[CalibanError]]): ZQuery[R, Nothing, ResponseValue] = {

      def wrap(query: ZQuery[R, Nothing, ResponseValue])(
        wrappers: List[FieldWrapper[R]],
        fieldInfo: FieldInfo
      ): ZQuery[R, Nothing, ResponseValue] =
        wrappers match {
          case Nil => query
          case wrapper :: tail =>
            ZQuery
              .environment[R]
              .flatMap(
                env =>
                  wrap(
                    wrapper
                      .f(query.provide(Described(env, "Wrapper")), fieldInfo)
                      .foldM(error => ZQuery.fromEffect(errors.update(error :: _)).map(_ => NullValue), ZQuery.succeed)
                  )(tail, fieldInfo)
              )
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
      reduced      = reduceStep(plan, root, Map(), Nil)
      query        = makeQuery(reduced, errors)
      result       <- query.run
      resultErrors <- errors.get
    } yield GraphQLResponse(result, resultErrors.reverse)
  }

  private def resolveVariables(
    arguments: Map[String, InputValue],
    variableDefinitions: List[VariableDefinition],
    variableValues: Map[String, InputValue]
  ): Map[String, InputValue] =
    arguments.map {
      case (k, v) =>
        k -> (v match {
          case InputValue.VariableValue(name) =>
            lazy val defaultInputValue = (for {
              definition <- variableDefinitions.find(_.name == name)
              inputValue <- definition.defaultValue
            } yield inputValue) getOrElse v
            variableValues.getOrElse(name, defaultInputValue)
          case value => value
        })
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

  private def fieldInfo(field: Field, path: List[Either[String, Int]]): FieldInfo =
    FieldInfo(field.alias.getOrElse(field.name), path, field.parentType, field.fieldType)

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

}
