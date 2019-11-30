package caliban.execution

import scala.collection.immutable.ListMap
import caliban.CalibanError.ExecutionError
import caliban.ResponseValue._
import caliban.Value._
import caliban.execution.QueryAnalyzer.QueryAnalyzer
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.OperationType.{ Mutation, Query, Subscription }
import caliban.parsing.adt._
import caliban.schema.Step._
import caliban.schema.{ GenericSchema, ReducedStep, RootSchema, Step }
import caliban.{ CalibanError, GraphQLResponse, InputValue, ResponseValue }
import zio._
import zquery.ZQuery

object Executor {

  /**
   * Executes the given query against a schema. It returns either an [[caliban.CalibanError.ExecutionError]] or a [[ResponseValue]].
   * @param document the parsed query
   * @param schema the schema to use to run the query
   * @param operationName the operation to run in case the query contains multiple operations.
   * @param variables a list of variables.
   */
  def executeRequest[R, Q, M, S](
    document: Document,
    schema: RootSchema[R, Q, M, S],
    operationName: Option[String] = None,
    variables: Map[String, InputValue] = Map(),
    queryAnalyzers: List[QueryAnalyzer[R]] = Nil
  ): URIO[R, GraphQLResponse[CalibanError]] = {
    val fragments = document.definitions.collect {
      case fragment: FragmentDefinition => fragment.name -> fragment
    }.toMap
    val operation = operationName match {
      case Some(name) =>
        document.definitions.collectFirst { case op: OperationDefinition if op.name.contains(name) => op }
          .toRight(s"Unknown operation $name.")
      case None =>
        document.definitions.collect { case op: OperationDefinition => op } match {
          case head :: Nil => Right(head)
          case _           => Left("Operation name is required.")
        }
    }
    operation match {
      case Left(error) => fail(ExecutionError(error))
      case Right(op) =>
        def executeOperation(root: Field): URIO[R, GraphQLResponse[CalibanError]] = {

          def exec[A](plan: Step[R], allowParallelism: Boolean): URIO[R, GraphQLResponse[CalibanError]] =
            executePlan(plan, root, op.variableDefinitions, variables, allowParallelism)

          op.operationType match {
            case Query => exec(schema.query.plan, allowParallelism = true)
            case Mutation =>
              schema.mutation match {
                case Some(m) => exec(m.plan, allowParallelism = false)
                case None    => fail(ExecutionError("Mutations are not supported on this schema"))
              }
            case Subscription =>
              schema.subscription match {
                case Some(m) => exec(m.plan, allowParallelism = true)
                case None    => fail(ExecutionError("Subscriptions are not supported on this schema"))
              }
          }

        }

        ZIO
          .foldLeft(queryAnalyzers)(Field(op.selectionSet, fragments, variables)) {
            case (field, analyzer) => analyzer(field)
          }
          .foldM(fail, executeOperation)
    }
  }

  private[caliban] def fail(error: CalibanError): UIO[GraphQLResponse[CalibanError]] =
    IO.succeed(GraphQLResponse(NullValue, List(error)))

  private def executePlan[R](
    plan: Step[R],
    root: Field,
    variableDefinitions: List[VariableDefinition],
    variableValues: Map[String, InputValue],
    allowParallelism: Boolean
  ): URIO[R, GraphQLResponse[CalibanError]] = {

    def reduceStep(
      step: Step[R],
      currentField: Field,
      arguments: Map[String, InputValue]
    ): ReducedStep[R] =
      step match {
        case s @ PureStep(value) =>
          value match {
            case EnumValue(v) if mergeFields(currentField, v).collectFirst {
                  case Field("__typename", _, _, _, _) => true
                }.nonEmpty =>
              // special case of an hybrid union containing case objects, those should return an object instead of a string
              val obj = mergeFields(currentField, v).collectFirst {
                case Field(name @ "__typename", alias, _, _, _) =>
                  ObjectValue(List(alias.getOrElse(name) -> StringValue(v)))
              }
              obj.fold(s)(PureStep(_))
            case _ => s
          }
        case FunctionStep(step) => reduceStep(step(arguments), currentField, Map())
        case ListStep(steps)    => reduceList(steps.map(reduceStep(_, currentField, arguments)))
        case ObjectStep(objectName, fields) =>
          val mergedFields = mergeFields(currentField, objectName)
          val items = mergedFields.map {
            case Field(name @ "__typename", alias, _, _, _) =>
              alias.getOrElse(name) -> PureStep(StringValue(objectName))
            case f @ Field(name, alias, _, _, args) =>
              val arguments = resolveVariables(args, variableDefinitions, variableValues)
              alias.getOrElse(name) ->
                fields
                  .get(name)
                  .map(reduceStep(_, f, arguments))
                  .getOrElse(NullStep)
          }
          reduceObject(items)
        case QueryStep(inner) =>
          ReducedStep.QueryStep(
            inner
              .map(reduceStep(_, currentField, arguments))
              .mapError(GenericSchema.effectfulExecutionError(currentField.name, _))
          )
        case StreamStep(stream) =>
          ReducedStep.StreamStep(
            stream.bimap(
              GenericSchema.effectfulExecutionError(currentField.name, _),
              reduceStep(_, currentField, arguments)
            )
          )
      }

    def makeQuery(step: ReducedStep[R], errors: Ref[List[CalibanError]]): ZQuery[R, Nothing, ResponseValue] = {
      def loop(step: ReducedStep[R]): ZQuery[R, Nothing, ResponseValue] =
        step match {
          case PureStep(value) => ZQuery.succeed(value)
          case ReducedStep.ListStep(steps) =>
            val queries = steps.map(loop)
            (if (allowParallelism) ZQuery.collectAllPar(queries) else ZQuery.collectAll(queries)).map(ListValue)
          case ReducedStep.ObjectStep(steps) =>
            val queries = steps.map { case (name, field) => loop(field).map(name -> _) }
            (if (allowParallelism) ZQuery.collectAllPar(queries) else ZQuery.collectAll(queries)).map(ObjectValue)
          case ReducedStep.QueryStep(step) =>
            step.fold(Left(_), Right(_)).flatMap {
              case Left(error)  => ZQuery.fromEffect(errors.update(error :: _)).map(_ => NullValue)
              case Right(query) => loop(query)
            }
          case ReducedStep.StreamStep(stream) =>
            ZQuery
              .fromEffect(ZIO.environment[R])
              .map(env => ResponseValue.StreamValue(stream.mapM(loop(_).run).provide(env)))
        }
      loop(step)
    }

    for {
      errors       <- Ref.make(List.empty[CalibanError])
      reduced      = reduceStep(plan, root, Map())
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
            variableValues.get(name) orElse variableDefinitions.find(_.name == name).flatMap(_.defaultValue) getOrElse v
          case value => value
        })
    }

  private[caliban] def mergeFields(field: Field, typeName: String): List[Field] = {
    val allFields = field.fields ++ field.conditionalFields.getOrElse(typeName, Nil)
    allFields
      .foldLeft(ListMap.empty[String, Field]) {
        case (result, field) =>
          result.updated(
            field.name,
            result
              .get(field.name)
              .fold(field)(f => f.copy(fields = f.fields ++ field.fields))
          )
      }
      .values
      .toList
  }

  private def reduceList[R](list: List[ReducedStep[R]]): ReducedStep[R] =
    if (list.forall(_.isInstanceOf[PureStep]))
      PureStep(ListValue(list.asInstanceOf[List[PureStep]].map(_.value)))
    else ReducedStep.ListStep(list)

  private def reduceObject[R](items: List[(String, ReducedStep[R])]): ReducedStep[R] =
    if (items.map(_._2).forall(_.isInstanceOf[PureStep]))
      PureStep(ObjectValue(items.asInstanceOf[List[(String, PureStep)]].map {
        case (k, v) => k -> v.value
      }))
    else ReducedStep.ObjectStep(items)

}
