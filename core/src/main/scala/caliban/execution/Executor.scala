package caliban.execution

import scala.collection.immutable.ListMap
import caliban.CalibanError.ExecutionError
import caliban.ResponseValue._
import caliban.Value._
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.OperationType.{ Mutation, Query, Subscription }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt._
import caliban.schema.Step._
import caliban.schema.{ GenericSchema, ReducedStep, RootSchema, Step }
import caliban.{ CalibanError, GraphQLResponse, InputValue, ResponseValue }
import zio.{ IO, Ref, UIO, URIO, ZIO }
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
    variables: Map[String, InputValue] = Map()
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
        def executeOperation[A](
          plan: Step[R],
          allowParallelism: Boolean
        ): URIO[R, GraphQLResponse[CalibanError]] =
          executePlan(
            plan,
            op.selectionSet,
            fragments,
            op.variableDefinitions,
            variables,
            allowParallelism
          )

        op.operationType match {
          case Query => executeOperation(schema.query.plan, allowParallelism = true)
          case Mutation =>
            schema.mutation match {
              case Some(m) => executeOperation(m.plan, allowParallelism = false)
              case None    => fail(ExecutionError("Mutations are not supported on this schema"))
            }
          case Subscription =>
            schema.subscription match {
              case Some(m) => executeOperation(m.plan, allowParallelism = true)
              case None    => fail(ExecutionError("Subscriptions are not supported on this schema"))
            }
        }
    }
  }

  private[caliban] def fail(error: CalibanError): UIO[GraphQLResponse[CalibanError]] =
    IO.succeed(GraphQLResponse(NullValue, List(error)))

  private def executePlan[R](
    plan: Step[R],
    selectionSet: List[Selection],
    fragments: Map[String, FragmentDefinition],
    variableDefinitions: List[VariableDefinition],
    variableValues: Map[String, InputValue],
    allowParallelism: Boolean
  ): URIO[R, GraphQLResponse[CalibanError]] = {

    def reduceStep(
      step: Step[R],
      selectionSet: List[Selection],
      arguments: Map[String, InputValue],
      fieldName: String
    ): ReducedStep[R] =
      step match {
        case s @ PureStep(value) =>
          value match {
            case EnumValue(v) if selectionSet.collectFirst {
                  case Selection.Field(_, "__typename", _, _, _) => true
                }.nonEmpty =>
              // special case of an hybrid union containing case objects, those should return an object instead of a string
              val mergedSelectionSet = mergeSelectionSet(selectionSet, v, fragments, variableValues)
              val obj = mergedSelectionSet.collectFirst {
                case Selection.Field(alias, name @ "__typename", _, _, _) =>
                  ObjectValue(List(alias.getOrElse(name) -> StringValue(v)))
              }
              obj.fold(s)(PureStep(_))
            case _ => s
          }
        case FunctionStep(step) => reduceStep(step(arguments), selectionSet, Map(), fieldName)
        case ListStep(steps)    => reduceList(steps.map(reduceStep(_, selectionSet, arguments, fieldName)))
        case ObjectStep(objectName, fields) =>
          val mergedSelectionSet = mergeSelectionSet(selectionSet, objectName, fragments, variableValues)
          val items = mergedSelectionSet.map {
            case Selection.Field(alias, name @ "__typename", _, _, _) =>
              alias.getOrElse(name) -> PureStep(StringValue(objectName))
            case Selection.Field(alias, name, args, _, selectionSet) =>
              val arguments = resolveVariables(args, variableDefinitions, variableValues)
              alias.getOrElse(name) -> fields
                .get(name)
                .map(reduceStep(_, selectionSet, arguments, name))
                .getOrElse(NullStep)
          }
          reduceObject(items)
        case QueryStep(inner) =>
          ReducedStep.QueryStep(
            inner
              .map(reduceStep(_, selectionSet, arguments, fieldName))
              .mapError(GenericSchema.effectfulExecutionError(fieldName, _))
          )
        case StreamStep(stream) =>
          ReducedStep.StreamStep(
            stream.bimap(
              GenericSchema.effectfulExecutionError(fieldName, _),
              reduceStep(_, selectionSet, arguments, fieldName)
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
      reduced      = reduceStep(plan, selectionSet, Map(), "")
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

  private[caliban] def mergeSelectionSet(
    selectionSet: List[Selection],
    name: String,
    fragments: Map[String, FragmentDefinition],
    variableValues: Map[String, InputValue]
  ): List[Field] = {
    val fields = selectionSet.flatMap {
      case f: Field if checkDirectives(f.directives, variableValues) => List(f)
      case InlineFragment(typeCondition, directives, sel) if checkDirectives(directives, variableValues) =>
        val matching = typeCondition.fold(true)(_.name == name)
        if (matching) mergeSelectionSet(sel, name, fragments, variableValues) else Nil
      case FragmentSpread(spreadName, directives) if checkDirectives(directives, variableValues) =>
        fragments.get(spreadName) match {
          case Some(fragment) if fragment.typeCondition.name == name =>
            mergeSelectionSet(fragment.selectionSet, name, fragments, variableValues)
          case _ => Nil
        }
      case _ => Nil
    }
    fields
      .foldLeft(ListMap.empty[String, Field]) {
        case (result, field) =>
          val name = field.alias.getOrElse(field.name)
          result.updated(
            name,
            result
              .get(name)
              .fold(field)(f => f.copy(selectionSet = f.selectionSet ++ field.selectionSet))
          )
      }
      .values
      .toList
  }

  private def checkDirectives(directives: List[Directive], variableValues: Map[String, InputValue]): Boolean =
    !checkDirective("skip", default = false, directives, variableValues) &&
      checkDirective("include", default = true, directives, variableValues)

  private def checkDirective(
    name: String,
    default: Boolean,
    directives: List[Directive],
    variableValues: Map[String, InputValue]
  ): Boolean =
    directives
      .find(_.name == name)
      .flatMap(_.arguments.get("if")) match {
      case Some(BooleanValue(value)) => value
      case Some(InputValue.VariableValue(name)) =>
        variableValues
          .get(name) match {
          case Some(BooleanValue(value)) => value
          case _                         => default
        }
      case _ => default
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
