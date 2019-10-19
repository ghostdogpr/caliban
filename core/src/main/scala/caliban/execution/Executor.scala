package caliban.execution

import scala.collection.immutable.ListMap
import caliban.CalibanError.ExecutionError
import caliban.ResponseValue
import caliban.ResponseValue._
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.OperationType.{ Mutation, Query, Subscription }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt._
import caliban.schema.RootSchema.Operation
import caliban.schema.Step._
import caliban.schema.{ Fetch, ReducedStep, RootSchema, Step }
import zio.{ IO, UIO, ZIO }

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
    variables: Map[String, Value] = Map(),
    parallelism: Int = 1
  ): ZIO[R, ExecutionError, ResponseValue] = {
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
    IO.fromEither(operation).mapError(ExecutionError(_)).flatMap { op =>
      def executeOperation[A](x: Operation[R, A], allowParallelism: Boolean): ZIO[R, ExecutionError, ResponseValue] =
        executePlan(
          x.schema.resolve(x.resolver),
          op.selectionSet,
          fragments,
          op.variableDefinitions,
          variables,
          allowParallelism
        )

      op.operationType match {
        case Query => executeOperation(schema.query, allowParallelism = true)
        case Mutation =>
          schema.mutation match {
            case Some(m) => executeOperation(m, allowParallelism = false)
            case None    => IO.fail(ExecutionError("Mutations are not supported on this schema"))
          }
        case Subscription =>
          schema.subscription match {
            case Some(m) => executeOperation(m, allowParallelism = true)
            case None    => IO.fail(ExecutionError("Subscriptions are not supported on this schema"))
          }
      }
    }
  }

  private def executePlan[R](
    plan: Step[R],
    selectionSet: List[Selection],
    fragments: Map[String, FragmentDefinition],
    variableDefinitions: List[VariableDefinition],
    variableValues: Map[String, Value],
    allowParallelism: Boolean
  ): ZIO[R, ExecutionError, ResponseValue] = {

    def reduceStep(step: Step[R], selectionSet: List[Selection], arguments: Map[String, Value]): ReducedStep[R] =
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
        case FunctionStep(step) => reduceStep(step(arguments), selectionSet, Map())
        case ListStep(steps)    => reduceList(steps.map(reduceStep(_, selectionSet, arguments)))
        case ObjectStep(objectName, fields) =>
          val mergedSelectionSet = mergeSelectionSet(selectionSet, objectName, fragments, variableValues)
          val items = mergedSelectionSet.map {
            case Selection.Field(alias, name @ "__typename", _, _, _) =>
              alias.getOrElse(name) -> PureStep(StringValue(objectName))
            case Selection.Field(alias, name, args, _, selectionSet) =>
              val arguments = resolveVariables(args, variableDefinitions, variableValues)
              alias.getOrElse(name) -> fields
                .get(name)
                .map(reduceStep(_, selectionSet, arguments))
                .getOrElse(NullStep)
          }
          reduceObject(items)
        case EffectStep(inner)  => ReducedStep.EffectStep(inner.map(reduceStep(_, selectionSet, arguments)))
        case FetchStep(inner)   => ReducedStep.FetchStep(inner.map(reduceStep(_, selectionSet, arguments)))
        case StreamStep(stream) => ReducedStep.StreamStep(stream.map(reduceStep(_, selectionSet, arguments)))
      }

    def executeStep(step: ReducedStep[R]): ZIO[R, ExecutionError, ResponseValue] =
      step match {
        case PureStep(value) => UIO(value)
        case ReducedStep.StreamStep(inner) =>
          ZIO.environment[R].map(env => ResponseValue.StreamValue(inner.mapM(executeStep).provide(env)))
        case _ =>
          val ioList = collectIO(step)
          if (ioList.nonEmpty) {
            for {
              ioResults <- if (allowParallelism) ZIO.collectAllPar(ioList) else ZIO.collectAll(ioList)
              result    <- executeStep(replaceIO(step, ioResults))
            } yield result
          } else {
            val fetches = collectFetch(step).map(_.run)
            for {
              fetchResults <- ZIO.collectAllPar(fetches)
              result       <- executeStep(replaceFetch(step, fetchResults.map(_.getOrElse(NullStep))))
            } yield result
          }
      }

    executeStep(reduceStep(plan, selectionSet, Map()))
  }

  private def resolveVariables(
    arguments: Map[String, Value],
    variableDefinitions: List[VariableDefinition],
    variableValues: Map[String, Value]
  ): Map[String, Value] =
    arguments.map {
      case (k, v) =>
        k -> (v match {
          case Value.VariableValue(name) =>
            variableValues.get(name) orElse variableDefinitions.find(_.name == name).flatMap(_.defaultValue) getOrElse v
          case value => value
        })
    }

  private[caliban] def mergeSelectionSet(
    selectionSet: List[Selection],
    name: String,
    fragments: Map[String, FragmentDefinition],
    variableValues: Map[String, Value]
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
          result.updated(
            field.name,
            result
              .get(field.name)
              .fold(field)(f => f.copy(selectionSet = f.selectionSet ++ field.selectionSet))
          )
      }
      .values
      .toList
  }

  private def checkDirectives(directives: List[Directive], variableValues: Map[String, Value]): Boolean =
    !checkDirective("skip", default = false, directives, variableValues) &&
      checkDirective("include", default = true, directives, variableValues)

  private def checkDirective(
    name: String,
    default: Boolean,
    directives: List[Directive],
    variableValues: Map[String, Value]
  ): Boolean =
    directives
      .find(_.name == name)
      .flatMap(_.arguments.get("if")) match {
      case Some(Value.BooleanValue(value)) => value
      case Some(Value.VariableValue(name)) =>
        variableValues
          .get(name) match {
          case Some(Value.BooleanValue(value)) => value
          case _                               => default
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

  private def collect[R, A](step: ReducedStep[R], pf: PartialFunction[ReducedStep[R], List[A]]): List[A] =
    pf.applyOrElse(
      step, {
        case ReducedStep.ListStep(steps)    => steps.flatMap(collect(_, pf))
        case ReducedStep.ObjectStep(fields) => fields.flatMap { case (_, v) => collect(v, pf) }
        case _                              => Nil
      }
    )

  private def collectIO[R](step: ReducedStep[R]): List[ZIO[R, ExecutionError, ReducedStep[R]]] =
    collect[R, ZIO[R, ExecutionError, ReducedStep[R]]](step, { case ReducedStep.EffectStep(inner) => List(inner) })

  private def collectFetch[R](step: ReducedStep[R]): List[Fetch[ReducedStep[R]]] =
    collect[R, Fetch[ReducedStep[R]]](step, { case ReducedStep.FetchStep(inner) => List(inner) })

  private def replace[R, A](
    step: ReducedStep[R],
    list: List[ReducedStep[R]],
    pf: PartialFunction[ReducedStep[R], Unit]
  ): ReducedStep[R] = {
    var l = list // use var for better performance
    def replaceLoop(step: ReducedStep[R]): ReducedStep[R] =
      pf.lift(step) match {
        case Some(_) if l.nonEmpty =>
          val head = l.head
          l = l.tail
          head
        case _ =>
          step match {
            case ReducedStep.ListStep(steps)    => reduceList(steps.map(replaceLoop))
            case ReducedStep.ObjectStep(fields) => reduceObject(fields.map { case (k, v) => k -> replaceLoop(v) })
            case other                          => other
          }
      }
    replaceLoop(step)
  }

  private def replaceIO[R](step: ReducedStep[R], list: List[ReducedStep[R]]): ReducedStep[R] =
    replace(step, list, { case ReducedStep.EffectStep(_) => () })

  private def replaceFetch[R](step: ReducedStep[R], list: List[ReducedStep[R]]): ReducedStep[R] =
    replace(step, list, { case ReducedStep.FetchStep(_) => () })

}
