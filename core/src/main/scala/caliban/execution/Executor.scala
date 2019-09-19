package caliban.execution

import caliban.CalibanError.ExecutionError
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.OperationType.{ Mutation, Query, Subscription }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.{ Document, Selection }
import caliban.schema.RootSchema.Operation
import caliban.schema.{ ResponseValue, RootSchema }
import zio.IO

object Executor {

  def execute[Q, M, S](
    document: Document,
    schema: RootSchema[Q, M, S],
    operationName: Option[String]
  ): IO[ExecutionError, ResponseValue] = {
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
      def exec[A](x: Operation[A], parallel: Boolean): IO[ExecutionError, ResponseValue] =
        x.schema.exec(x.resolver, op.selectionSet, Map(), fragments, parallel)
      op.operationType match {
        case Query => exec(schema.query, parallel = true)
        case Mutation =>
          schema.mutation match {
            case Some(m) => exec(m, parallel = false)
            case None    => IO.fail(ExecutionError("Mutations are not supported on this schema"))
          }
        case Subscription =>
          schema.subscription match {
            case Some(_) => ??? // TODO
            case None    => IO.fail(ExecutionError("Subscriptions are not supported on this schema"))
          }
      }
    }
  }

  def mergeSelectionSet(
    selectionSet: List[Selection],
    name: String,
    fragments: Map[String, FragmentDefinition]
  ): List[Field] = {
    val fields = selectionSet.flatMap {
      case f: Field => List(f)
      case InlineFragment(typeCondition, _, sel) =>
        val matching = typeCondition.fold(true)(_.name == name)
        if (matching) mergeSelectionSet(sel, name, fragments) else Nil
      case FragmentSpread(spreadName, _) =>
        fragments.get(spreadName) match {
          case Some(fragment) if fragment.typeCondition.name == name =>
            mergeSelectionSet(fragment.selectionSet, name, fragments)
          case _ => Nil
        }
    }
    fields
      .groupBy(_.name)
      .toList
      .flatMap {
        case (_, head :: fields) => Some(head.copy(selectionSet = head.selectionSet ++ fields.flatMap(_.selectionSet)))
        case _                   => None
      }
  }

}
