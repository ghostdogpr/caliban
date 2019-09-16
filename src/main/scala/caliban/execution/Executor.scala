package caliban.execution

import caliban.CalibanError.ExecutionError
import caliban.introspection.Introspector.Introspection
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.OperationType.{ Mutation, Query, Subscription }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.parsing.adt.{ Document, Selection }
import caliban.schema.{ ResponseValue, RootSchema, Schema }
import zio.IO

object Executor {

  def execute[Q, M, S](
    document: Document,
    schema: Either[RootSchema[Q, M, S], (Schema[Introspection], Introspection)],
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
      schema match {
        case Left(schema) =>
          op.operationType match {
            case Query => schema.query.schema.exec(schema.query.resolver, op.selectionSet, Map(), fragments)
            case Mutation =>
              schema.mutation match {
                case Some(m) => m.schema.exec(m.resolver, op.selectionSet, Map(), fragments)
                case None    => IO.fail(ExecutionError("Mutations are not supported on this schema"))
              }
            case Subscription =>
              schema.subscription match {
                case Some(m) => m.schema.exec(m.resolver, op.selectionSet, Map(), fragments)
                case None    => IO.fail(ExecutionError("Subscriptions are not supported on this schema"))
              }
          }
        case Right((schema, resolver)) =>
          // introspection
          schema.exec(resolver, op.selectionSet, Map(), fragments)
      }
    }
  }

  def mergeSelectionSet(
    selectionSet: List[Selection],
    name: String,
    fragments: Map[String, FragmentDefinition]
  ): List[Field] =
    selectionSet.flatMap {
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

}
