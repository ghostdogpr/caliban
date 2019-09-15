package caliban.execution

import caliban.CalibanError.ExecutionError
import caliban.introspection.Introspector.Introspection
import caliban.parsing.adt.ExecutableDefinition.{ FragmentDefinition, OperationDefinition }
import caliban.parsing.adt.OperationType.{ Mutation, Query, Subscription }
import caliban.parsing.adt.{ Document, Selection }
import caliban.parsing.adt.Selection.{ Field, FragmentSpread, InlineFragment }
import caliban.schema.{ ResponseValue, RootSchema, Schema }
import zio.IO

object Executor {

  def execute[Q, M, S](
    document: Document,
    schema: Either[RootSchema[Q, M, S], (Schema[Introspection], Introspection)]
  ): IO[ExecutionError, List[ResponseValue]] = {
    val fragments = document.definitions.collect {
      case fragment: FragmentDefinition => fragment.name -> fragment
    }.toMap
    IO.collectAll(document.definitions.collect {
      case OperationDefinition(opType, _, _, _, selection) =>
        schema match {
          case Left(schema) =>
            opType match {
              case Query => schema.query.schema.exec(schema.query.resolver, selection, Map(), fragments)
              case Mutation =>
                schema.mutation match {
                  case Some(m) => m.schema.exec(m.resolver, selection, Map(), fragments)
                  case None    => IO.fail(ExecutionError("Mutations are not supported on this schema"))
                }
              case Subscription =>
                schema.subscription match {
                  case Some(m) => m.schema.exec(m.resolver, selection, Map(), fragments)
                  case None    => IO.fail(ExecutionError("Subscriptions are not supported on this schema"))
                }
            }
          case Right((schema, resolver)) =>
            // introspection
            schema.exec(resolver, selection, Map(), fragments)
        }
    })
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
