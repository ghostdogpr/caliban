package caliban

import caliban.CalibanError.ExecutionError
import caliban.Rendering.renderTypes
import caliban.introspection.Introspector
import caliban.parsing.Parser
import caliban.parsing.adt.ExecutableDefinition.OperationDefinition
import caliban.parsing.adt.OperationType._
import caliban.parsing.adt.Selection.Field
import caliban.parsing.adt.{ Document, Selection, Value }
import caliban.schema.RootSchema.Operation
import caliban.schema.Types.Type
import caliban.schema.{ ResponseValue, RootSchema, RootType, Schema }
import caliban.validation.Validator
import zio.{ IO, Runtime, ZIO }

class GraphQL[Q, M, S](schema: RootSchema[Q, M, S]) {

  val rootType =
    RootType(schema.query.schema.toType, schema.mutation.map(_.schema.toType), schema.subscription.map(_.schema.toType))
  val (introspectionSchema, introspectionResolver) = Introspector.introspect(rootType)
  val introspectionRootType                        = RootType(introspectionSchema.toType, None, None)

  def render: String = renderTypes(rootType.types)

  def execute(query: String): IO[CalibanError, List[ResponseValue]] =
    for {
      document <- Parser.parseQuery(query)
      intro    = isIntrospection(document)
      _        <- if (intro) Validator.validate(document, introspectionRootType) else Validator.validate(document, rootType)
      result <- IO.collectAll(document.definitions.collect {
                 case OperationDefinition(opType, _, _, _, selection) =>
                   if (intro) introspectionSchema.exec(introspectionResolver, selection)
                   else
                     opType match {
                       case Query => schema.query.schema.exec(schema.query.resolver, selection)
                       case Mutation =>
                         schema.mutation match {
                           case Some(m) => m.schema.exec(m.resolver, selection)
                           case None    => IO.fail(ExecutionError("Mutations are not supported on this schema"))
                         }
                       case Subscription =>
                         schema.subscription match {
                           case Some(m) => m.schema.exec(m.resolver, selection)
                           case None    => IO.fail(ExecutionError("Subscriptions are not supported on this schema"))
                         }
                     }
               })
    } yield result

  private def isIntrospection(document: Document): Boolean =
    document.definitions.forall {
      case OperationDefinition(_, _, _, _, selectionSet) =>
        selectionSet.forall {
          case Field(_, name, _, _, _) => name == "__schema" || name == "__type"
          case _                       => false
        }
      case _ => false
    }
}

object GraphQL {

  def graphQL[Q, M, S](
    resolver: RootResolver[Q, M, S]
  )(implicit querySchema: Schema[Q], mutationSchema: Schema[M], subscriptionSchema: Schema[S]): GraphQL[Q, M, S] =
    new GraphQL[Q, M, S](
      RootSchema(
        Operation(querySchema, resolver.queryResolver),
        resolver.mutationResolver.map(Operation(mutationSchema, _)),
        resolver.subscriptionResolver.map(Operation(subscriptionSchema, _))
      )
    )

  implicit def effectSchema[R, E <: Throwable, A](implicit ev: Schema[A], runtime: Runtime[R]): Schema[ZIO[R, E, A]] =
    new Schema[ZIO[R, E, A]] {
      override def toType: Type = ev.toType
      override def exec(
        value: ZIO[R, E, A],
        selectionSet: List[Selection],
        arguments: Map[String, Value]
      ): IO[ExecutionError, ResponseValue] =
        value.flatMap(ev.exec(_, selectionSet, arguments)).provide(runtime.Environment).mapError {
          case e: ExecutionError => e
          case other             => ExecutionError("Caught error during execution of effectful field", Some(other))
        }
    }

}
