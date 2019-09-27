package caliban

import caliban.CalibanError.ExecutionError
import caliban.Rendering.renderTypes
import caliban.execution.Executor
import caliban.introspection.Introspector
import caliban.introspection.adt.{ __Introspection, __Type }
import caliban.parsing.Parser
import caliban.parsing.adt.Value
import caliban.schema.RootSchema.Operation
import caliban.schema._
import caliban.validation.Validator
import zio.stream.ZStream
import zio.{ IO, Runtime, ZIO }

class GraphQL[Q, M, S](schema: RootSchema[Q, M, S]) {

  private val rootType =
    RootType(
      schema.query.schema.toType(),
      schema.mutation.map(_.schema.toType()),
      schema.subscription.map(_.schema.toType())
    )
  private val introspectionRootSchema: RootSchema[__Introspection, Nothing, Nothing] = Introspector.introspect(rootType)
  private val introspectionRootType                                                  = RootType(introspectionRootSchema.query.schema.toType(), None, None)

  def render: String = renderTypes(rootType.types)

  def execute(query: String, operationName: Option[String] = None): IO[CalibanError, ResponseValue] =
    for {
      document <- Parser.parseQuery(query)
      intro    = Introspector.isIntrospection(document)
      _        <- Validator.validate(document, if (intro) introspectionRootType else rootType)
      result   <- Executor.executeRequest(document, if (intro) introspectionRootSchema else schema, operationName)
    } yield result
}

object GraphQL {

  def graphQL[Q, M, S: SubscriptionSchema](
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
      override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
      override def resolve(value: ZIO[R, E, A], arguments: Map[String, Value]): IO[ExecutionError, ResolvedValue] =
        value.flatMap(ev.resolve(_, arguments)).provide(runtime.Environment).mapError {
          case e: ExecutionError => e
          case other             => ExecutionError("Caught error during execution of effectful field", Some(other))
        }
    }

  implicit def streamSchema[R, E <: Throwable, A](
    implicit ev: Schema[A],
    runtime: Runtime[R]
  ): Schema[ZStream[R, E, A]] =
    new Schema[ZStream[R, E, A]] {
      override def toType(isInput: Boolean = false): __Type = ev.toType(isInput)
      override def resolve(
        stream: ZStream[R, E, A],
        arguments: Map[String, Value]
      ): IO[ExecutionError, ResolvedValue] =
        IO.succeed(
          ResolvedValue.ResolvedStreamValue(stream.mapM(ev.resolve(_, arguments)).provide(runtime.Environment))
        )
    }

}
