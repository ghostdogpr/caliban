package caliban.gateway.internal

import caliban.Value.NullValue
import caliban.execution.{ Executor, RequestPreparation }
import caliban.introspection.Introspector
import caliban.schema.{ RootSchema, RootType }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse }
import caliban.gateway.GatewayRuntime
import zio.{ IO, Trace, URIO, ZIO }

private[gateway] final class RemoteGatewayRuntime[-R](rootType: RootType, source: RemoteGraphQLSource)
    extends GatewayRuntime[R] {

  private val introspection: RootSchema[Any] = Introspector.introspect[Any](rootType)

  def check(query: String)(implicit trace: Trace): IO[CalibanError, Unit] =
    RequestPreparation.check(query, rootType)

  def executeRequest(request: GraphQLRequest)(implicit trace: Trace): URIO[R, GraphQLResponse[CalibanError]] =
    RequestPreparation
      .prepare(request, rootType)
      .foldZIO(
        Executor.fail,
        prepared =>
          if (prepared.isIntrospection)
            Executor.executeRequest(prepared.executionRequest, introspection.query.plan)
          else
            source
              .execute(request)
              .catchAll(_ =>
                ZIO.succeed(
                  GraphQLResponse(
                    NullValue,
                    List(CalibanError.ExecutionError("Remote GraphQL request failed."))
                  )
                )
              )
      )
}
