package caliban.gateway.internal

import caliban.gateway.GatewayRuntime
import caliban.gateway.GatewayRuntime.{ OperationCacheStatus, Status }
import caliban.{ CalibanError, GraphQLRequest, GraphQLResponse }
import zio.{ Trace, UIO, ZIO }

private[gateway] final class RuntimeControl private (
  requests: ExecutionGate,
  sources: Map[String, ExecutionGate]
) {

  def admit[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
    requests(effect)

  def source[R](name: String, source: GraphQLSource[R]): GraphQLSource[R] =
    sources.get(name).fold(source)(new GatedGraphQLSource(source, _))

  def status(cache: OperationCacheStatus)(implicit trace: Trace): UIO[Status] =
    requests.status.zipWith(
      ZIO.foreach(sources) { case (name, gate) => gate.status.map(name -> _) }
    ) { (requestStatus, sourceStatus) =>
      GatewayRuntime.Status(
        requestStatus,
        sourceStatus,
        cache
      )
    }

  private final class GatedGraphQLSource[-R](
    underlying: GraphQLSource[R],
    gate: ExecutionGate
  ) extends GraphQLSource[R] {
    val errorPolicy: GraphQLSource.ErrorPolicy = underlying.errorPolicy

    def execute(request: GraphQLRequest)(implicit
      trace: Trace
    ): ZIO[R, GraphQLSource.Failure, GraphQLResponse[CalibanError]] =
      gate(underlying.execute(request))
  }
}

private[gateway] object RuntimeControl {
  def make(requestLimit: Int, sourceLimits: Map[String, Int])(implicit trace: Trace): UIO[RuntimeControl] =
    for {
      requests <- ExecutionGate.make(requestLimit)
      sources  <- ZIO.foreach(sourceLimits) { case (name, limit) => ExecutionGate.make(limit).map(name -> _) }
    } yield new RuntimeControl(requests, sources)
}
