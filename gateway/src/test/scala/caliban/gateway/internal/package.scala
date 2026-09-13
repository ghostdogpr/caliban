package caliban.gateway

import caliban.gateway.internal.execution.RemoteSubgraphExecutor
import zio.http.URL

package object internal {
  private[gateway] def unmanagedRemoteSubgraphExecutor[R](
    endpoint: URL,
    http: GatewayHttpClient,
    config: RemoteGraphQLConfig[R] = RemoteGraphQLConfig.default,
    responseStructureLimits: RemoteSubgraphExecutor.ResponseStructureLimits =
      RemoteSubgraphExecutor.ResponseStructureLimits.default,
    remoteErrorMessages: Boolean = false
  ): RemoteSubgraphExecutor[R] =
    new RemoteSubgraphExecutor(
      "remote",
      endpoint,
      http,
      config,
      responseStructureLimits,
      None,
      None,
      GatewayWrapper.empty,
      remoteErrorMessages
    )
}
