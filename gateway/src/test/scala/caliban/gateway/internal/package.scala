package caliban.gateway

import caliban.gateway.internal.execution.RemoteSubgraphExecutor
import sttp.client4.httpclient.zio.SttpClient
import sttp.model.Uri

package object internal {
  private[gateway] def unmanagedRemoteSubgraphExecutor[R](
    endpoint: Uri,
    backend: SttpClient,
    config: RemoteGraphQLConfig[R] = RemoteGraphQLConfig.default,
    responseStructureLimits: RemoteSubgraphExecutor.ResponseStructureLimits =
      RemoteSubgraphExecutor.ResponseStructureLimits.default,
    remoteErrorMessages: Boolean = false
  ): RemoteSubgraphExecutor[R] =
    new RemoteSubgraphExecutor(
      "remote",
      endpoint,
      backend,
      config,
      responseStructureLimits,
      None,
      None,
      GatewayWrapper.empty,
      remoteErrorMessages
    )
}
