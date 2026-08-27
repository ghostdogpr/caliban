package caliban.gateway

import sttp.client4.httpclient.zio.SttpClient
import sttp.model.Uri

package object internal {
  private[gateway] def unmanagedRemoteGraphQLSource[R](
    endpoint: Uri,
    backend: SttpClient,
    config: RemoteGraphQLConfig[R] = RemoteGraphQLConfig.default,
    responseStructureLimits: RemoteGraphQLSource.ResponseStructureLimits =
      RemoteGraphQLSource.ResponseStructureLimits.default
  ): RemoteGraphQLSource[R] =
    new RemoteGraphQLSource(
      "remote",
      endpoint,
      backend,
      config,
      responseStructureLimits,
      None,
      None,
      GatewayWrapper.empty
    )
}
