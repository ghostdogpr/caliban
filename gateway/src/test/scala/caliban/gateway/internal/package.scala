package caliban.gateway

import sttp.client4.httpclient.zio.SttpClient
import sttp.model.Uri

package object internal {
  private[gateway] def unmanagedRemoteGraphQLSource[R](
    endpoint: Uri,
    backend: SttpClient,
    config: RemoteGraphQLConfig[R] = RemoteGraphQLConfig.default,
    structuralLimits: RemoteGraphQLSource.StructuralLimits = RemoteGraphQLSource.StructuralLimits.default
  ): RemoteGraphQLSource[R] =
    new RemoteGraphQLSource(
      "remote",
      endpoint,
      backend,
      config,
      structuralLimits,
      None,
      None,
      GatewayWrapper.empty
    )
}
