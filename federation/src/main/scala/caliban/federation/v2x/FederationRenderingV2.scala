package caliban.federation.v2x

import caliban.GraphQL
import caliban.rendering.{ DocumentRenderer, Renderer }

trait FederationRenderingV2 { self: FederationV2 =>

  /**
   * Constructs a renderer that can render a GraphQL schema with federation directives.
   *
   * This is useful if you need to render the schema to a string or file in CI or for debugging purposes.
   *
   * @note Make sure to use this renderer on the graph _before_ apply the federation aspect, otherwise it will include
   *       the federation specific fields as well.
   */
  def renderFederated[R](graphql: GraphQL[R]): String =
    renderer[R].render(graphql)

  def renderFederatedCompact[R](graphql: GraphQL[R]): String =
    renderer[R].renderCompact(graphql)

  private def renderer[R]: Renderer[GraphQL[R]] =
    DocumentRenderer
      .contramap[GraphQL[_]] {
        _.withSchemaDirectives(extensions.map(_.toDirective)).toDocument
      }

}
