package caliban.federation.v2x

import caliban.GraphQL
import caliban.federation.{ FederationDirectives, FederationSupport }
import caliban.rendering.{ DocumentRenderer, Renderer }

class FederationV2(extensions: List[Extension])
    extends FederationSupport(Nil, extensions.map(_.toDirective))
    with FederationDirectives
    with FederationDirectivesV2 {

  /**
   * Extends a GraphQL schema with federation directives. Useful in specific cases, for instance if you
   * want to render the schema with federation directives for schema check or debugging purposes
   *
   * @note This method should be used on the "base" api, that is, before @@ federated is used to avoid including the
   *       gateway specific additions.
   */
  def extend[R](graphql: GraphQL[R]): GraphQL[R] =
    graphql.withSchemaDirectives(extensions.map(_.toDirective))

  lazy val federationRenderer: Renderer[GraphQL[Any]] =
    DocumentRenderer.contramap(extend[Any](_).toDocument)
}

object FederationV2 {

  val federationV2Url = "https://specs.apollo.dev/federation"
  val connectUrl      = "https://specs.apollo.dev/connect"

  def DefaultDirectives: List[Import] = List(
    Import("@key"),
    Import("@requires"),
    Import("@provides"),
    Import("@external"),
    Import("@shareable"),
    Import("@tag"),
    Import("@inaccessible"),
    Import("@override"),
    Import("@extends")
  )

  private[v2x] val v2_0 = Link(
    url = s"$federationV2Url/v2.0",
    `import` = DefaultDirectives
  )

  private[v2x] val v2_1 = Link(
    url = s"$federationV2Url/v2.1",
    `import` = v2_0.`import` :+ Import("@composeDirective")
  )

  private[v2x] val v2_3 = Link(
    url = s"$federationV2Url/v2.3",
    `import` = v2_1.`import` :+ Import("@interfaceObject")
  )

  private[v2x] val v2_4 = Link(
    url = s"$federationV2Url/v2.4",
    `import` = v2_3.`import`
  )
  private[v2x] val v2_5 = Link(
    url = s"$federationV2Url/v2.5",
    `import` = v2_4.`import` :+ Import("@authenticated") :+ Import("@requiresScopes")
  )

  private[v2x] val v2_6 = Link(
    url = s"$federationV2Url/v2.6",
    `import` = v2_5.`import` :+ Import("@policy")
  )

  private[v2x] val v2_7 = Link(
    url = s"$federationV2Url/v2.7",
    `import` = v2_6.`import`
  )

  private[v2x] val v2_8 = Link(
    url = s"$federationV2Url/v2.8",
    `import` = v2_7.`import` :+ Import("@context") :+ Import("@fromContext")
  )

  private[v2x] val v2_9 = Link(
    url = s"$federationV2Url/v2.9",
    `import` = v2_8.`import` :+ Import("@cost") :+ Import("@listSize")
  )

  private[v2x] val v2_10 = Link(
    url = s"$federationV2Url/v2.10",
    `import` = v2_9.`import`
  )

  val connect: Link = Link(
    url = s"$connectUrl/v0.1",
    `import` = List(Import("@connect"), Import("@source"))
  )

}
