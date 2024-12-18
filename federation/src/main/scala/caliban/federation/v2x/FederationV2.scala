package caliban.federation.v2x

import caliban.GraphQL
import caliban.federation.{ FederationDirectives, FederationSupport }
import caliban.rendering.{ DocumentRenderer, Renderer }

class FederationV2(extensions: List[Extension])
    extends FederationSupport(Nil, extensions.map(_.toDirective))
    with FederationDirectives
    with FederationDirectivesV2 {

  /**
   * Constructs a renderer that can render a GraphQL schema with federation directives.
   *
   * This is useful if you need to render the schema to a string or file in CI or for debugging purposes.
   *
   * @note Make sure to use this renderer on the graph _before_ apply the federation aspect, otherwise it will include
   *       the federation specific fields as well.
   */
  lazy val renderer: Renderer[GraphQL[_]] = DocumentRenderer.contramap[GraphQL[_]] {
    _.withSchemaDirectives(extensions.map(_.toDirective)).toDocument
  }
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
