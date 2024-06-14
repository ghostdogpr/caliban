package caliban.federation.v2x

import caliban.federation.{ FederationDirectives, FederationSupport }

class FederationV2(extensions: List[Extension])
    extends FederationSupport(Nil, extensions.map(_.toDirective))
    with FederationDirectives
    with FederationDirectivesV2

object FederationV2 {

  val federationV2Url = "https://specs.apollo.dev/federation"

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

}
