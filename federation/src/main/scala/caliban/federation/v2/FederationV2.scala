package caliban.federation.v2

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

  private[v2] val v2_0 = Link(
    url = s"$federationV2Url/v2.0",
    `import` = DefaultDirectives
  )

  private[v2] val v2_1 = Link(
    url = s"$federationV2Url/v2.1",
    `import` = v2_0.`import` :+ Import("@composeDirective")
  )

  private[v2] val v2_3 = Link(
    url = s"$federationV2Url/v2.3",
    `import` = v2_1.`import` :+ Import("@interfaceObject")
  )

}
